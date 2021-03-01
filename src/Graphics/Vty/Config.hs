{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Vty supports a configuration file format and associated 'Config'
-- data type. The 'Config' can be provided to 'mkVty' to customize the
-- application's use of Vty.
--
-- Lines in config files that fail to parse are ignored. Later entries
-- take precedence over earlier ones.
--
-- = Debug
--
-- == @debugLog@
--
-- Format:
--
-- @
--  \"debugLog\" string
-- @
--
-- The value of the environment variable @VTY_DEBUG_LOG@ is equivalent
-- to a debugLog entry at the end of the last config file.
--
-- = Input Processing
--
-- == @map@
--
-- Format:
--
-- @
--  \"map\" term string key modifier_list
--  where
--      key := KEsc | KChar Char | KBS ... (same as 'Key')
--      modifier_list := \"[\" modifier+ \"]\"
--      modifier := MShift | MCtrl | MMeta | MAlt
--      term := "_" | string
-- @
--
-- E.g., if the contents are
--
-- @
--  map _       \"\\ESC[B\"    KUp   []
--  map _       \"\\ESC[1;3B\" KDown [MAlt]
--  map \"xterm\" \"\\ESC[D\"    KLeft []
-- @
--
-- Then the bytes @\"\\ESC[B\"@ will result in the KUp event on all
-- terminals. The bytes @\"\\ESC[1;3B\"@ will result in the event KDown
-- with the MAlt modifier on all terminals. The bytes @\"\\ESC[D\"@ will
-- result in the KLeft event when @TERM@ is @xterm@.
--
-- If a debug log is requested then vty will output the current input
-- table to the log in the above format. A workflow for using this is
-- to set @VTY_DEBUG_LOG@. Run the application. Check the debug log for
-- incorrect mappings. Add corrected mappings to @$HOME/.vty/config@.
--
-- = Unicode Character Width Maps
--
-- == @widthMap@
--
-- Format:
--
-- @
--  \"widthMap\" string string
-- @
--
-- E.g.,
--
-- @
--   widthMap \"xterm\" \"\/home\/user\/.vty\/xterm\_map.dat\"
-- @
--
-- This directive specifies the path to a Unicode character width
-- map (the second argument) that should be loaded and used when
-- the value of TERM matches the first argument. Unicode character
-- width maps can be produced either by running the provided binary
-- @vty-build-width-table@ or by calling the library routine
-- 'Graphics.Vty.UnicodeWidthTable.Query.buildUnicodeWidthTable'. The
-- 'Graphics.Vty.mkVty' function will use these configuration settings
-- to attempt to load and install the specified width map. See the
-- documentation for 'Graphics.Vty.mkVty' for details.
module Graphics.Vty.Config
  ( InputMap
  , Config(..)
  , VtyConfigurationError(..)
  , userConfig
  , overrideEnvConfig
  , standardIOConfig
  , runParseConfig
  , parseConfigFile
  , defaultConfig
  , getTtyEraseChar
  , currentTerminalName

  , vtyConfigPath
  , widthTableFilename
  , vtyDataDirectory
  , terminalWidthTablePath
  , vtyConfigFileEnvName

  , ConfigUpdateResult(..)
  , addConfigWidthMap
  )
where

import Prelude

import Control.Applicative hiding (many)

import Control.Exception (catch, IOException, Exception(..), throwIO)
import Control.Monad (guard, void)

import qualified Data.ByteString as BS
#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (Monoid(..))
#endif
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Typeable (Typeable)

import Graphics.Vty.Input.Events

import GHC.Generics

import System.Directory ( getAppUserDataDirectory, doesFileExist
                        , createDirectoryIfMissing
                        )
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)
import System.Posix.IO (stdInput, stdOutput)
import System.Posix.Types (Fd(..))
import Foreign.C.Types (CInt(..), CChar(..))

import Text.Parsec hiding ((<|>))
import Text.Parsec.Token ( GenLanguageDef(..) )
import qualified Text.Parsec.Token as P

-- | Type of errors that can be thrown when configuring VTY
data VtyConfigurationError =
    VtyMissingTermEnvVar
    -- ^ TERM environment variable not set
    deriving (Show, Eq, Typeable)

instance Exception VtyConfigurationError where
    displayException VtyMissingTermEnvVar = "TERM environment variable not set"

-- | Mappings from input bytes to event in the order specified. Later
-- entries take precedence over earlier in the case multiple entries
-- have the same byte string.
type InputMap = [(Maybe String, String, Event)]

-- | A Vty configuration.
data Config =
    Config { vmin  :: Maybe Int
           -- ^ The default is 1 character.
           , vtime :: Maybe Int
           -- ^ The default is 100 milliseconds, 0.1 seconds.
           , mouseMode :: Maybe Bool
           -- ^ The default is False.
           , bracketedPasteMode :: Maybe Bool
           -- ^ The default is False.
           , debugLog :: Maybe FilePath
           -- ^ Debug information is appended to this file if not
           -- Nothing.
           , inputMap :: InputMap
           -- ^ The (input byte, output event) pairs extend the internal
           -- input table of VTY and the table from terminfo.
           --
           -- See "Graphics.Vty.Config" module documentation for
           -- documentation of the @map@ directive.
           , inputFd :: Maybe Fd
           -- ^ The input file descriptor to use. The default is
           -- 'System.Posix.IO.stdInput'
           , outputFd :: Maybe Fd
           -- ^ The output file descriptor to use. The default is
           -- 'System.Posix.IO.stdOutput'
           , termName :: Maybe String
           -- ^ The terminal name used to look up terminfo capabilities.
           -- The default is the value of the TERM environment variable.
           , termWidthMaps :: [(String, FilePath)]
           -- ^ Terminal width map files.
           , allowCustomUnicodeWidthTables :: Maybe Bool
           -- ^ Whether to permit custom Unicode width table loading by
           -- 'Graphics.Vty.mkVty'. @'Just' 'False'@ indicates that
           -- table loading should not be performed. Other values permit
           -- table loading.
           --
           -- If a table load is attempted and fails, information
           -- about the failure will be logged to the debug log if the
           -- configuration specifies one. If no custom table is loaded
           -- (or if a load fails), the built-in character width table
           -- will be used.
           }
           deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = mempty

instance Semigroup Config where
    c0 <> c1 =
        -- latter config takes priority for everything but inputMap
        Config { vmin = vmin c1 <|> vmin c0
               , vtime = vtime c1 <|> vtime c0
               , mouseMode = mouseMode c1
               , bracketedPasteMode = bracketedPasteMode c1
               , debugLog = debugLog c1 <|> debugLog c0
               , inputMap = inputMap c0 <> inputMap c1
               , inputFd = inputFd c1 <|> inputFd c0
               , outputFd = outputFd c1 <|> outputFd c0
               , termName = termName c1 <|> termName c0
               , termWidthMaps = termWidthMaps c1 <|> termWidthMaps c0
               , allowCustomUnicodeWidthTables =
                   allowCustomUnicodeWidthTables c1 <|> allowCustomUnicodeWidthTables c0
               }

instance Monoid Config where
    mempty =
        Config { vmin = Nothing
               , vtime = Nothing
               , mouseMode = Nothing
               , bracketedPasteMode = Nothing
               , debugLog = mempty
               , inputMap = mempty
               , inputFd = Nothing
               , outputFd = Nothing
               , termName = Nothing
               , termWidthMaps = []
               , allowCustomUnicodeWidthTables = Nothing
               }
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

vtyDataDirectory :: IO FilePath
vtyDataDirectory = getAppUserDataDirectory "vty"

vtyConfigPath :: IO FilePath
vtyConfigPath = do
    dir <- vtyDataDirectory
    return $ dir </> "config"

vtyConfigFileEnvName :: String
vtyConfigFileEnvName = "VTY_CONFIG_FILE"

-- | Load a configuration from 'vtyConfigPath' and @$VTY_CONFIG_FILE@.
userConfig :: IO Config
userConfig = do
    configFile <- vtyConfigPath >>= parseConfigFile
    overrideConfig <- maybe (return defaultConfig) parseConfigFile =<<
        lookupEnv vtyConfigFileEnvName
    let base = configFile <> overrideConfig
    mappend base <$> overrideEnvConfig

widthTableFilename :: String -> String
widthTableFilename term = "width_table_" <> term <> ".dat"

termVariable :: String
termVariable = "TERM"

currentTerminalName :: IO (Maybe String)
currentTerminalName = lookupEnv termVariable

terminalWidthTablePath :: IO (Maybe FilePath)
terminalWidthTablePath = do
    dataDir <- vtyDataDirectory
    result <- lookupEnv termVariable
    case result of
        Nothing -> return Nothing
        Just term -> do
            return $ Just $ dataDir </> widthTableFilename term

overrideEnvConfig :: IO Config
overrideEnvConfig = do
    d <- lookupEnv "VTY_DEBUG_LOG"
    return $ defaultConfig { debugLog = d }

-- | Configures VTY using defaults suitable for terminals. This function
-- can raise 'VtyConfigurationError'.
standardIOConfig :: IO Config
standardIOConfig = do
    mb <- lookupEnv termVariable
    case mb of
      Nothing -> throwIO VtyMissingTermEnvVar
      Just t ->
        return defaultConfig
          { vmin               = Just 1
          , mouseMode          = Just False
          , bracketedPasteMode = Just False
          , vtime              = Just 100
          , inputFd            = Just stdInput
          , outputFd           = Just stdOutput
          , termName           = Just t
          }

parseConfigFile :: FilePath -> IO Config
parseConfigFile path = do
    catch (runParseConfig path <$> BS.readFile path)
          (\(_ :: IOException) -> return defaultConfig)

runParseConfig :: String -> BS.ByteString -> Config
runParseConfig name cfgTxt =
  case runParser parseConfig () name cfgTxt of
    Right cfg -> cfg
    Left{}    -> defaultConfig

------------------------------------------------------------------------

type Parser = Parsec BS.ByteString ()

configLanguage :: Monad m => P.GenLanguageDef BS.ByteString () m
configLanguage = LanguageDef
    { commentStart    = "{-"
    , commentEnd      = "-}"
    , commentLine     = "--"
    , nestedComments  = True
    , identStart      = letter <|> char '_'
    , identLetter     = alphaNum <|> oneOf "_'"
    , opStart         = opLetter configLanguage
    , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedOpNames = []
    , reservedNames   = []
    , caseSensitive   = True
    }

configLexer :: Monad m => P.GenTokenParser BS.ByteString () m
configLexer = P.makeTokenParser configLanguage

mapDecl :: Parser Config
mapDecl = do
    "map" <- P.identifier configLexer
    termIdent <- (char '_' >> P.whiteSpace configLexer >> return Nothing)
             <|> (Just <$> P.stringLiteral configLexer)
    bytes     <- P.stringLiteral configLexer
    key       <- parseValue
    modifiers <- parseValue
    return defaultConfig { inputMap = [(termIdent, bytes, EvKey key modifiers)] }

debugLogDecl :: Parser Config
debugLogDecl = do
    "debugLog" <- P.identifier configLexer
    path       <- P.stringLiteral configLexer
    return defaultConfig { debugLog = Just path }

widthMapDecl :: Parser Config
widthMapDecl = do
    "widthMap" <- P.identifier configLexer
    tName <- P.stringLiteral configLexer
    path <- P.stringLiteral configLexer
    return defaultConfig { termWidthMaps = [(tName, path)] }

ignoreLine :: Parser ()
ignoreLine = void $ manyTill anyChar newline

parseConfig :: Parser Config
parseConfig = fmap mconcat $ many $ do
    P.whiteSpace configLexer
    let directives = [try mapDecl, try debugLogDecl, try widthMapDecl]
    choice directives <|> (ignoreLine >> return defaultConfig)

class    Parse a        where parseValue :: Parser a
instance Parse Char     where parseValue = P.charLiteral configLexer
instance Parse Int      where parseValue = fromInteger <$> P.natural configLexer
instance Parse Key      where parseValue = genericParse
instance Parse Modifier where parseValue = genericParse
instance Parse a => Parse [a] where
  parseValue = P.brackets configLexer
                 (parseValue `sepBy` P.symbol configLexer ",")

------------------------------------------------------------------------
-- Derived parser for ADTs via generics
------------------------------------------------------------------------

genericParse :: (Generic a, GParse (Rep a)) => Parser a
genericParse = to <$> gparse

class    GParse f                      where gparse :: Parser (f a)
instance GParse f => GParse (M1 S i f) where gparse = M1 <$> gparse
instance GParse U1                     where gparse = return U1
instance Parse a => GParse (K1 i a)    where gparse = K1 <$> parseValue

instance (GParse f, GParse g) => GParse (f :*: g) where
  gparse = (:*:) <$> gparse <*> gparse

instance GParseAlts f => GParse (M1 D i f) where
  gparse =
    do con <- P.identifier configLexer
       M1 <$> gparseAlts con

------------------------------------------------------------------------

class GParseAlts f where
  gparseAlts :: String -> Parser (f a)

instance (Constructor i, GParse f) => GParseAlts (M1 C i f) where
  gparseAlts con =
    do guard (con == conName (M1 Nothing :: C1 i Maybe a))
       M1 <$> gparse

instance (GParseAlts f, GParseAlts g) => GParseAlts (f :+: g) where
  gparseAlts con = L1 <$> gparseAlts con <|> R1 <$> gparseAlts con

instance GParseAlts V1 where gparseAlts _ = fail "GParse: V1"

foreign import ccall "vty_get_tty_erase" cGetTtyErase :: Fd -> IO CChar

-- | Get the "erase" character for the terminal attached to the
-- specified file descriptor. This is the character configured by 'stty
-- erase'. If the call to 'tcgetattr' fails, this will return 'Nothing'.
-- Otherwise it will return the character that has been configured to
-- indicate the canonical mode ERASE behavior. That character can then
-- be added to the table of strings that we interpret to mean Backspace.
--
-- For more details, see:
--
-- * https://www.gnu.org/software/libc/manual/html_node/Canonical-or-Not.html
-- * https://www.gsp.com/cgi-bin/man.cgi?section=1&topic=stty
-- * https://github.com/matterhorn-chat/matterhorn/issues/565
getTtyEraseChar :: Fd -> IO (Maybe Char)
getTtyEraseChar fd = do
    c <- cGetTtyErase fd
    if c /= 0
       then return $ Just $ toEnum $ fromEnum c
       else return Nothing

data ConfigUpdateResult =
    ConfigurationCreated
    | ConfigurationModified
    | ConfigurationConflict String
    | ConfigurationRedundant
    deriving (Eq, Show)

-- | Add a @widthMap@ directive to the Vty configuration file at the
-- specified path.
--
-- If the configuration path refers to a configuration that already
-- contains the directive for the specified map and terminal type, the
-- configuration file will not be modified. If the file does not contain
-- the directive, it will be appended to the file.
--
-- If the configuration path does not exist, a new configuration file
-- will be created and any directories in the path will also be created.
--
-- This returns @True@ if the configuration was created or modified and
-- @False@ otherwise. This does not handle exceptions raised by file or
-- directory permissions issues.
addConfigWidthMap :: FilePath
                  -- ^ The configuration file path of the configuration
                  -- to modify or create.
                  -> String
                  -- ^ The @TERM@ value for the @widthMap@ directive.
                  -> FilePath
                  -- ^ The width table file path for the directive.
                  -> IO ConfigUpdateResult
addConfigWidthMap configPath term tablePath = do
    configEx <- doesFileExist configPath
    if configEx
       then updateConfig
       else createConfig >> return ConfigurationCreated

    where
        directive = "widthMap " <> show term <> " " <> show tablePath <> "\n"

        createConfig = do
            let dir = takeDirectory configPath
            createDirectoryIfMissing True dir
            writeFile configPath directive

        updateConfig = do
            config <- parseConfigFile configPath
            if (term, tablePath) `elem` termWidthMaps config
               then return ConfigurationRedundant
               else case lookup term (termWidthMaps config) of
                   Just other -> return $ ConfigurationConflict other
                   Nothing -> do
                       appendFile configPath directive
                       return ConfigurationModified
