{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -D_XOPEN_SOURCE=500 -fno-warn-warnings-deprecations #-}
{-# CFILES gwinsz.c #-}

-- | Terminfo-based terminal output driver.
--
-- Copyright Corey O'Connor (coreyoconnor@gmail.com)
module Graphics.Vty.Output.TerminfoBased
  ( reserveTerminal
  )
where

import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (toForeignPtr)
import Data.Terminfo.Parse
import Data.Terminfo.Eval

import Graphics.Vty.Attributes
import Graphics.Vty.Image (DisplayRegion)
import Graphics.Vty.DisplayAttributes
import Graphics.Vty.Output.Interface

import Blaze.ByteString.Builder (Write, writeToByteString, writeStorable, writeWord8)

import Data.Bits ((.&.))
import Data.IORef
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Word

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (foldMap)
#endif

import Foreign.C.Types ( CInt(..), CLong(..) )
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)

import qualified System.Console.Terminfo as Terminfo
import System.Posix.IO (fdWriteBuf)
import System.Posix.Types (Fd(..))

data TerminfoCaps = TerminfoCaps
    { smcup :: Maybe CapExpression
    , rmcup :: Maybe CapExpression
    , cup :: CapExpression
    , cnorm :: Maybe CapExpression
    , civis :: Maybe CapExpression
    , useAltColorMap :: Bool
    , setForeColor :: CapExpression
    , setBackColor :: CapExpression
    , setDefaultAttr :: CapExpression
    , clearScreen :: CapExpression
    , clearEol :: CapExpression
    , displayAttrCaps :: DisplayAttrCaps
    , ringBellAudio :: Maybe CapExpression
    }

data DisplayAttrCaps = DisplayAttrCaps
    { setAttrStates :: Maybe CapExpression
    , enterStandout :: Maybe CapExpression
    , exitStandout :: Maybe CapExpression
    , enterItalic :: Maybe CapExpression
    , exitItalic :: Maybe CapExpression
    , enterUnderline :: Maybe CapExpression
    , exitUnderline :: Maybe CapExpression
    , enterReverseVideo :: Maybe CapExpression
    , enterDimMode :: Maybe CapExpression
    , enterBoldMode :: Maybe CapExpression
    }

-- kinda like:
-- https://code.google.com/p/vim/source/browse/src/fileio.c#10422
-- fdWriteBuf will throw on error. Unless the error is EINTR. On EINTR
-- the write will be retried.
fdWriteAll :: Fd -> Ptr Word8 -> Int -> Int -> IO Int
fdWriteAll outFd ptr len count
    | len <  0  = fail "fdWriteAll: len is less than 0"
    | len == 0  = return count
    | otherwise = do
        writeCount <- fromEnum <$> fdWriteBuf outFd ptr (toEnum len)
        let len' = len - writeCount
            ptr' = ptr `plusPtr` writeCount
            count' = count + writeCount
        fdWriteAll outFd ptr' len' count'

sendCapToTerminal :: Output -> CapExpression -> [CapParam] -> IO ()
sendCapToTerminal t cap capParams = do
    outputByteBuffer t $ writeToByteString $ writeCapExpr cap capParams

-- | Constructs an output driver that uses terminfo for all control
-- codes. While this should provide the most compatible terminal,
-- terminfo does not support some features that would increase
-- efficiency and improve compatibility:
--
--  * determining the character encoding supported by the terminal.
--    Should this be taken from the LANG environment variable?
--
--  * Providing independent string capabilities for all display
--    attributes.
reserveTerminal :: String -> Fd -> ColorMode -> IO Output
reserveTerminal termName outFd colorMode = do
    ti <- Terminfo.setupTerm termName
    -- assumes set foreground always implies set background exists.
    -- if set foreground is not set then all color changing style
    -- attributes are filtered.
    msetaf <- probeCap ti "setaf"
    msetf <- probeCap ti "setf"
    let (useAlt, setForeCap)
            = case msetaf of
                Just setaf -> (False, setaf)
                Nothing -> case msetf of
                    Just setf -> (True, setf)
                    Nothing -> (True, error $ "no fore color support for terminal " ++ termName)
    msetab <- probeCap ti "setab"
    msetb <- probeCap ti "setb"
    let setBackCap
            = case msetab of
                Just setab -> setab
                Nothing -> case msetb of
                    Just setb -> setb
                    Nothing -> error $ "no back color support for terminal " ++ termName

    hyperlinkModeStatus <- newIORef False
    newAssumedStateRef <- newIORef initialAssumedState

    let terminfoSetMode m newStatus = do
          curStatus <- terminfoModeStatus m
          when (newStatus /= curStatus) $
              case m of
                  Hyperlink -> do
                      writeIORef hyperlinkModeStatus newStatus
                      writeIORef newAssumedStateRef initialAssumedState
                  _ -> return ()
        terminfoModeStatus m =
            case m of
                Hyperlink -> readIORef hyperlinkModeStatus
                _ -> return False
        terminfoModeSupported Hyperlink = True
        terminfoModeSupported _ = False

    terminfoCaps <- pure TerminfoCaps
        <*> probeCap ti "smcup"
        <*> probeCap ti "rmcup"
        <*> requireCap ti "cup"
        <*> probeCap ti "cnorm"
        <*> probeCap ti "civis"
        <*> pure useAlt
        <*> pure setForeCap
        <*> pure setBackCap
        <*> requireCap ti "sgr0"
        <*> requireCap ti "clear"
        <*> requireCap ti "el"
        <*> currentDisplayAttrCaps ti
        <*> probeCap ti "bel"
    let t = Output
            { terminalID = termName
            , releaseTerminal = do
                sendCap setDefaultAttr []
                maybeSendCap cnorm []
            , supportsBell = return $ isJust $ ringBellAudio terminfoCaps
            , ringTerminalBell = maybeSendCap ringBellAudio []
            , reserveDisplay = do
                -- If there is no support for smcup: Clear the screen
                -- and then move the mouse to the home position to
                -- approximate the behavior.
                maybeSendCap smcup []
                sendCap clearScreen []
            , releaseDisplay = do
                maybeSendCap rmcup []
                maybeSendCap cnorm []
            , displayBounds = do
                rawSize <- getWindowSize outFd
                case rawSize of
                    (w, h)  | w < 0 || h < 0 -> fail $ "getwinsize returned < 0 : " ++ show rawSize
                            | otherwise      -> return (w,h)
            , outputByteBuffer = \outBytes -> do
                let (fptr, offset, len) = toForeignPtr outBytes
                actualLen <- withForeignPtr fptr
                             $ \ptr -> fdWriteAll outFd (ptr `plusPtr` offset) len 0
                when (toEnum len /= actualLen) $ fail $ "Graphics.Vty.Output: outputByteBuffer "
                  ++ "length mismatch. " ++ show len ++ " /= " ++ show actualLen
                  ++ " Please report this bug to vty project."
            , supportsCursorVisibility = isJust $ civis terminfoCaps
            , supportsMode = terminfoModeSupported
            , setMode = terminfoSetMode
            , getModeStatus = terminfoModeStatus
            , assumedStateRef = newAssumedStateRef
            , outputColorMode = colorMode
            -- I think fix would help assure tActual is the only
            -- reference. I was having issues tho.
            , mkDisplayContext = \tActual -> terminfoDisplayContext tActual terminfoCaps
            }
        sendCap s = sendCapToTerminal t (s terminfoCaps)
        maybeSendCap s = when (isJust $ s terminfoCaps) . sendCap (fromJust . s)
    return t

requireCap :: Terminfo.Terminal -> String -> IO CapExpression
requireCap ti capName
    = case Terminfo.getCapability ti (Terminfo.tiGetStr capName) of
        Nothing     -> fail $ "Terminal does not define required capability \"" ++ capName ++ "\""
        Just capStr -> parseCap capStr

probeCap :: Terminfo.Terminal -> String -> IO (Maybe CapExpression)
probeCap ti capName
    = case Terminfo.getCapability ti (Terminfo.tiGetStr capName) of
        Nothing     -> return Nothing
        Just capStr -> Just <$> parseCap capStr

parseCap :: String -> IO CapExpression
parseCap capStr = do
    case parseCapExpression capStr of
        Left e -> fail $ show e
        Right cap -> return cap

currentDisplayAttrCaps :: Terminfo.Terminal -> IO DisplayAttrCaps
currentDisplayAttrCaps ti
    =   pure DisplayAttrCaps
    <*> probeCap ti "sgr"
    <*> probeCap ti "smso"
    <*> probeCap ti "rmso"
    <*> probeCap ti "sitm"
    <*> probeCap ti "ritm"
    <*> probeCap ti "smul"
    <*> probeCap ti "rmul"
    <*> probeCap ti "rev"
    <*> probeCap ti "dim"
    <*> probeCap ti "bold"

foreign import ccall "gwinsz.h vty_c_get_window_size" c_getWindowSize :: Fd -> IO CLong

getWindowSize :: Fd -> IO (Int,Int)
getWindowSize fd = do
    (a,b) <- (`divMod` 65536) `fmap` c_getWindowSize fd
    return (fromIntegral b, fromIntegral a)

terminfoDisplayContext :: Output -> TerminfoCaps -> DisplayRegion -> IO DisplayContext
terminfoDisplayContext tActual terminfoCaps r = return dc
    where dc = DisplayContext
            { contextDevice = tActual
            , contextRegion = r
            , writeMoveCursor = \x y -> writeCapExpr (cup terminfoCaps) [toEnum y, toEnum x]
            , writeShowCursor = case cnorm terminfoCaps of
                Nothing -> error "this terminal does not support show cursor"
                Just c -> writeCapExpr c []
            , writeHideCursor = case civis terminfoCaps of
                Nothing -> error "this terminal does not support hide cursor"
                Just c -> writeCapExpr c []
            , writeSetAttr = terminfoWriteSetAttr dc terminfoCaps
            , writeDefaultAttr = \urlsEnabled ->
                writeCapExpr (setDefaultAttr terminfoCaps) [] `mappend`
                (if urlsEnabled then writeURLEscapes EndLink else mempty)
            , writeRowEnd = writeCapExpr (clearEol terminfoCaps) []
            , inlineHack = return ()
            }

-- | Write the escape sequences that are used in some terminals to
-- include embedded hyperlinks. As of yet, this information isn't
-- included in termcap or terminfo, so this writes them directly
-- instead of looking up the appropriate capabilities.
writeURLEscapes :: URLDiff -> Write
writeURLEscapes (LinkTo url) =
    foldMap writeStorable (BS.unpack "\x1b]8;;") `mappend`
    foldMap writeStorable (BS.unpack url) `mappend`
    writeStorable (0x07 :: Word8)
writeURLEscapes EndLink =
    foldMap writeStorable (BS.unpack "\x1b]8;;\a")
writeURLEscapes NoLinkChange =
    mempty

-- | Portably setting the display attributes is a giant pain in the ass.
--
-- If the terminal supports the sgr capability (which sets the on/off
-- state of each style directly; and, for no good reason, resets the
-- colors to the default) this procedure is used:
--
--  0. set the style attributes. This resets the fore and back color.
--
--  1, If a foreground color is to be set then set the foreground color
--
--  2. likewise with the background color
--
-- If the terminal does not support the sgr cap then: if there is a
-- change from an applied color to the default (in either the fore or
-- back color) then:
--
--  0. reset all display attributes (sgr0)
--
--  1. enter required style modes
--
--  2. set the fore color if required
--
--  3. set the back color if required
--
-- Entering the required style modes could require a reset of the
-- display attributes. If this is the case then the back and fore colors
-- always need to be set if not default.
--
-- This equation implements the above logic.
--
-- Note that this assumes the removal of color changes in the
-- display attributes is done as expected with noColors == True. See
-- `limitAttrForDisplay`.
--
-- Note that this optimizes for fewer state changes followed by fewer
-- bytes.
terminfoWriteSetAttr :: DisplayContext -> TerminfoCaps -> Bool -> FixedAttr -> Attr -> DisplayAttrDiff -> Write
terminfoWriteSetAttr dc terminfoCaps urlsEnabled prevAttr reqAttr diffs =
    urlAttrs urlsEnabled `mappend` case (foreColorDiff diffs == ColorToDefault) || (backColorDiff diffs == ColorToDefault) of
        -- The only way to reset either color, portably, to the default
        -- is to use either the set state capability or the set default
        -- capability.
        True -> do
            case reqDisplayCapSeqFor (displayAttrCaps terminfoCaps)
                                     (fixedStyle attr)
                                     (styleToApplySeq $ fixedStyle attr) of
                -- only way to reset a color to the defaults
                EnterExitSeq caps -> writeDefaultAttr dc urlsEnabled
                                     `mappend`
                                     foldMap (\cap -> writeCapExpr cap []) caps
                                     `mappend`
                                     setColors
                -- implicitly resets the colors to the defaults
                SetState state -> writeCapExpr (fromJust $ setAttrStates
                                                         $ displayAttrCaps
                                                         $ terminfoCaps
                                               )
                                               (sgrArgsForState state)
                                  `mappend` setItalics
                                  `mappend` setColors
        -- Otherwise the display colors are not changing or changing
        -- between two non-default points.
        False -> do
            -- Still, it could be the case that the change in display
            -- attributes requires the colors to be reset because the
            -- required capability was not available.
            case reqDisplayCapSeqFor (displayAttrCaps terminfoCaps)
                                     (fixedStyle attr)
                                     (styleDiffs diffs) of
                -- Really, if terminals were re-implemented with modern
                -- concepts instead of bowing down to 40 yr old dumb
                -- terminal requirements this would be the only case
                -- ever reached! Changes the style and color states
                -- according to the differences with the currently
                -- applied states.
                EnterExitSeq caps -> foldMap (\cap -> writeCapExpr cap []) caps
                                     `mappend`
                                     writeColorDiff Foreground (foreColorDiff diffs)
                                     `mappend`
                                     writeColorDiff Background (backColorDiff diffs)
                -- implicitly resets the colors to the defaults
                SetState state -> writeCapExpr (fromJust $ setAttrStates
                                                         $ displayAttrCaps terminfoCaps
                                               )
                                               (sgrArgsForState state)
                                  `mappend` setItalics
                                  `mappend` setColors
    where
        urlAttrs True = writeURLEscapes (urlDiff diffs)
        urlAttrs False = mempty
        colorMap = case useAltColorMap terminfoCaps of
                        False -> ansiColorIndex
                        True -> altColorIndex
        attr = fixDisplayAttr prevAttr reqAttr

        -- italics can't be set via SGR, so here we manually
        -- apply the enter and exit sequences as needed after
        -- changing the SGR
        setItalics
          | hasStyle (fixedStyle attr) italic
          , Just sitm <- enterItalic (displayAttrCaps terminfoCaps)
          = writeCapExpr sitm []
          | otherwise = mempty
        setColors =
            (case fixedForeColor attr of
                Just c -> writeColor Foreground c
                Nothing -> mempty)
            `mappend`
            (case fixedBackColor attr of
                Just c -> writeColor Background c
                Nothing -> mempty)
        writeColorDiff _side NoColorChange
            = mempty
        writeColorDiff _side ColorToDefault
            = error "ColorToDefault is not a possible case for applyColorDiffs"
        writeColorDiff side (SetColor c)
            = writeColor side c

        writeColor side (RGBColor r g b) =
            case outputColorMode (contextDevice dc) of
                FullColor ->
                    hardcodeColor side (r, g, b)
                _ ->
                    error "clampColor should remove rgb colors in standard mode"
        writeColor side c =
            writeCapExpr (setSideColor side terminfoCaps) [toEnum $ colorMap c]

-- a color can either be in the foreground or the background
data ColorSide = Foreground | Background

-- get the capability for drawing a color on a specific side
setSideColor :: ColorSide -> TerminfoCaps -> CapExpression
setSideColor Foreground = setForeColor
setSideColor Background = setBackColor

hardcodeColor :: ColorSide -> (Word8, Word8, Word8) -> Write
hardcodeColor side (r, g, b) =
    -- hardcoded color codes are formatted as "\x1b[{side};2;{r};{g};{b}m"
    mconcat [ writeStr "\x1b[", sideCode, delimiter, writeChar '2', delimiter
            , writeColor r, delimiter, writeColor g, delimiter, writeColor b
            , writeChar 'm']
    where
        writeChar = writeWord8 . fromIntegral . fromEnum
        writeStr = mconcat . map writeChar
        writeColor = writeStr . show
        delimiter = writeChar ';'
        -- 38/48 are used to set whether we should write to the
        -- foreground/background. I really don't want to know why.
        sideCode = case side of
            Foreground -> writeStr "38"
            Background -> writeStr "48"

-- | The color table used by a terminal is a 16 color set followed by a
-- 240 color set that might not be supported by the terminal.
--
-- This takes a Color which clearly identifies which pallete to use and
-- computes the index into the full 256 color pallete.
ansiColorIndex :: Color -> Int
ansiColorIndex (ISOColor v) = fromEnum v
ansiColorIndex (Color240 v) = 16 + fromEnum v
ansiColorIndex (RGBColor _ _ _) =
    error $ unlines [ "Attempted to create color index from rgb color."
                    , "This is currently unsupported, and shouldn't ever happen"
                    ]

-- | For terminals without setaf/setab
--
-- See table in `man terminfo`
-- Will error if not in table.
altColorIndex :: Color -> Int
altColorIndex (ISOColor 0) = 0
altColorIndex (ISOColor 1) = 4
altColorIndex (ISOColor 2) = 2
altColorIndex (ISOColor 3) = 6
altColorIndex (ISOColor 4) = 1
altColorIndex (ISOColor 5) = 5
altColorIndex (ISOColor 6) = 3
altColorIndex (ISOColor 7) = 7
altColorIndex (ISOColor v) = fromEnum v
altColorIndex (Color240 v) = 16 + fromEnum v
altColorIndex (RGBColor _ _ _) =
    error $ unlines [ "Attempted to create color index from rgb color."
                    , "This is currently unsupported, and shouldn't ever happen"
                    ]

{- | The sequence of terminfo caps to apply a given style are determined
 - according to these rules.
 -
 -  1. The assumption is that it's preferable to use the simpler
 -  enter/exit mode capabilities than the full set display attribute
 -  state capability.
 -
 -  2. If a mode is supposed to be removed but there is not an exit
 -  capability defined then the display attributes are reset to defaults
 -  then the display attribute state is set.
 -
 -  3. If a mode is supposed to be applied but there is not an enter
 -  capability defined then then display attribute state is set if
 -  possible. Otherwise the mode is not applied.
 -
 -  4. If the display attribute state is being set then just update the
 -  arguments to that for any apply/remove.
 -}
data DisplayAttrSeq
    = EnterExitSeq [CapExpression]
    | SetState DisplayAttrState

data DisplayAttrState = DisplayAttrState
    { applyStandout :: Bool
    , applyUnderline :: Bool
    , applyItalic :: Bool
    , applyReverseVideo :: Bool
    , applyBlink :: Bool
    , applyDim :: Bool
    , applyBold :: Bool
    }

sgrArgsForState :: DisplayAttrState -> [CapParam]
sgrArgsForState attrState = map (\b -> if b then 1 else 0)
    [ applyStandout attrState
    , applyUnderline attrState
    , applyReverseVideo attrState
    , applyBlink attrState
    , applyDim attrState
    , applyBold attrState
    , False -- invis
    , False -- protect
    , False -- alt char set
    ]

reqDisplayCapSeqFor :: DisplayAttrCaps -> Style -> [StyleStateChange] -> DisplayAttrSeq
reqDisplayCapSeqFor caps s diffs
    -- if the state transition implied by any diff cannot be supported
    -- with an enter/exit mode cap then either the state needs to be set
    -- or the attribute change ignored.
    = case (any noEnterExitCap diffs, isJust $ setAttrStates caps) of
        -- If all the diffs have an enter-exit cap then just use those
        ( False, _    ) -> EnterExitSeq $ map enterExitCap diffs
        -- If not all the diffs have an enter-exit cap and there is no
        -- set state cap then filter out all unsupported diffs and just
        -- apply the rest
        ( True, False ) -> EnterExitSeq $ map enterExitCap
                                        $ filter (not . noEnterExitCap) diffs
        -- if not all the diffs have an enter-exit can and there is a
        -- set state cap then just use the set state cap.
        ( True, True  ) -> SetState $ stateForStyle s
    where
        noEnterExitCap ApplyItalic = isNothing $ enterItalic caps
        noEnterExitCap RemoveItalic = isNothing $ exitItalic caps
        noEnterExitCap ApplyStandout = isNothing $ enterStandout caps
        noEnterExitCap RemoveStandout = isNothing $ exitStandout caps
        noEnterExitCap ApplyUnderline = isNothing $ enterUnderline caps
        noEnterExitCap RemoveUnderline = isNothing $ exitUnderline caps
        noEnterExitCap ApplyReverseVideo = isNothing $ enterReverseVideo caps
        noEnterExitCap RemoveReverseVideo = True
        noEnterExitCap ApplyBlink = True
        noEnterExitCap RemoveBlink = True
        noEnterExitCap ApplyDim = isNothing $ enterDimMode caps
        noEnterExitCap RemoveDim = True
        noEnterExitCap ApplyBold = isNothing $ enterBoldMode caps
        noEnterExitCap RemoveBold = True
        enterExitCap ApplyItalic = fromJust $ enterItalic caps
        enterExitCap RemoveItalic = fromJust $ exitItalic caps
        enterExitCap ApplyStandout = fromJust $ enterStandout caps
        enterExitCap RemoveStandout = fromJust $ exitStandout caps
        enterExitCap ApplyUnderline = fromJust $ enterUnderline caps
        enterExitCap RemoveUnderline = fromJust $ exitUnderline caps
        enterExitCap ApplyReverseVideo = fromJust $ enterReverseVideo caps
        enterExitCap ApplyDim = fromJust $ enterDimMode caps
        enterExitCap ApplyBold = fromJust $ enterBoldMode caps
        enterExitCap _ = error "enterExitCap applied to diff that was known not to have one."

stateForStyle :: Style -> DisplayAttrState
stateForStyle s = DisplayAttrState
    { applyStandout = isStyleSet standout
    , applyUnderline = isStyleSet underline
    , applyItalic = isStyleSet italic
    , applyReverseVideo = isStyleSet reverseVideo
    , applyBlink = isStyleSet blink
    , applyDim = isStyleSet dim
    , applyBold = isStyleSet bold
    }
    where isStyleSet = hasStyle s

styleToApplySeq :: Style -> [StyleStateChange]
styleToApplySeq s = concat
    [ applyIfRequired ApplyStandout standout
    , applyIfRequired ApplyUnderline underline
    , applyIfRequired ApplyItalic italic
    , applyIfRequired ApplyReverseVideo reverseVideo
    , applyIfRequired ApplyBlink blink
    , applyIfRequired ApplyDim dim
    , applyIfRequired ApplyBold bold
    ]
    where
        applyIfRequired op flag
            = if 0 == (flag .&. s)
                then []
                else [op]
