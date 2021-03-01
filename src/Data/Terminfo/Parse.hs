{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -funbox-strict-fields -O #-}

module Data.Terminfo.Parse
  ( module Data.Terminfo.Parse
  , Text.Parsec.ParseError
  )
where

import Control.DeepSeq

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Word
import qualified Data.Vector.Unboxed as Vector

import Numeric (showHex)

import Text.Parsec

data CapExpression = CapExpression
    { capOps :: !CapOps
    , capBytes :: !(Vector.Vector Word8)
    , sourceString :: !String
    , paramCount :: !Int
    , paramOps :: !ParamOps
    } deriving (Eq)

instance Show CapExpression where
    show c
        = "CapExpression { " ++ show (capOps c) ++ " }"
        ++ " <- [" ++ hexDump ( map ( toEnum . fromEnum ) $! sourceString c ) ++ "]"
        ++ " <= " ++ show (sourceString c)
        where
            hexDump :: [Word8] -> String
            hexDump = foldr showHex ""

instance NFData CapExpression where
    rnf (CapExpression ops !_bytes !str !c !pOps)
        = rnf ops `seq` rnf str `seq` rnf c `seq` rnf pOps

type CapParam = Word

type CapOps = [CapOp]
data CapOp =
      Bytes !Int !Int -- offset count
    | DecOut | CharOut
    -- This stores a 0-based index to the parameter. However the
    -- operation that implies this op is 1-based
    | PushParam !Word | PushValue !Word
    -- The conditional parts are the sequence of (%t expression, %e
    -- The expression) pairs. %e expression may be NOP
    | Conditional
      { conditionalExpr :: !CapOps
      , conditionalParts :: ![(CapOps, CapOps)]
      }
    | BitwiseOr | BitwiseXOr | BitwiseAnd
    | ArithPlus | ArithMinus
    | CompareEq | CompareLt | CompareGt
    deriving (Show, Eq)

instance NFData CapOp where
    rnf (Bytes offset byteCount ) = rnf offset `seq` rnf byteCount
    rnf (PushParam pn) = rnf pn
    rnf (PushValue v) = rnf v
    rnf (Conditional cExpr cParts) = rnf cExpr `seq` rnf cParts
    rnf BitwiseOr = ()
    rnf BitwiseXOr = ()
    rnf BitwiseAnd = ()
    rnf ArithPlus = ()
    rnf ArithMinus = ()
    rnf CompareEq = ()
    rnf CompareLt = ()
    rnf CompareGt = ()
    rnf DecOut = ()
    rnf CharOut = ()

type ParamOps = [ParamOp]
data ParamOp =
      IncFirstTwo
    deriving (Show, Eq)

instance NFData ParamOp where
    rnf IncFirstTwo = ()

parseCapExpression :: String -> Either ParseError CapExpression
parseCapExpression capString =
    let v = runParser capExpressionParser
                      initialBuildState
                      "terminfo cap"
                      capString
    in case v of
        Left e -> Left e
        Right buildResults -> Right $ constructCapExpression capString buildResults

constructCapExpression :: String -> BuildResults -> CapExpression
constructCapExpression capString buildResults =
    let expr = CapExpression
                { capOps = outCapOps buildResults
                -- The cap bytes are the lower 8 bits of the input
                -- string's characters.
                , capBytes = Vector.fromList $ map (toEnum.fromEnum) capString
                , sourceString = capString
                , paramCount = outParamCount buildResults
                , paramOps = outParamOps buildResults
                }
    in rnf expr `seq` expr

type CapParser a = Parsec String BuildState a

capExpressionParser :: CapParser BuildResults
capExpressionParser = do
    rs <- many $ paramEscapeParser <|> bytesOpParser
    return $ mconcat rs

paramEscapeParser :: CapParser BuildResults
paramEscapeParser = do
    _ <- char '%'
    incOffset 1
    literalPercentParser <|> paramOpParser

literalPercentParser :: CapParser BuildResults
literalPercentParser = do
    _ <- char '%'
    startOffset <- nextOffset <$> getState
    incOffset 1
    return $ BuildResults 0 [Bytes startOffset 1] []

paramOpParser :: CapParser BuildResults
paramOpParser
    = incrementOpParser
    <|> pushOpParser
    <|> decOutParser
    <|> charOutParser
    <|> conditionalOpParser
    <|> bitwiseOpParser
    <|> arithOpParser
    <|> literalIntOpParser
    <|> compareOpParser
    <|> charConstParser

incrementOpParser :: CapParser BuildResults
incrementOpParser = do
    _ <- char 'i'
    incOffset 1
    return $ BuildResults 0 [] [ IncFirstTwo ]

pushOpParser :: CapParser BuildResults
pushOpParser = do
    _ <- char 'p'
    paramN <- read . pure <$> digit
    incOffset 2
    return $ BuildResults (fromEnum paramN) [PushParam $ paramN - 1] []

decOutParser :: CapParser BuildResults
decOutParser = do
    _ <- char 'd'
    incOffset 1
    return $ BuildResults 0 [ DecOut ] []

charOutParser :: CapParser BuildResults
charOutParser = do
    _ <- char 'c'
    incOffset 1
    return $ BuildResults 0 [ CharOut ] []

conditionalOpParser :: CapParser BuildResults
conditionalOpParser = do
    _ <- char '?'
    incOffset 1
    condPart <- manyExpr conditionalTrueParser
    parts <- manyP
             ( do
                truePart <- manyExpr $ choice [ try $ lookAhead conditionalEndParser
                                              , conditionalFalseParser
                                              ]
                falsePart <- manyExpr $ choice [ try $ lookAhead conditionalEndParser
                                               , conditionalTrueParser
                                               ]
                return ( truePart, falsePart )
             )
             conditionalEndParser

    let trueParts = map fst parts
        falseParts = map snd parts
        BuildResults n cond condParamOps = condPart

    let n' = maximum $ n : map outParamCount trueParts
        n'' = maximum $ n' : map outParamCount falseParts

    let trueOps = map outCapOps trueParts
        falseOps = map outCapOps falseParts
        condParts = zip trueOps falseOps

    let trueParamOps = mconcat $ map outParamOps trueParts
        falseParamOps = mconcat $ map outParamOps falseParts
        pOps = mconcat [condParamOps, trueParamOps, falseParamOps]

    return $ BuildResults n'' [ Conditional cond condParts ] pOps

    where
        manyP !p !end = choice
            [ try end >> return []
            , do !v <- p
                 !vs <- manyP p end
                 return $! v : vs
            ]
        manyExpr end = fmap mconcat $ manyP ( paramEscapeParser <|> bytesOpParser ) end

conditionalTrueParser :: CapParser ()
conditionalTrueParser = do
    _ <- string "%t"
    incOffset 2

conditionalFalseParser :: CapParser ()
conditionalFalseParser = do
    _ <- string "%e"
    incOffset 2

conditionalEndParser :: CapParser ()
conditionalEndParser = do
    _ <- string "%;"
    incOffset 2

bitwiseOpParser :: CapParser BuildResults
bitwiseOpParser
    =   bitwiseOrParser
    <|> bitwiseAndParser
    <|> bitwiseXorParser

bitwiseOrParser :: CapParser BuildResults
bitwiseOrParser = do
    _ <- char '|'
    incOffset 1
    return $ BuildResults 0 [ BitwiseOr ] [ ]

bitwiseAndParser :: CapParser BuildResults
bitwiseAndParser = do
    _ <- char '&'
    incOffset 1
    return $ BuildResults 0 [ BitwiseAnd ] [ ]

bitwiseXorParser :: CapParser BuildResults
bitwiseXorParser = do
    _ <- char '^'
    incOffset 1
    return $ BuildResults 0 [ BitwiseXOr ] [ ]

arithOpParser :: CapParser BuildResults
arithOpParser
    =   plusOp
    <|> minusOp
    where
        plusOp = do
            _ <- char '+'
            incOffset 1
            return $ BuildResults 0 [ ArithPlus ] [ ]
        minusOp = do
            _ <- char '-'
            incOffset 1
            return $ BuildResults 0 [ ArithMinus ] [ ]

literalIntOpParser :: CapParser BuildResults
literalIntOpParser = do
    _ <- char '{'
    incOffset 1
    nStr <- many1 digit
    incOffset $ toEnum $ length nStr
    let n :: Word = read nStr
    _ <- char '}'
    incOffset 1
    return $ BuildResults 0 [ PushValue n ] [ ]

compareOpParser :: CapParser BuildResults
compareOpParser
    =   compareEqOp
    <|> compareLtOp
    <|> compareGtOp
    where
        compareEqOp = do
            _ <- char '='
            incOffset 1
            return $ BuildResults 0 [ CompareEq ] [ ]
        compareLtOp = do
            _ <- char '<'
            incOffset 1
            return $ BuildResults 0 [ CompareLt ] [ ]
        compareGtOp = do
            _ <- char '>'
            incOffset 1
            return $ BuildResults 0 [ CompareGt ] [ ]

bytesOpParser :: CapParser BuildResults
bytesOpParser = do
    bytes <- many1 $ satisfy (/= '%')
    startOffset <- nextOffset <$> getState
    let !c = length bytes
    !s <- getState
    let s' = s { nextOffset = startOffset + c }
    setState s'
    return $ BuildResults 0 [Bytes startOffset c] []

charConstParser :: CapParser BuildResults
charConstParser = do
    _ <- char '\''
    charValue <- toEnum . fromEnum <$> anyChar
    _ <- char '\''
    incOffset 3
    return $ BuildResults 0 [ PushValue charValue ] [ ]

data BuildState = BuildState
    { nextOffset :: Int
    }

incOffset :: Int -> CapParser ()
incOffset n = do
    s <- getState
    let s' = s { nextOffset = nextOffset s + n }
    setState s'

initialBuildState :: BuildState
initialBuildState = BuildState 0

data BuildResults = BuildResults
    { outParamCount :: !Int
    , outCapOps :: !CapOps
    , outParamOps :: !ParamOps
    }

instance Semigroup BuildResults where
    v0 <> v1
        = BuildResults
        { outParamCount = outParamCount v0 `max` outParamCount v1
        , outCapOps = outCapOps v0 <> outCapOps v1
        , outParamOps = outParamOps v0 <> outParamOps v1
        }

instance Monoid BuildResults where
    mempty = BuildResults 0 [] []
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif
