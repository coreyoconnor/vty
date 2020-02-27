{-# LANGUAGE RecordWildCards, NamedFieldPuns, CPP #-}
-- | This module provides functions for accessing the current terminal
-- or a specific terminal device.
--
-- See also:
--
-- 1. "Graphics.Vty.Output": This instantiates an abtract interface
-- to the terminal based on the @TERM@ and @COLORTERM@ environment
-- variables.
--
-- 2. "Graphics.Vty.Output.Interface": Defines the generic interface all
-- terminal modules need to implement.
--
-- 3. "Graphics.Vty.Output.TerminfoBased": Defines a terminal instance
-- that uses terminfo for all control strings. No attempt is made to
-- change the character set to UTF-8 for these terminals.
--
-- 4. "Graphics.Vty.Output.XTermColor": This module contains an
-- interface suitable for xterm-like terminals. These are the terminals
-- where @TERM@ begins with @xterm@. This does use terminfo for as many
-- control codes as possible.
module Graphics.Vty.Output
  ( outputForConfig
  , setCursorPos
  , hideCursor
  , showCursor
  )
where

import Control.Monad (when)

import Graphics.Vty.Config
import Graphics.Vty.Image (regionWidth, regionHeight)
import Graphics.Vty.Output.Interface
import Graphics.Vty.Output.XTermColor as XTermColor
import Graphics.Vty.Output.TerminfoBased as TerminfoBased

import Blaze.ByteString.Builder (writeToByteString)

import Data.List (isPrefixOf)

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

-- | Returns an `Output` for the terminal specified in `Config`.
--
-- The specific Output implementation used is hidden from the API user.
-- All terminal implementations are assumed to perform more, or less,
-- the same. Currently, all implementations use terminfo for at least
-- some terminal specific information.
--
-- If a terminal implementation is developed for a terminal without
-- terminfo support then Vty should work as expected on that terminal.
--
-- Selection of a terminal is done as follows:
--
--      * If TERM contains "xterm" or "screen", use XTermColor.
--      * otherwise use the TerminfoBased driver.
outputForConfig :: Config -> IO Output
outputForConfig Config{ outputFd = Just fd, termName = Just termName
                      , colorMode = Just colorMode, .. } = do
    t <- if "xterm" `isPrefixOf` termName || "screen" `isPrefixOf` termName
        then XTermColor.reserveTerminal termName fd colorMode
        -- Not an xterm-like terminal. try for generic terminfo.
        else TerminfoBased.reserveTerminal termName fd colorMode

    case mouseMode of
        Just s -> setMode t Mouse s
        Nothing -> return ()

    case bracketedPasteMode of
        Just s -> setMode t BracketedPaste s
        Nothing -> return ()

    return t
outputForConfig config = (<> config) <$> standardIOConfig >>= outputForConfig

-- | Sets the cursor position to the given output column and row.
--
-- This is not necessarially the same as the character position with the
-- same coordinates. Characters can be a variable number of columns in
-- width.
--
-- Currently, the only way to set the cursor position to a given
-- character coordinate is to specify the coordinate in the Picture
-- instance provided to 'outputPicture' or 'refresh'.
setCursorPos :: Output -> Int -> Int -> IO ()
setCursorPos t x y = do
    bounds <- displayBounds t
    when (x >= 0 && x < regionWidth bounds && y >= 0 && y < regionHeight bounds) $ do
        dc <- displayContext t bounds
        outputByteBuffer t $ writeToByteString $ writeMoveCursor dc x y

-- | Hides the cursor.
hideCursor :: Output -> IO ()
hideCursor t = do
    bounds <- displayBounds t
    dc <- displayContext t bounds
    outputByteBuffer t $ writeToByteString $ writeHideCursor dc

-- | Shows the cursor.
showCursor :: Output -> IO ()
showCursor t = do
    bounds <- displayBounds t
    dc <- displayContext t bounds
    outputByteBuffer t $ writeToByteString $ writeShowCursor dc
