{-# LANGUAGE OverloadedStrings #-}

module Idle.Screens.Home where

import Data.IORef
import Data.List.NonEmpty
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Idle.Ore
import Graphics.Vty hiding (string)
import Graphics.Vty.Widgets.All
import System.Exit
import System.IO.Unsafe
import Text.Printf

data Zipper a = Zipper
              { left :: [a]
              , zFocus :: a
              , right :: [a]
              }

enter :: NonEmpty a -> Zipper a
enter (m :| ms) = Zipper [] m ms

next :: Zipper a -> Maybe (Zipper a)
next (Zipper ls f (r:rs)) = Just $ Zipper (f:ls) r rs
next _ = Nothing

previous :: Zipper a -> Maybe (Zipper a)
previous (Zipper (l:ls) f rs) = Just $ Zipper ls l (f:rs)
previous _ = Nothing

num :: IORef (Zipper Ore)
num = unsafePerformIO $ ores >>= newIORef . enter

drawDisplay :: Widget FormattedText -> Widget FormattedText -> Widget FormattedText -> IO ()
drawDisplay disp l r = do
    (Zipper ls f rs) <- readIORef num
    setText l $ if null ls then "  " else "◀ "
    setText r $ if null rs then "  " else " ▶"
    setTextWithAttrs disp $ display f
    where
        showB m | m >= 10000000000 = T.pack $ printf "%.2fB" (fromIntegral (m `div` 10000000) / 100 :: Double)
        showB m = T.pack $ show m
        plain t = (T.cons '\n' $ T.center 16 ' ' t, def_attr)
        line = ("\n", def_attr)
        display (Ore n h d v i) = i
            ++ [ line, plain n
               , plain $ "HP: " <> showB h
               , plain $ "D: " <> showB d
               , plain $ "$" <> showB v]

home :: IO (Widget (Box Table FormattedText), Widget FocusGroup)
home = do
    legend <- plainText "[q]uit [u]pgrade [s]ave"
    oreDisplay <- plainText "nothing"
    leftArrow <- plainText "nothing"
    rightArrow <- plainText "nothing"
    oreTable <- newTable
        [ ColumnSpec (ColFixed 2) (Just AlignLeft) Nothing
        , ColumnSpec (ColFixed 20) (Just AlignCenter) Nothing
        , ColumnSpec (ColFixed 2) (Just AlignRight) Nothing
        ]
        BorderNone
    addRow oreTable [leftArrow, oreDisplay, rightArrow]
    homeScreen <- vBox oreTable legend
    f <- newFocusGroup
    _ <- addToFocusGroup f homeScreen

    drawDisplay oreDisplay leftArrow rightArrow

    homeScreen `onKeyPressed` \this k _ms -> case k of
        KEsc -> exitSuccess
        KASCII 'q' -> exitSuccess
        KASCII ']' -> do
            atomicModifyIORef' num (\z -> (fromMaybe z (next z), ()))
            drawDisplay oreDisplay leftArrow rightArrow
            return True
        KASCII '[' -> do
            atomicModifyIORef' num (\z -> (fromMaybe z (previous z), ()))
            drawDisplay oreDisplay leftArrow rightArrow
            return True
        KASCII 't' -> do
            vis <- getVisible this
            setVisible this (not vis) >> return True
        _ -> return False

    return (homeScreen, f)
