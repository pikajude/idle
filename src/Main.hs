{-# LANGUAGE OverloadedStrings #-}

import Graphics.Vty.Widgets.All
import Idle.Screens.Home

main :: IO ()
main = do
    c <- newCollection
    _ <- uncurry (addToCollection c) =<< home

    runUi c defaultContext
