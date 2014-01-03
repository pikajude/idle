{-# LANGUAGE OverloadedStrings #-}

module Idle.Ore where

import Data.List.NonEmpty
import Data.String
import qualified Data.Text as T
import Graphics.Vty.Attributes
import Prelude hiding (readFile)

rockOf_ :: Color -> Color -> [(T.Text, Attr)]
rockOf_ b f = [ def "       ", blk "__", def "    \n"
              , def "     ", blk "_/" , c "@@", blk "`-", def "  \n"
              , def "    ", blk "/", c "@", blk "\\", c "@", blk "(", c "@@", blk "\\", def " \n"
              , def "    ", blk "\\", c "@@", blk "`", c "@", blk ")", c "@@", blk "\\", def "\n"
              , def "     ", blk "/", c "@@", blk "(", c "@@", blk "|", def " \n"
              , def "     ", blk "\\", c "@@@@@", blk "/", def " "
              ]
    where blk m = (m, Attr KeepCurrent (SetTo b) KeepCurrent)
          c m = (m, Attr KeepCurrent (SetTo f) KeepCurrent)
          def m = (m, def_attr)

rockOf :: Color -> [(T.Text, Attr)]
rockOf = rockOf_ black

lightRockOf :: Color -> [(T.Text, Attr)]
lightRockOf = rockOf_ white

mistyBone :: IsString s => Color -> [(s, Attr)]
mistyBone f = [ col "\n       ) (\n   )  (   ) )\n  ( "
              , light "_", col "  )   (", light "_ "
              , col "(", light "\n   (_'-----'_) ", col ")"
              , light "\n   (_.'\"\"\"\"._)" ] where
    col c = (c, Attr KeepCurrent (SetTo f) KeepCurrent)
    light c = (c, Attr KeepCurrent (SetTo (ISOColor 231)) KeepCurrent)

data Ore = Ore
         { name :: T.Text
         , hp :: Int
         , defense :: Int
         , value :: Int
         , image :: [(T.Text, Attr)]
         } deriving Show

poo = Ore
    { name    = "Poo"
    , hp      = 100
    , defense = 0
    , value   = 2
    , image = [ ("       (    )\n    (   ) (\n     ) ", fg_green)
              , ("_", fg_brown)
              , ("   )\n", fg_green)
              , ("      ( \\_\n    _(_\\ \\)__\n   (____\\___))", fg_brown)
              ]
    }
    where fg_brown = Attr KeepCurrent (SetTo (ISOColor 95)) KeepCurrent
          fg_green = Attr KeepCurrent (SetTo green) KeepCurrent

paper = Ore
    { name    = "Paper"
    , hp      = 400
    , defense = 3
    , value   = 10
    , image   = [ ("       ____ \n      /|   |\n     /_|   |\n     |     |\n     |     |\n     |_____|"
                  , Attr KeepCurrent (SetTo white) KeepCurrent) ]
    }

salt = Ore
    { name    = "Salt"
    , hp      = 700
    , defense = 15
    , value   = 22
    , image   = [ ("       __     \n      (__)    \n    _(_)(_)_  \n   (__)(____) \n  (__)(_)(___)\n"
                  , Attr KeepCurrent (SetTo white) KeepCurrent) ]
    }

clay = Ore "Clay" 1400 35 50 (rockOf (ISOColor 138))

rock = Ore "Rock" 2200 90 120 (rockOf (ISOColor 246))

coal = Ore "Coal" 4000 200 275 (rockOf (ISOColor 8))

bone = Ore
    { name    = "Bone"
    , hp      = 7000
    , defense = 380
    , value   = 580
    , image = [ ("\n\n    _       _ \n   (_'-----'_)\n   (_.'\"\"\"\"._)\n"
                , Attr KeepCurrent (SetTo (ISOColor 231)) KeepCurrent) ]
    }

lead = Ore "Lead" 12400 700 1100 (rockOf (ISOColor 232))

iron = Ore "Iron" 16000 1140 1850 (rockOf (ISOColor 240))

copper = Ore "Copper" 25000 1600 3200 (rockOf (ISOColor 52))

carbonite = Ore "Carbonite" 40000 2500 5200 (rockOf (ISOColor 232))

quartz = Ore "Quartz" 64000 3800 8600 (rockOf white)

spookyBone = Ore "Spooky Bone" 92000 5400 14000 (mistyBone black)

silver = Ore "Silver" 128000 7200 20000 (rockOf (ISOColor 248))

crystal = Ore "Crystal" 200000 9999 42000 (lightRockOf (ISOColor 81))

topaz = Ore "Topaz" 500000 13500 140000 (rockOf (ISOColor 208))

amethyst = Ore "Amethyst" 1400000 18000 480000 (rockOf_ (ISOColor 99) (ISOColor 56))

aquamarine = Ore "Aquamarine" 4800000 24500 2200000 (rockOf (ISOColor 33))

emerald = Ore "Emerald" 13000000 34000 7200000 (rockOf_ (ISOColor 23) (ISOColor 36))

ruby = Ore "Ruby" 42000000 50000 25500000 (rockOf red)

sapphire = Ore "Sapphire" 120000000 80000 85000000 (rockOf (ISOColor 21))

hauntedBone = Ore "Haunted Bone" 200000000 130000 190000000 (mistyBone red)

gold = Ore "Gold" 360000000 200000 400000000 (rockOf_ (ISOColor 220) (ISOColor 202))

platinum = Ore "Platinum" 500000000 295000 760000000 (rockOf_ (ISOColor 24) (ISOColor 81))

diamond = Ore "Diamond" 700000000 440000 1600000000 (rockOf_ (ISOColor 245) white)

mithril = Ore "Mithril" 1000000000 680000 2800000000 (rockOf (ISOColor 24))

obsidian = Ore "Obsidian" 1400000000 1050000 4800000000 (rockOf_ (ISOColor 128) black)

poo, paper, salt, clay, rock, coal, bone, lead, iron
  , copper, carbonite, quartz, spookyBone, silver
  , crystal, topaz, amethyst, aquamarine, emerald
  , ruby, sapphire, hauntedBone, gold, platinum
  , diamond, mithril, obsidian
  :: Ore

ores :: NonEmpty Ore
ores = poo :| [ paper, salt, clay, rock, coal, bone, lead, iron
              , copper, carbonite, quartz, spookyBone, silver
              , crystal, topaz, amethyst, aquamarine, emerald
              , ruby, sapphire, hauntedBone, gold, platinum
              , diamond, mithril, obsidian
              ]
