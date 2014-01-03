{-# LANGUAGE OverloadedStrings #-}

module Idle.Ore where

import Control.Applicative
import Data.Char
import Data.List.NonEmpty
import qualified Data.Text as T
import Graphics.Vty.Attributes
import Idle.Ore.Parse
import Paths_idle
import Prelude hiding (readFile)
import Text.Trifecta

rockOf_ :: Color -> Color -> [(T.Text, Attr)]
rockOf_ b f = [ def "\n       ", blk "__", def "    \n"
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

glowy :: Color -> Color -> Color -> [(T.Text, Attr)]
glowy g b i = [ glow "\n    \\", border "  __", glow "   /\n -,  "
              , border "_/", inner "@@", border "`-  ", glow ".-\n    "
              , border "/", inner "@", border "\\", inner "@", border "("
              , inner "@@", border "\\\n ", glow "-- ", border "\\"
              , inner "@@", border "`", inner "@", border ")", inner "@@"
              , border "\\ ", glow "--\n  ,  ", border "/", inner "@@"
              , border "(", inner "@@", border "|  ", glow ",\n ` . ", border "\\"
              , inner "@@@@@", border "/", glow " . `"
              ] where
    glow c = (c, Attr KeepCurrent (SetTo g) KeepCurrent)
    border c = (c, Attr KeepCurrent (SetTo b) KeepCurrent)
    inner c = (c, Attr KeepCurrent (SetTo i) KeepCurrent)

mistyBone :: Color -> Color -> [(T.Text, Attr)]
mistyBone f l = [ col "\n\n       ) (\n   )  (   ) )\n  ( "
                , light "_", col "  )   (", light "_ "
                , col "(", light "\n   (_'-----'_) ", col ")"
                , light "\n   (_.'\"\"\"\"._)" ] where
    col c = (c, Attr KeepCurrent (SetTo f) KeepCurrent)
    light c = (c, Attr KeepCurrent (SetTo l) KeepCurrent)

orb :: Color -> Color -> Color -> [(T.Text, Attr)]
orb x b i = [ border "      ____ ", ex "$\n  $$", inner ".X+. ", ex "$$"
            , border ".\n  .", ex "$$", inner "+-.", ex "$$   "
            , border ".\n  X", inner "XX", ex "$$$", inner "..    "
            , border ".\n  X", inner "Xxx", ex "$$$", inner "-..  "
            , border "'\n  `", inner "X", ex "$$", inner "xx+", ex "$$"
            , inner "--", border "'\n    ", ex "$", inner "XXXxxx", ex "$$"] where
    border c = (c, Attr KeepCurrent (SetTo b) KeepCurrent)
    ex c = (c, Attr KeepCurrent (SetTo x) KeepCurrent)
    inner c = (c, Attr KeepCurrent (SetTo i) KeepCurrent)

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
    , image = [ ("\n       (    )\n    (   ) (\n     ) ", fg_green)
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
    , image   = [ ("\n       ____ \n      /|   |\n     /_|   |\n     |     |\n     |     |\n     |_____|"
                  , Attr KeepCurrent (SetTo white) KeepCurrent) ]
    }

salt = Ore
    { name    = "Salt"
    , hp      = 700
    , defense = 15
    , value   = 22
    , image   = [ ("\n       __     \n      (__)    \n    _(_)(_)_  \n   (__)(____) \n  (__)(_)(___)\n"
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
    , image = [ ("\n\n\n\n    _       _ \n   (_'-----'_)\n   (_.'\"\"\"\"._)"
                , Attr KeepCurrent (SetTo (ISOColor 231)) KeepCurrent) ]
    }

lead = Ore "Lead" 12400 700 1100 (rockOf (ISOColor 232))

iron = Ore "Iron" 16000 1140 1850 (rockOf (ISOColor 240))

copper = Ore "Copper" 25000 1600 3200 (rockOf (ISOColor 52))

carbonite = Ore "Carbonite" 40000 2500 5200 (rockOf (ISOColor 232))

quartz = Ore "Quartz" 64000 3800 8600 (rockOf white)

spookyBone = Ore "Spooky Bone" 92000 5400 14000 (mistyBone black (ISOColor 231))

silver = Ore "Silver" 128000 7200 20000 (rockOf (ISOColor 248))

crystal = Ore "Crystal" 200000 9999 42000 (lightRockOf (ISOColor 81))

topaz = Ore "Topaz" 500000 13500 140000 (rockOf (ISOColor 208))

amethyst = Ore "Amethyst" 1400000 18000 480000 (rockOf_ (ISOColor 99) (ISOColor 56))

aquamarine = Ore "Aquamarine" 4800000 24500 2200000 (rockOf (ISOColor 33))

emerald = Ore "Emerald" 13000000 34000 7200000 (rockOf_ (ISOColor 23) (ISOColor 36))

ruby = Ore "Ruby" 42000000 50000 25500000 (rockOf red)

sapphire = Ore "Sapphire" 120000000 80000 85000000 (rockOf (ISOColor 21))

hauntedBone = Ore "Haunted Bone" 200000000 130000 190000000 (mistyBone red (ISOColor 231))

gold = Ore "Gold" 360000000 200000 400000000 (rockOf_ (ISOColor 220) (ISOColor 202))

platinum = Ore "Platinum" 500000000 295000 760000000 (rockOf_ (ISOColor 24) (ISOColor 81))

diamond = Ore "Diamond" 700000000 440000 1600000000 (rockOf_ (ISOColor 245) white)

mithril = Ore "Mithril" 1000000000 680000 2800000000 (rockOf (ISOColor 24))

obsidian = Ore "Obsidian" 1400000000 1050000 4800000000 (rockOf_ (ISOColor 128) black)

earthEssence = Ore "Earth Essence" 2000000000 1400000 8200000000 (glowy (ISOColor 22) (ISOColor 82) (ISOColor 208))

orbium = Ore "Orbium" 2600000000 2000000 13500000000 (orb (ISOColor 248) black (ISOColor 81))

novalite = Ore "Novalite" 3500000000 2800000 24500000000 (orb red black yellow)

magicCrystal = Ore "Magic Crystal" 4900000000 4000000 42000000000 (glowy green green (ISOColor 57))

darkstone = Ore "Darkstone" 7200000000 5800000 70000000000 (rockOf_ (ISOColor 124) black)

adamantium = Ore "Adamantium" 10000000000 8500000 125000000000 (rockOf_ (ISOColor 202) (ISOColor 161))

fireEssence = Ore "Fire Essence" 14000000000 12000000 200000000000 (glowy (ISOColor 202) (ISOColor 196) (ISOColor 228))

lunalite = Ore "Lunalite" 20000000000 17000000 340000000000 (orb (ISOColor 101) (ISOColor 216) (ISOColor 230))

mysterium = Ore "Mysterium" 30000000000 24000000 580000000000
    [("\n\n\n\n       ?\n\n", Attr KeepCurrent (SetTo white) KeepCurrent)]

cursedBone = Ore "Cursed Bone" 45000000000 33500000 1050000000000 (mistyBone (ISOColor 126) (ISOColor 75))

windEssence = Ore "Wind Essence" 68000000000 48000000 1700000000000 (glowy (ISOColor 51) (ISOColor 159) (ISOColor 123))

unobtanium = Ore "Unobtanium" 100000000000 69000000 3000000000000 (rockOf_ white black)

sollite = Ore "Sollite" 130000000000 95000000 4800000000000 (orb (ISOColor 214) (ISOColor 226) (ISOColor 196))

waterEssence = Ore "Water Essence" 175000000000 138000000 7200000000000 (glowy (ISOColor 20) (ISOColor 27) (ISOColor 33))

absurdium = Ore "Absurdium" 240000000000 205000000 12000000000000 (rockOf_ (ISOColor 247) black)

cosmolite = Ore "Cosmolite" 320000000000 300000000 20000000000000 (orb black blue white)

shadowEssence = Ore "Shadow Essence" 435000000000 440000000 30000000000000 (glowy (ISOColor 55) (ISOColor 240) (ISOColor 232))

poo, paper, salt, clay, rock, coal, bone, lead, iron
  , copper, carbonite, quartz, spookyBone, silver
  , crystal, topaz, amethyst, aquamarine, emerald
  , ruby, sapphire, hauntedBone, gold, platinum
  , diamond, mithril, obsidian, earthEssence, orbium
  , novalite, magicCrystal, darkstone, adamantium
  , fireEssence, lunalite, mysterium, cursedBone
  , windEssence, unobtanium, sollite, waterEssence
  , absurdium, cosmolite, shadowEssence
  :: Ore

ores :: IO (NonEmpty Ore)
ores = fmap fromList . sequence
     $ [poo, paper, salt]
    where
        sanitize = Prelude.map (\c -> if isAlphaNum c then toLower c else '-')
        ore s a b c = do
            m <- getDataFileName $ sanitize s ++ ".ore"
            cont <- parseFromFileEx oreParser m
            case cont of
                Success q -> return $ Ore (T.pack s) a b c q
                e -> error $ show e
        poo = ore "Poo" 100 0 2
        paper = ore "Paper" 400 3 10
        salt = ore "Salt" 700 15 22

-- ores :: NonEmpty Ore
-- ores = poo :| [ paper, salt, clay, rock, coal, bone, lead, iron
--               , copper, carbonite, quartz, spookyBone, silver
--               , crystal, topaz, amethyst, aquamarine, emerald
--               , ruby, sapphire, hauntedBone, gold, platinum
--               , diamond, mithril, obsidian, earthEssence, orbium
--               , novalite, magicCrystal, darkstone, adamantium
--               , fireEssence, lunalite, mysterium, cursedBone
--               , windEssence, unobtanium, sollite, waterEssence
--               , absurdium, cosmolite, shadowEssence
--               ]
