{-# LANGUAGE DeriveFunctor #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Game.Yaku
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Game.Yaku where

import ClassyPrelude
import Control.Monad.Free
import qualified Data.List as L

-----------------------------------------
import Hajong.Game.Tiles
import Hajong.Game.Types

-- * Mentsu

type Mentsus = [Mentsu]
type CompleteHand = [Mentsu]

handComplete :: Mentsus -> [Tile] -> Bool
handComplete mentsu = not . null .
    mapMaybe (isComplete . (mentsu ++)) . mentsuSearch

mentsuSearch :: [Tile] -> [Mentsus]
mentsuSearch =
    buildVariations . map getMentsu .
    groupBy compareSuit . sort

buildVariations :: [[Mentsus]] -> [Mentsus]
buildVariations = go
    where go (x : xs) = x >>= (`map` go xs) . (++)
          go []       = []

-- | This gets only total mentsu: no orphan tiles are left in the result.
getMentsu :: [Tile] -> [Mentsus]
getMentsu tiles = map fst $ go ([], tiles)
    where 
        go :: ([Mentsu], [Tile]) -> [([Mentsu], [Tile])] -- (mentsu, leftovers)
        go (done, xs@(a:b:es)) = let

            takingJantou  = if a == b then go (Jantou [a,b] Nothing : done, es) ++ takingKoutsu else []
            takingKoutsu  = case es of
                (c:es') | c == a      -> go (Koutsu [a,b,c] Nothing : done, es') ++ takingKantsu
                        | otherwise   -> []
                _                     -> []
            takingKantsu  = case es of
                (c:d:es') | c == d    -> go (Kantsu [a,b,c,d] Nothing : done, es')
                          | otherwise -> []
                _                     -> []
            takingShuntsu = case shuntsuOf a xs of
                Just ment   -> go (ment : done, xs L.\\ mentsuPai ment)
                Nothing     -> []
            in takingJantou ++ takingShuntsu

        go (done, []) = [(done, [])]
        go (_,  _)    = [] -- branch cannot be complete with one orphan tile

shuntsuOf :: Tile -> [Tile] -> Maybe Mentsu
shuntsuOf a xs = do
    r <- tileSucc a
    s <- tileSucc r
    guard (tileSuited a && r `elem` xs && s `elem` xs)
    return $ Shuntsu [a,r,s] Nothing

isShuntsu :: [Tile] -> Bool
isShuntsu xs = case sort xs of
    (x:y:z:[]) -> tileSuited x && Just y == tileSucc x && Just z == tileSucc y
    _ -> False

isComplete :: Mentsus -> Maybe CompleteHand
isComplete xs = do
    guard $ length xs == 5
    guard $ length (filter isJantou xs) == 1
    return xs

-- | Documentation for 'isJantou'
isJantou :: Mentsu -> Bool
isJantou (Jantou{}) = True
isJantou _ = False

-- * YakuChecker

type Yaku = Free YakuChecker

data YakuChecker next = YakuMentsu MentsuKind MentsuProp next -- tile kind about first tile. As optimization put these before tile-dependant checks
                      | YakuMentsu' MentsuKind MentsuProp (Tile -> next)
                      | YakuStateful (YakuInfo -> next)
                      | YakuHandConcealed (Maybe (Bool, next))
                      | YakuHandOpen next
                      deriving (Functor)

data YakuInfo = YakuInfo
              { yakuRoundKaze :: Kazehai
              , yakuPlayerKaze :: Kazehai
              }

data MentsuKind = MentsuJantou
                | MentsuAny      -- Note: NOT jantou
                | MentsuAnyJantou
                | MentsuShuntsu
                | MentsuKoutsu
                | MentsuKantsu
                | MentsuKoutsuKantsu

-- * Functions

concealedHandDegrade :: Yaku ()
concealedHandDegrade = liftF $ YakuHandConcealed (Just (True, ()))

concealedHand :: Yaku ()
concealedHand = liftF $ YakuHandConcealed (Just (False, ()))

openHand :: Yaku ()
openHand = liftF $ YakuHandConcealed Nothing

yakuState :: Yaku YakuInfo
yakuState = liftF (YakuStateful id)

anyKoutsu, anyKantsu, anyShuntsu, anyJantou, anyMentsu, anyKoutsuKantsu, anyMentsuJantou :: MentsuProp -> Yaku ()
anyKoutsu  tkind       = liftF $ YakuMentsu MentsuKoutsu  tkind ()
anyShuntsu tkind       = liftF $ YakuMentsu MentsuShuntsu tkind ()
anyKantsu  tkind       = liftF $ YakuMentsu MentsuKantsu  tkind ()
anyJantou  tkind       = liftF $ YakuMentsu MentsuJantou  tkind ()
anyMentsu  tkind       = liftF $ YakuMentsu MentsuAny     tkind ()
anyKoutsuKantsu  tkind = liftF $ YakuMentsu MentsuKoutsuKantsu tkind ()
anyMentsuJantou tkind  = liftF $ YakuMentsu MentsuAnyJantou tkind ()

anyShuntsu', anyKoutsuKantsu', anyMentsu', anyMentsuJantou' :: MentsuProp -> Yaku Tile
anyKoutsuKantsu' tkind = liftF $ YakuMentsu' MentsuKoutsuKantsu tkind id
anyShuntsu' tkind      = liftF $ YakuMentsu' MentsuShuntsu tkind id
anyMentsu' tkind       = liftF $ YakuMentsu' MentsuAny tkind id
anyMentsuJantou' tkind = liftF $ YakuMentsu' MentsuAnyJantou tkind id

-- * MentsuProps

data MentsuProp = TileTerminal
                | TileSameAs Tile
                | TileSuited
                | TileSameSuit Tile
                | TileSameNumber Tile
                | TileNumber Number
                | TileHonor
                | TileSangenpai
                | TileAnd MentsuProp MentsuProp
                | TileOr MentsuProp MentsuProp
                | TileNot MentsuProp
                | TileConcealed
                | TileAny

ofTileType :: MentsuProp -> Mentsu -> Bool
ofTileType TileTerminal Shuntsu{mentsuPai = (x:_) } = tileNumber x == Ii || tileNumber x == Chii
ofTileType tt mentsu = let firstTile = unsafeHead $ mentsuPai mentsu
    in case tt of
        TileTerminal        -> tileTerminal firstTile
        TileSameAs tile     -> firstTile == tile
        TileSuited          -> tileSuited firstTile
        TileSameSuit tile   -> compareSuit tile firstTile
        TileSameNumber tile -> tileNumber tile      == tileNumber firstTile
        TileNumber n        -> tileNumber firstTile == n
        TileHonor           -> not $ tileSuited firstTile
        TileSangenpai       -> tileSangenpai firstTile
        TileAnd x y         -> ofTileType x mentsu && ofTileType y mentsu
        TileOr x y          -> ofTileType x mentsu || ofTileType y mentsu
        TileNot x           -> not $ ofTileType x mentsu
        TileConcealed       -> isNothing $ mentsuOpen mentsu
        TileAny             -> True

terminal, honor, sangenpai, suited, anyTile, concealed :: MentsuProp
terminal  = TileTerminal
honor     = TileHonor
sangenpai = TileSangenpai
suited    = TileSuited
anyTile   = TileAny
concealed = TileConcealed

sameTile, sameNumber, sameSuit :: Tile -> MentsuProp
sameTile = TileSameAs
sameNumber = TileSameNumber
sameSuit = TileSameSuit

ofNumber :: Number -> MentsuProp
ofNumber = TileNumber

(&.), (|.) :: MentsuProp -> MentsuProp -> MentsuProp
(&.) = TileAnd
(|.) = TileOr
infixl 1 &., |.

propNot :: MentsuProp -> MentsuProp
propNot = TileNot

allMentsuOfKind :: MentsuProp -> Yaku ()
allMentsuOfKind tkind = do
    replicateM_ 4 $ anyMentsu tkind
    anyJantou tkind

-- ** Special

yakuChiitoitsu :: Yaku ()
yakuChiitoitsu = undefined

-- ** 4 + 1

-- *** Shuntsu

yakuPinfu :: Yaku Int
yakuPinfu = do
    concealedHand
    replicateM_ 4 (anyShuntsu suited)
    anyJantou suited
    return 1

yakuIipeikou :: Yaku Int
yakuIipeikou = do
    concealedHand
    tile <- anyShuntsu' anyTile
    anyShuntsu (sameTile tile)
    return 1

yakuRyanpeikou :: Yaku Int
yakuRyanpeikou = concealedHand >> yakuIipeikou >> yakuIipeikou >> return 3

yakuSanshokuDoujin :: Yaku Int
yakuSanshokuDoujin = do
    concealedHandDegrade
    tile  <- anyShuntsu' anyTile
    tile' <- anyShuntsu' (f tile)
    anyShuntsu (f tile' &. f tile)
    return 2
    where
        f tile = sameNumber tile &. propNot (sameSuit tile)

yakuIttsuu :: Yaku Int
yakuIttsuu = do
    concealedHandDegrade
    tile <- anyShuntsu' (ofNumber Ii)
    anyShuntsu (sameSuit tile &. ofNumber Suu)
    anyShuntsu (sameSuit tile &. ofNumber Chii)
    return 2

-- *** Koutsu or Kantsu

-- NOTE this does not combine with chanta
yakuHonroutou :: Yaku Int
yakuHonroutou = do
    replicateM_ 4 $ anyKoutsuKantsu (terminal |. honor) 
    anyJantou (terminal |. honor)
    return 2

yakuToitoi :: Yaku Int
yakuToitoi = do
    replicateM_ 4 $ anyKoutsuKantsu anyTile
    return 2

yakuSanankou :: Yaku Int
yakuSanankou = do
    replicateM_ 3 $ anyKoutsuKantsu concealed
    return 2

yakuSanKantsu :: Yaku Int
yakuSanKantsu = do
    replicateM_ 3 $ anyKantsu anyTile
    return 2

yakuSanshokuDoukou :: Yaku Int
yakuSanshokuDoukou = do
    tile <- anyKoutsuKantsu' anyTile
    replicateM_ 2 $ anyKoutsuKantsu (sameNumber tile)
    return 2

yakuShouSangen :: Yaku Int
yakuShouSangen = do
    anyKoutsuKantsu sangenpai
    anyKoutsuKantsu sangenpai
    anyJantou sangenpai
    anyMentsu (propNot sangenpai)
    return 2

-- *** Tile kind based

yakuFanpai :: Yaku Int
yakuFanpai = do
    info <- yakuState
    let roundTile = Kaze $ yakuRoundKaze info
        playerKaze = Kaze $ yakuPlayerKaze info
    tile <- anyMentsu' (sangenpai |. sameTile roundTile |. sameTile playerKaze)
    return $ if roundTile == playerKaze && roundTile == tile
        then 2
        else 1

yakuTanyao :: Yaku Int
yakuTanyao = do
    concealedHand
    allMentsuOfKind suited
    return 1

yakuKuitan :: Yaku Int
yakuKuitan = do
    openHand
    allMentsuOfKind suited
    return 1

yakuChanta :: Yaku Int
yakuChanta = do -- TODO this does not notice 7-8-9 Shuntsu!
    anyShuntsu terminal
    replicateM_ 4 $ anyMentsuJantou (terminal |. honor)
    return 2

yakuHonitsu :: Yaku Int
yakuHonitsu = do
    concealedHandDegrade
    anyMentsuJantou honor
    tile <- anyMentsuJantou' suited
    replicateM_ 3 $ anyMentsuJantou (honor |. sameSuit tile)
    return 3

yakuJunchan :: Yaku Int
yakuJunchan = do
    concealedHandDegrade
    allMentsuOfKind terminal -- TODO this does not notice 7-8-9 shuntsu
    return 3

yakuChinitsu :: Yaku Int
yakuChinitsu = do
    concealedHandDegrade
    tile <- anyMentsu' suited
    replicateM_ 3 (anyMentsu $ sameSuit tile)
    anyJantou (sameSuit tile)
    return 6

-- *** Unrelated to mentsu

yakuMenzenTsumo :: Yaku ()
yakuMenzenTsumo = undefined

yakuRiichi :: Yaku ()
yakuRiichi = undefined

yakuIppatsu :: Yaku ()
yakuIppatsu = undefined

yakuDoubleRiichi :: Yaku ()
yakuDoubleRiichi = undefined

yakuHouteiRaoyui :: Yaku ()
yakuHouteiRaoyui = undefined

yakuRinshanKaihou :: Yaku ()
yakuRinshanKaihou = undefined

yakuChankan :: Yaku ()
yakuChankan = undefined

yakuNagashiMangan :: Yaku ()
yakuNagashiMangan = undefined
