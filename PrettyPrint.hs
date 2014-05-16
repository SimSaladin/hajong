{-# LANGUAGE FlexibleInstances #-}
module PrettyPrint where

import ClassyPrelude
import Control.Lens
import Tiles
import Riichi
import Data.List (transpose, cycle)
import Data.Text (splitOn, chunksOf, justifyLeft, justifyRight)
import qualified Data.List.Split as L (chunksOf)

class PrettyPrint x where
    pshow :: x -> Text

class PrettyRead x where
    pread :: Text -> x

-- Discard pile position
newtype DiscardPileOwn   = DiscardPileOwn [(Tile, Maybe Player)] deriving (Show, Read, Eq)
newtype DiscardPileLeft  = DiscardPileLeft [(Tile, Maybe Player)] deriving (Show, Read, Eq)
newtype DiscardPileRight = DiscardPileRight [(Tile, Maybe Player)] deriving (Show, Read, Eq)
newtype DiscardPileFront = DiscardPileFront [(Tile, Maybe Player)] deriving (Show, Read, Eq)

-- Player

instance PrettyPrint (GamePlayer Text) where
    pshow pstate =
        let me      = _playerPlayer pstate
            _east   = pstate ^. playerPublic.riichiDealer
            
            players = take 4
                $ dropWhile (^. _1.to (/= me))
                $ cycle
                $ breakGamePlayer pstate

            discards = zipWith ($) [ pshow . DiscardPileOwn, pshow . DiscardPileRight
                                   , pshow . DiscardPileFront, pshow . DiscardPileLeft
                                   ] (players ^.. each._4.handDiscards)
            in unlines discards
                           
breakGamePlayer :: GamePlayer Text -> [(Player, Maybe Text, Points, HandPublic)]
breakGamePlayer pstate = zipWith (\(player, mnick, points) (_, hand) -> (player, mnick, points, hand))
      (_playerPlayers pstate)
      (itoList $ _playerPublicHands pstate)

--      [front player]      [right player]
-- [left player] [discards]
--      [me player]
--
--  0-      mm-mm-mm
--  |       mm-mm-mm                                                mm-mm-mm
--  |       mm-mm-mm    N       Player1                             mm-mm-mm-mm
--  3-----  mm-mm-mm-mm _ _ _ _ _ _ _ _ _ _ _ _ _                   mm-mm-mm-mm
--  |                                                               mm-mm-mm
--  5--- E        |        XX XX XX XX XX XX XX XX      XX          |
--  |             |              XX XX XX XX XX XX      XX          | W
--  7---          |              XX XX XX XX XX XX      XX          |
--  8--           |      XX XX XX                 XX XX XX          |  
--  9---  Player3 |      XX XX XX  (NN)           XX XX XX          | Player4
--  |             |      XX XX XX                 XX XX XX          |  
--  |     (25000) |      XX XX XX Do Ra He Re ..  XX XX XX          | (25000)
--  |             |      XX XX XX                 XX XX XX          |  
--  |             |      XX XX XX                 XX XX XX          |  
--  14--          |      XX XX XX                 XX XX XX          |  
--  15--          |      XX      XX XX XX XX XX XX                  |
--  |      mm-mm-mm      XX      XX XX XX XX XX XX                  |
--  17---  mm-mm-mm      XX      XX XX XX XX XX XX XX XX XX         |
--  |      mm-mm-mm      12----1920------>                          46-------10
--  |   mm-mm-mm-mm
--  |   0--------10                  
--  21--      01 02 03 04 05 06 07 08 09 10 11 12 13   14
--  |                                         mm-mm-mm-mm
--  |              S Player3      (25000)     mm-mm-mm
--  |                                         mm-mm-mm
--  25-----------                             mm-mm-mm
--
-- 0        
--
--  Parts:  - mentsu (ankan, open)
--          - discard pile
--          - dead wall (NN)
--          - Dora indicators
--          - player avatars, points, seat winds

-- Hand

-- | Own hand
instance PrettyPrint Hand where
    pshow = (\conc tsumo -> conc <> " : " <> tsumo)
        <$> view (handConcealed.to pshow)
        <*> view (handPick.to (maybe "" pshow))

-- | Public hand
instance PrettyPrint HandPublic where
    pshow = undefined

-- Note: this is not really useful
instance PrettyRead Hand where
    pread = initHand . pread

-- Discards

instance PrettyPrint DiscardPileOwn where
    pshow (DiscardPileOwn tiles) =
        intercalate "\n" $ map (justifyLeft (6 * 3 - 1) ' ' . unwords)
        . discardSplit $ map fst $ filter (isn't _Nothing . view _2) tiles

instance PrettyPrint DiscardPileLeft where
    pshow (DiscardPileLeft tiles) =
        intercalate "\n" $ map (justifyRight 8 ' ' . unwords)
        $ transpose $ reverse $ discardSplit $ map fst $ filter (isn't _Nothing . view _2) tiles

instance PrettyPrint DiscardPileRight where
    pshow (DiscardPileRight tiles) =
        intercalate "\n" $ map (justifyLeft 8 ' ' . unwords)
        $ reverse $ transpose $ discardSplit $ map fst $ filter (isn't _Nothing . view _2) tiles

instance PrettyPrint DiscardPileFront where
    pshow (DiscardPileFront tiles) =
        intercalate "\n" $ map (justifyRight (6 * 3 - 1) ' ' . unwords)
        $ reverse $ reverse <$> discardSplit $ map fst $ filter (isn't _Nothing . view _2) tiles

-- | helper function for discard pretty printers
discardSplit :: [Tile] -> [[Text]]
discardSplit = (\(xs, x) -> xs ++ [x]) . over _2 join . splitAt 2 . L.chunksOf 6 . map pshow

-- Tile

instance PrettyPrint [Tile] where pshow = unwords . map pshow
instance PrettyRead [Tile]  where pread = map pread . words

instance PrettyPrint Tile where
    pshow (Man n aka) = (if aka then "m" else "M") <> pshow n
    pshow (Pin n aka) = (if aka then "p" else "P") <> pshow n
    pshow (Sou n aka) = (if aka then "s" else "S") <> pshow n
    pshow (Sangen sangen) = case sangen of
                                Haku    -> "W!"
                                Hatsu   -> "G!"
                                Chun    -> "R!"
    pshow (Kaze kaze) = case kaze of
                            Ton     -> "E "
                            Nan     -> "S "
                            Shaa    -> "W "
                            Pei     -> "N "

instance PrettyPrint (Tile, Maybe Player) where
    pshow (tile, Nothing) = pshow tile
    pshow (tile, Just _)  = pshow tile

instance PrettyRead Tile where
    pread xs = case m of
        "G" -> Sangen Hatsu
        "R" -> Sangen Chun
        "E" -> Kaze Ton

        "W" | n == "!"  -> Sangen Haku
            | otherwise -> Kaze Shaa

        "N" -> Kaze Pei
        "M" -> Man ( pread n) False
        "P" -> Pin ( pread n) False

        "S" | n == " "  -> Kaze Nan
            | otherwise -> Sou ( pread n) False

        _ -> error "no PrettyRead"
        where
            [m,n] = case chunksOf 1 xs of
                        [a,b] -> [a, b ]
                        [a]   -> [a," "]
                        _ -> error $ "Tile no read: " <> unpack xs

-- Number

instance PrettyRead Number where
    pread = toEnum . (\x -> x - 1) . fromMaybe (error "No read") . readMay

instance PrettyPrint Number where
    pshow n = tshow (fromEnum n + 1)

-- Mentsu

instance PrettyPrint Mentsu where
    pshow (Kantsu tiles _)  = intercalate "-" $ map pshow tiles
    pshow (Koutsu tiles _)  = intercalate "-" $ map pshow tiles
    pshow (Shuntsu tiles _) = intercalate "-" $ map pshow tiles
    pshow (Jantou tiles _)  = intercalate "-" $ map pshow tiles

instance PrettyRead Mentsu where
    pread input = case pread <$> splitOn "-" input of
        tiles@(x:y:_)
            | length tiles == 4           -> Kantsu tiles True
            | length tiles == 3 && x == y -> Koutsu tiles True
            | length tiles == 3           -> Shuntsu tiles True
            | length tiles == 2 && x == y -> Jantou tiles True
            | otherwise -> error "pread: no parse"
        _ -> error "pread: no parse"

instance PrettyPrint [Mentsu] where
    pshow = intercalate "\n" . map pshow
