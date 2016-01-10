{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
------------------------------------------------------------------------------
-- |
-- Module         : Hajong.Client.Pretty
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Client.Pretty where

import Prelude hiding ((<>))
import Data.List (transpose, cycle)
import Data.Text (splitOn, chunksOf, justifyLeft, justifyRight)
import qualified Data.List.Split as L (chunksOf)
import Text.PrettyPrint.ANSI.Leijen (Pretty(..), string, (<>))

import Mahjong
import Hajong.Connections

-- * PP

ppGame :: Int -> (Text, Set Nick) -> Text
ppGame n (name,nicks) = mconcat ["(", tshow n, ") ", name, " [", ppNicks nicks, "]"]

ppNicks :: Set Nick -> Text
ppNicks nicks = case setToList nicks of
            [] -> ""
            (x:xs) -> foldl' (\a b -> a <> ", " <> b) x xs

-- * Functions

breakGamePlayer :: GamePlayer -> [PInfo]
breakGamePlayer pstate = zipWith
      (\(player, nick, points) (_, hand) -> (player, nick, points, hand))
      (_playerPlayers pstate)
      (itoList $ _playerPublicHands pstate)

-- | "pushToPlace src (y, x) dest" puts src start at row y and column x in
-- dest character matrix. dest is possibly grown in both directions in
-- order to fit src to it.
pushToPlace :: Text -> (Int, Int) -> [Text] -> [Text]
pushToPlace text (y, x) = goy y
    where
        goy 0 texts         = zipWith gox (srcs ++ repeat "") (texts ++ replicate (length srcs - length texts) "")
        goy y' (txt : txts) = txt : goy (y' - 1) txts
        goy y' _            = ""  : goy (y' - 1) []

        srcs = lines text

        gox src dest = let (a, b) = splitAt x dest & _1%~justifyLeft x ' '
                                                   & _2%~snd.splitAt (length src)
                           in a <> src <> b

-- | In order mine-right-front-left
mapPositions :: (Pretty (PosMine x), Pretty (PosRight x), Pretty (PosLeft x), Pretty (PosFront x))
             => [x] -> [Text]
mapPositions = zipWith ($)
    [ pretty . PosMine
    , pretty . PosRight
    , pretty . PosFront
    , pretty . PosLeft ]

discardHelper :: (Text -> Text) -> [[(Tile, Maybe Player)]] -> Text
discardHelper f = intercalate "\n" . map (f . unwords . map (pretty . fst))

-- | helper function for discard pretty printers
discardSplit :: [a] -> [[a]]
discardSplit = (\(xs, x) -> xs ++ [x]) . over _2 join . splitAt 2 . L.chunksOf 6

-- | Nick and points justified
pinfoNickPoints :: Int -> PInfo -> Text
pinfoNickPoints n (_, nick, points, _) = intercalate "\n" $ map (justifyRight n ' ')
    [ pretty (PNick nick), pretty (PPoints points) ]

-- * Related types

type PInfo = (Player, Nick, Points, HandPublic)

newtype PPoints = PPoints Int

newtype PNick = PNick Text

type Discards = [(Tile, Maybe Player)]

-- * Positional
-- | Pos-wrappers are used to indicate the position of an object in the
-- game table.
newtype PosMine a  = PosMine { posMine :: a } deriving (Show, Read)
newtype PosFront a = PosFront { posFront :: a } deriving (Show, Read)
newtype PosLeft a  = PosLeft { posLeft :: a } deriving (Show, Read)
newtype PosRight a = PosRight { posRight :: a } deriving (Show, Read)

----------------------------------------------------------

-- Instances

-- Player Info

instance Pretty GamePlayer where
    pretty = do
        dora      <- view $ playerPublic.pDora.to pretty
        concealed <- view $ playerMyHand.to pretty
        wallCount <- view $ playerPublic.sWallTilesLeft.to (\x -> "(" <> tshow x <> ")")

        -- rotate players to right positions
        me      <- view playerPlayer
        players <- let magic = take 4 . dropWhile (^. _1.to (/= me)) . cycle . breakGamePlayer
                       in view $ to magic

        let [dmine, dright, dfront, dleft]             = mapPositions (players ^.. each._4.handDiscards)
            [infoMine, infoRight, infoFront, infoLeft] = mapPositions (players ^.. each)
            [_, handRight, handFront, handLeft]        = mapPositions (players ^.. each._4.handCalled.to (OtherConceal . (\o -> 13 - o * 3) . length))
            [openMine, openRight, openFront, openLeft] = map pretty    (players ^.. each._4.handCalled)

        return $ string $ unlines $ []
                & pushToPlace dora      (12, 24)
                & pushToPlace wallCount (10, 24)
                & pushToPlace dmine     (15, 22)
                & pushToPlace dleft     (9 , 14)
                & pushToPlace dfront    (6 , 22) -- TODO fails if discards go over 6*3
                & pushToPlace dright    (9 , 39) --  here too
                & pushToPlace concealed (21, 4 )
                & pushToPlace openMine  (21, 49)
                & pushToPlace openRight (2 , 49)
                & pushToPlace openFront (1 , 5 )
                & pushToPlace openLeft  (16, 1 )
                & pushToPlace infoMine  (23, 9 )
                & pushToPlace infoRight (7 , 51)
                & pushToPlace infoFront (2 , 17)
                & pushToPlace infoLeft  (6 , 2 )
                & pushToPlace handRight (6 , 11)
                & pushToPlace handFront (4 , 17)
                & pushToPlace handLeft  (7 , 49)

instance Pretty (PosMine PInfo) where
    pretty (PosMine info) = let player = pretty (info^._1)
                               in player <> drop (length player) (pinfoNickPoints 28 info)

instance Pretty (PosLeft PInfo)  where pretty (PosLeft info)  = pretty (info^._1) <> "\n\n" <> pinfoNickPoints 8 info
instance Pretty (PosRight PInfo) where pretty (PosRight info) = pretty (info^._1) <> "\n\n" <> pinfoNickPoints 8 info
instance Pretty (PosFront PInfo) where pretty (PosFront info) = pretty (PosMine info)

instance Pretty PPoints    where pretty (PPoints points) = "(" <> tshow points <> ")"
instance Pretty PNick      where pretty (PNick nick)     = nick
instance Pretty Player     where pretty (Player kaze)    = tshow kaze <> " (" <> take 1 (pretty $ toKazehai kaze) <> ")"

newtype OtherConceal = OtherConceal  Int

instance Pretty (PosMine  OtherConceal) where pretty (PosMine (OtherConceal _))  = error "No Pretty for (OtherConceael PosMine)"
instance Pretty (PosRight OtherConceal) where pretty (PosRight (OtherConceal n)) = intersperse '\n' $ replicate n '|'
instance Pretty (PosLeft  OtherConceal) where pretty (PosLeft (OtherConceal n))  = intersperse '\n' $ replicate n '|'
instance Pretty (PosFront OtherConceal) where pretty (PosFront (OtherConceal n)) = mconcat (replicate n "_ " :: [Text])
-- Discards

instance Pretty (PosMine  Discards) where pretty (PosMine  tiles) = discardHelper (justifyLeft (6*3-1)   ' ') $ discardSplit tiles
instance Pretty (PosLeft  Discards) where pretty (PosLeft  tiles) = discardHelper (justifyRight 8       ' ') $ transpose $ reverse $ discardSplit tiles
instance Pretty (PosRight Discards) where pretty (PosRight tiles) = discardHelper (justifyLeft 8        ' ') $ reverse $ transpose $ discardSplit tiles
instance Pretty (PosFront Discards) where pretty (PosFront tiles) = discardHelper (justifyRight (6*3-1) ' ') $ reverse $ reverse <$> discardSplit tiles
--        $ filter (isn't _Nothing . view _2) -- plant this to indicate
--        shouts
