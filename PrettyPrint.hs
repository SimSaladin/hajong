{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
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

-- * Functions
 
breakGamePlayer :: GamePlayer Text -> [(Player, Maybe Text, Points, HandPublic)]
breakGamePlayer pstate = zipWith (\(player, mnick, points) (_, hand) -> (player, mnick, points, hand))
      (_playerPlayers pstate)
      (itoList $ _playerPublicHands pstate)

-- | "pushToPlace src (y, x) dest" puts src start at row y and column x in
-- dest character matrix. dest is possibly grown in both directions in
-- order to fit src to it.
pushToPlace :: Text -> (Int, Int)-> [Text] -> [Text]
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
mapPositions :: (PrettyPrint (PosMine x), PrettyPrint (PosRight x), PrettyPrint (PosLeft x), PrettyPrint (PosFront x))
             => [x] -> [Text]
mapPositions = zipWith ($)
    [ pshow . PosMine
    , pshow . PosRight
    , pshow . PosFront
    , pshow . PosLeft ]

discardHelper :: (Text -> Text) -> [[(Tile, Maybe Player)]] -> Text
discardHelper f = intercalate "\n" . map (f . unwords . map (pshow . fst))

-- | helper function for discard pretty printers
discardSplit :: [a] -> [[a]]
discardSplit = (\(xs, x) -> xs ++ [x]) . over _2 join . splitAt 2 . L.chunksOf 6

-- | Nick and points justified
pinfoNickPoints :: Int -> PInfo -> Text
pinfoNickPoints n (_, nick, points, _) = intercalate "\n" $ map (justifyRight n ' ')
    [ pshow (PNick nick), pshow (PPoints points) ]

-- * Related types 

type PInfo = (Player, Maybe Text, Points, HandPublic)

newtype PPoints = PPoints Int

newtype PNick = PNick (Maybe Text)

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

instance PrettyPrint (GamePlayer Text) where
    pshow = do
        dora      <- view $ playerPublic.riichiDora.to pshow
        concealed <- view $ playerMyHand.to pshow
        wallCount <- view $ playerPublic.riichiWallTilesLeft.to (\x -> "(" <> tshow x <> ")")

        -- rotate players to right positions
        me      <- view playerPlayer
        players <- let magic = take 4 . dropWhile (^. _1.to (/= me)) . cycle . breakGamePlayer
                       in view $ to magic

        let [dmine, dright, dfront, dleft]             = mapPositions (players ^.. each._4.handDiscards)
            [openMine, openRight, openFront, openLeft] = map pshow (players ^.. each._4.handOpen)
            [infoMine, infoRight, infoFront, infoLeft] = mapPositions (players ^.. each)
            [_, handRight, handFront, handLeft]        = mapPositions (players ^.. each._4.handOpen.to (OtherConceal . (*3) . length))

        return $ unlines $ []
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

instance PrettyPrint (PosMine PInfo) where
    pshow (PosMine info) = let player = pshow (info^._1)
                               in player <> drop (length player) (pinfoNickPoints 28 info)

instance PrettyPrint (PosLeft PInfo)  where pshow (PosLeft info)  = pshow (info^._1) <> "\n\n" <> pinfoNickPoints 8 info
instance PrettyPrint (PosRight PInfo) where pshow (PosRight info) = pshow (info^._1) <> "\n\n" <> pinfoNickPoints 8 info
instance PrettyPrint (PosFront PInfo) where pshow (PosFront info) = pshow (PosMine info)

instance PrettyPrint PPoints    where pshow (PPoints points) = "(" <> tshow points <> ")"
instance PrettyPrint PNick      where pshow (PNick mnick)    = fromMaybe "<nobody>" mnick
instance PrettyPrint Player     where pshow (Player kaze)    = tshow kaze <> " (" <> take 1 (pshow $ Kaze kaze) <> ")"

newtype OtherConceal = OtherConceal  Int

instance PrettyPrint (PosMine  OtherConceal) where pshow (PosMine (OtherConceal _))  = error "No PrettyPrint for (OtherConceael PosMine)"
instance PrettyPrint (PosRight OtherConceal) where pshow (PosRight (OtherConceal n)) = intersperse '\n' $ replicate n '|'
instance PrettyPrint (PosLeft  OtherConceal) where pshow (PosLeft (OtherConceal n))  = intersperse '\n' $ replicate n '|'
instance PrettyPrint (PosFront OtherConceal) where pshow (PosFront (OtherConceal n)) = mconcat (replicate n "_ " :: [Text])

-- Hand

-- | Own hand
instance PrettyPrint Hand where
    pshow = do
        concealed   <- view handConcealed
        mpick       <- view handPick
        return $ pshow concealed <> maybe "" (\p -> " | " <> pshow p) mpick

-- | Public hand
instance PrettyPrint HandPublic where
    pshow = do
        -- FIXME 
        tilenum <- view (handOpen.to length) <&> (13 -) . (*3)
        return $ unwords $ replicate tilenum "_"

-- Note: this is not really useful
instance PrettyRead Hand where
    pread = initHand . pread

-- Discards

instance PrettyPrint (PosMine  Discards) where pshow (PosMine  tiles) = discardHelper (justifyLeft (6*3-1)   ' ') $ discardSplit tiles
instance PrettyPrint (PosLeft  Discards) where pshow (PosLeft  tiles) = discardHelper (justifyRight 8       ' ') $ transpose $ reverse $ discardSplit tiles
instance PrettyPrint (PosRight Discards) where pshow (PosRight tiles) = discardHelper (justifyLeft 8        ' ') $ reverse $ transpose $ discardSplit tiles
instance PrettyPrint (PosFront Discards) where pshow (PosFront tiles) = discardHelper (justifyRight (6*3-1) ' ') $ reverse $ reverse <$> discardSplit tiles
--        $ filter (isn't _Nothing . view _2) -- plant this to indicate
--        shouts

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
