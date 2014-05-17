module Yaku where

import ClassyPrelude
import Tiles
import qualified Data.List as L

type CompleteHand = [Mentsu]

-- [ [ [ Mentsu ] ] ] -> [CompleteHand] : [[Mentsu]]
-- ^ ^ ^ possibility
-- | \ list of possible within group
-- \ groups

mentsuSearch :: [Tile] -> [CompleteHand]
mentsuSearch =
    mapMaybe isComplete . buildVariations 
    . map (map fst . getMentsu) 
    . groupBy compareSuit 
    . sort

buildVariations :: [[[Mentsu]]] -> [CompleteHand]
buildVariations = go
    where go (x : xs) = x >>= (`map` go xs) . (++)
          go []       = []

getMentsu :: [Tile] -> [([Mentsu], [Tile])] -- (mentsu, leftovers)
getMentsu tiles = go ([], tiles)
    where 
        go :: ([Mentsu], [Tile]) -> [([Mentsu], [Tile])]
        go (done, xs@(a:b:es)) = let

            s             = tileSucc a
            t             = tileSucc b
            takingJantou  = if a == b then go (Jantou [a,b] False : done, es) ++ takingKoutsu else []
            takingKoutsu  = case es of
                (c:es') | c == a      -> go (Koutsu [a,b,c] False : done, es') ++ takingKantsu
                        | otherwise   -> []
                _                     -> []
            takingKantsu  = case es of
                (c:d:es') | c == d    -> go (Kantsu [a,b,c,d] False : done, es')
                          | otherwise -> []
                _                     -> []
            takingShuntsu = if tileSuited a && s `elem` xs && t `elem` xs 
                               then go (Shuntsu [a,s,t] False : done, xs L.\\ [a,s,t])
                               else []
            in takingJantou ++ takingShuntsu

        go (done, []) = [(done, [])]
        go (_,  _)    = [] -- branch cannot be complete with one orphan tile

isComplete :: [Mentsu] -> Maybe CompleteHand
isComplete xs = do
    guard $ (== 1) $ length $ filter isJantou xs
    return xs

-- | Documentation for 'isJantou'
isJantou :: Mentsu -> Bool
isJantou (Jantou{}) = True
isJantou _ = False

-- * Yaku

data Yaku = Yaku 
          { _yakuConcealed :: Int
          , _yakuOpen :: Maybe Int
          }

data YakuBuilder next = YakuPart [Tile] Mentsu next
                      | YakuMatch Int
                      | YakuNot

-- instance Functor YakuBuilder where
--     fmap f (YakuPart tiles mentsu next) = YakuPart tiles mentsu (f next)
--     fmap f                            x = x

-- list plausible yaku combinations
getYaku :: [Tile] -> [[Yaku]]
getYaku _ = undefined

-- * Hand-Tile-based

yakuPinfu :: Yaku
yakuPinfu = undefined

yakuTanyao :: Yaku
yakuTanyao = undefined

yakuIipeikou :: Yaku
yakuIipeikou = undefined

yakuFanpai :: Yaku
yakuFanpai = undefined

yakuSanshokuDoujin :: Yaku
yakuSanshokuDoujin = undefined

yakuIttsuu :: Yaku
yakuIttsuu = undefined

yakuChanta :: Yaku
yakuChanta = undefined

yakuHonroutou :: Yaku
yakuHonroutou = undefined

yakuToitoi :: Yaku
yakuToitoi = undefined

yakuSanankou :: Yaku
yakuSanankou = undefined

yakuSanKantsu :: Yaku
yakuSanKantsu = undefined

yakuSanshokuDoukou :: Yaku
yakuSanshokuDoukou = undefined

yakuChiitoitsu :: Yaku
yakuChiitoitsu = undefined

yakuShouSangen :: Yaku
yakuShouSangen = undefined

yakuHonitsu :: Yaku
yakuHonitsu = undefined

yakuJunchan :: Yaku
yakuJunchan = undefined

yakuRyanpeikou :: Yaku
yakuRyanpeikou = undefined

yakuChinitsu :: Yaku
yakuChinitsu = undefined

-- ** Other

yakuMenzenTsumo :: Yaku
yakuMenzenTsumo = undefined

yakuRiichi :: Yaku
yakuRiichi = undefined

yakuIppatsu :: Yaku
yakuIppatsu = undefined

yakuDoubleRiichi :: Yaku
yakuDoubleRiichi = undefined

yakuHouteiRaoyui :: Yaku
yakuHouteiRaoyui = undefined

yakuRinshanKaihou :: Yaku
yakuRinshanKaihou = undefined

yakuChankan :: Yaku
yakuChankan = undefined

yakuNagashiMangan :: Yaku
yakuNagashiMangan = undefined