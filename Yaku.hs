module Yaku where

import ClassyPrelude
import Tiles

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
