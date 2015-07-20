import Data.Set (Set)
import qualified Data.Set as Set

boatLimit = 2

data Family = A | B | C deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Role = Father | Daughter deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Person = P Role Family deriving (Show, Read, Eq, Ord)

data BoatLocation = Near | Far deriving (Show, Read, Eq, Ord)

data Location = Shore BoatLocation | OnBoat deriving (Show, Read, Eq)

type NearShore = Set Person
type Boat = Set Person
type FarShore = Set Person

data World = World NearShore Boat FarShore BoatLocation deriving (Show, Read, Eq, Ord)

allPeople = [P r f | r <- enumAll, f <- enumAll]

initialWorld = World (Set.fromList allPeople) Set.empty Set.empty Near

enumAll :: (Bounded a, Enum a) => [a]
enumAll = [minBound .. maxBound]

isDaughterOf :: Person -> Person -> Bool
(P Daughter f1) `isDaughterOf` (P Father f2) = f1 == f2
_ `isDaughterOf` _ = False

legalMoves :: World -> Set World
legalMoves w = Set.filter isWorldLegal worlds
  where worlds = Set.fromList $ moveBoat w : [move p w | p <- allPeople]

locatePerson :: Person -> World -> Location
locatePerson p (World n _ f _) | p `Set.member` n = Shore Near
                               | p `Set.member` f = Shore Far
                               | otherwise        = OnBoat

move :: Person -> World -> World
move p w@(World n b f Near) =
  case locatePerson p w of
    Shore Near -> World (Set.delete p n) (Set.insert p b) f Near
    OnBoat     -> World (Set.insert p n) (Set.delete p b) f Near
    Shore Far  -> w  -- impossible; person is on different shore from boat
move p w@(World n b f Far) =
  case locatePerson p w of
    Shore Near -> w  -- impossible; person is on different shore from boat
    OnBoat     -> World n (Set.delete p b) (Set.insert p f) Far
    Shore Far  -> World n (Set.insert p b) (Set.delete p f) Far

moveBoat :: World -> World
moveBoat w@(World n b f Near) = if Set.size b > 0 then World n b f Far else w
moveBoat w@(World n b f Far)  = if Set.size b > 0 then World n b f Near else w

isWorldLegal :: World -> Bool
isWorldLegal (World n b f l)
  | Set.size b > boatLimit = False
  -- | 
  | otherwise = True
