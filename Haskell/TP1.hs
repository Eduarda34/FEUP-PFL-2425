import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- Function that returns all unique cities from the provided RoadMap
cities :: RoadMap -> [City]
cities roadmap = Data.List.nub [city | (city1, city2, _) <- roadmap, city <- [city1, city2]]

-- Function that returns a boolean indicating whether two cities are linked directly
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadmap city1 city2 = any matches roadmap
    where matches (x, y, _) = (x == city1 && y == city2) || (x == city2 && y == city1)

-- Function that returns a Just value with the distance between two cities connected directly, or Nothing otherwise
distance :: RoadMap -> City -> City -> Maybe Distance
distance roadmap city1 city2 = 
    case Data.List.find matches roadmap of
        Just (_, _, d) -> Just d
        Nothing -> Nothing
    where matches (x, y, _) = (x == city1 && y == city2) || (x == city2 && y == city1)

-- Function that returns the cities adjacent to a particular city and the distance to them 
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent roadmap city = [(y, d) | (x, y, d) <- roadmap, x == city] ++ [(x, d) | (y, x, d) <- roadmap, y == city]

-- OU
{-
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadmap city = 
    [(otherCity, d) | (c1, c2, d) <- roadmap, 
                      let otherCity = if c1 == city then c2 else c1,
                      areAdjacent roadmap city otherCity]
-}

-- Function that returns the sum of all individual distances in a path between two cities in a Just value, if all the consecutive pairs of cities are directly connected by roads. Otherwise, it returns Nothing
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadmap (city1:city2:xs) = 
        case distance roadmap city1 city2 of
                Just d -> (+ d) <$> pathDistance roadmap (city2:xs)
                Nothing -> Nothing

-- Function that returns the names of the cities with the highest number of roads connecting to them, complexity O(n log n)
rome :: RoadMap -> [City]
rome roadmap =
    let allCities = [city | (c1, c2, _) <- roadmap, city <- [c1, c2]]
        groupedCities = Data.List.group $ Data.List.sort allCities
        cityCounts = [(head group, length group) | group <- groupedCities]
        maxDegree = maximum (map snd cityCounts)
    in [city | (city, degree) <- cityCounts, degree == maxDegree]

-- Function that returns a boolean indicating whether all the cities in the graph are connected in the roadmap 
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap =
    let allCities = Data.List.nub [c | (c1, c2, _) <- roadmap, c <- [c1, c2]]
    in all (\city -> length (adjacent roadmap city) == length allCities - 1) allCities

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]