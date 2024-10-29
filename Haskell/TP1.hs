{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
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
                Just d -> fmap (d +) (pathDistance roadmap (city2:xs))
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

{-
-- Helper function to find all paths between two cities
allPaths :: RoadMap -> City -> City -> [City] -> [Path]
allPaths roadmap city1 city2 cityList = [city1:path | (nextCity, _) <- adjacent roadmap city1, nextCity `notElem` cityList, path <- allPaths roadmap nextCity city2 (nextCity:cityList)]

OU

allPaths :: RoadMap -> City -> City -> [City] -> [Path]
allPaths roadmap city1 city2 cityList =
    let nextCities = [c | (c1, c2, _) <- roadmap, c <- [c1, c2], c /= city1, c /= city2, c `notElem` cityList]
    in [city1:path | c <- nextCities, path <- allPaths roadmap c city2 (c:cityList)]
-}

-- Helper function to convert the RoadMap to an adjacency list representation locally.
toAdjList :: RoadMap -> [(City, [(City, Distance)])]
toAdjList roadmap =
    let citiesList = cities roadmap
    in [(city, adjacent roadmap city) | city <- citiesList]

--- Helper to get neighbors from adjacency list
lookupAdjacent :: [(City, [(City, Distance)])] -> City -> [(City, Distance)]
lookupAdjacent adj city = maybe [] id (lookup city adj)

-- Helper to get direct distance from adjacency list
lookupDistance :: [(City, [(City, Distance)])] -> City -> City -> Maybe Distance
lookupDistance adj city1 city2 = lookup city2 =<< lookup city1 adj

-- Use `pathDistance` to calculate the distance of a path from the adjacency list.
pathDistanceFrom :: [(City, [(City, Distance)])] -> Path -> Distance
pathDistanceFrom adjList path = sum [d | (c1, c2) <- zip path (tail path), Just d <- [lookupDistance adjList c1 c2]]

-- Dijkstra's algorithm adaptation for finding the shortest paths between two cities.
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start end
    | start == end = [[start]]
    | otherwise = dijkstra (toAdjList roadmap) [(start, [start], 0)] [] []
  where
    -- Dijkstra's function
    dijkstra adjList [] _ paths = paths
    dijkstra adjList ((currentCity, currentPath, currentDist):queue) visited paths
        | currentCity == end =
            let shortestDist = case paths of
                                    [] -> currentDist
                                    _  -> minimum (map (pathDistanceFrom adjList) paths)
                newPaths = if currentDist == shortestDist then currentPath : paths else paths
            in dijkstra adjList queue (currentCity : visited) newPaths
        | otherwise =
            let neighbors = case lookup currentCity adjList of
                                Just n -> n
                                Nothing -> []
                newEntries = [(n, currentPath ++ [n], currentDist + d) |
                                (n, d) <- neighbors, n `notElem` visited]
                newQueue = Data.List.sortOn (\(_, _, d) -> d) (queue ++ newEntries)
            in dijkstra adjList newQueue (currentCity : visited) paths

{- 
-- OU (usando as funções ja feitas)

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap city1 city2
    | city1 == city2 = [[city1]]
    | not (areAdjacent roadmap city1 city2) = []
    | otherwise = [path1] ++ shortestPaths
    where
        paths = allPaths roadmap city1 city2 []
        shortestPaths = [path | path <- paths, length path == length (shortestPath roadmap (head path) city2) + 1]
        path1 = shortestPath roadmap city1 city2
        shortestPath roadmap city1 city2
            | city1 == city2 = [city1]
            | not (areAdjacent roadmap city1 city2) = []
            | otherwise = [city1:shortestPath roadmap city2 city2]
-}

-- Solves the TSP using a brute-force approach and adjacency list for efficiency
travelSales :: RoadMap -> Path
travelSales roadmap =
    let adjList = toAdjList roadmap
        allCities = map fst adjList

        -- Recursive helper to find shortest TSP path from a specific city
        findShortestPath :: City -> [City] -> Distance -> Path
        findShortestPath start visitedPath totalDistance
            | length visitedPath == length allCities =
                case lookupDistance adjList (last visitedPath) start of
                    Just returnDist -> visitedPath ++ [start]
                    Nothing -> []
            | otherwise =
                let neighbors = lookupAdjacent adjList (last visitedPath)
                    validPaths = [(nextCity, dist) | (nextCity, dist) <- neighbors, nextCity `notElem` visitedPath]
                    paths = [findShortestPath start (visitedPath ++ [nextCity]) (totalDistance + dist) | (nextCity, dist) <- validPaths]
                    -- Filter out any invalid paths and sort by total path distance
                    validSortedPaths = Data.List.sortOn (pathDistanceFrom adjList) (filter (not . null) paths)
                in if null validSortedPaths then [] else head validSortedPaths

    in if not (isStronglyConnected roadmap) then []
       else case allCities of
           [] -> []
           (start:_) -> findShortestPath start [start] 0

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]