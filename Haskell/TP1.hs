{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits
import Data.Time
import System.CPUTime
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as POSIX
import Control.Monad (replicateM)

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

-- Function that returns the sum of all individual distances in a path between two cities in a Just value, if all the consecutive pairs of cities are directly connected by roads. Otherwise, it returns Nothing
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadmap (city1:city2:xs) =
        case distance roadmap city1 city2 of
                Just d -> fmap (d +) (pathDistance roadmap (city2:xs))
                Nothing -> Nothing

-- Function that returns the names of the cities with the highest number of roads connecting to them
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

-- Helper function to convert the RoadMap to an adjacency list representation locally for better efficiency.
toAdjList :: RoadMap -> [(City, [(City, Distance)])]
toAdjList roadmap =
    let citiesList = cities roadmap
    in [(city, adjacent roadmap city) | city <- citiesList]

--- Helper function to get neighbors from adjacency list
lookupAdjacent :: [(City, [(City, Distance)])] -> City -> [(City, Distance)]
lookupAdjacent adj city = maybe [] id (lookup city adj)

-- Helper function to get direct distance from adjacency list
lookupDistance :: [(City, [(City, Distance)])] -> City -> City -> Maybe Distance
lookupDistance adj city1 city2 = lookup city2 =<< lookup city1 adj

-- Helper function to calculate the distance of a path from the adjacency list.
pathDistanceFrom :: [(City, [(City, Distance)])] -> Path -> Distance
pathDistanceFrom adjList path = sum [d | (c1, c2) <- zip path (tail path), Just d <- [lookupDistance adjList c1 c2]]

-- Function that computes all shortest paths connecting two cities given as input. It uses a Dijkstra's algorithm adaptation to find the shortest paths between two cities for better efficiency in bigger graphs.
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start end
    | start == end = [[start]]
    | otherwise = dijkstra (toAdjList roadmap) [(start, [start], 0)] [] []
  where
    -- Dijkstra's function adaptation
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

-- Helper function to find a greedy path for the TSP recursively
findGreedyPath :: [(City, [(City, Distance)])] -> City -> [City] -> Distance -> Path
findGreedyPath adjList start visited totalDistance
    | length visited == length adjList =
        case lookupDistance adjList (last visited) start of
            Just returnDist -> visited ++ [start]
            Nothing -> []
    | otherwise =
        let current = last visited
            neighbors = lookupAdjacent adjList current
            nextOptions = [(nextCity, dist) | (nextCity, dist) <- neighbors, nextCity `notElem` visited]
            sortedNext = Data.List.sortOn snd nextOptions
        in case sortedNext of
            (nextCity, dist):_ -> findGreedyPath adjList start (visited ++ [nextCity]) (totalDistance + dist)
            [] -> []

-- Function that returns a possible solution for the TSP. It uses a greedy approach and an adjacency list for better efficiency
travelSales :: RoadMap -> Path
travelSales roadmap = 
    let adjList = toAdjList roadmap
        citiesList = map fst adjList
    in case citiesList of
        [] -> []
        (start:_) -> findGreedyPath adjList start [start] 0

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

-- Testing stuff

gSmall :: RoadMap
gSmall = [("A", "B", 1), ("A", "C", 2), ("B", "C", 1)]

gMedium :: RoadMap
gMedium = [("0", "1", 10), ("0", "2", 15), ("0", "3", 20), ("1", "2", 35), ("1", "3", 25), ("2", "3", 30)]

gLarge :: RoadMap
gLarge = [("0", "1", 1), ("0", "2", 2), ("0", "3", 3), ("1", "4", 4), ("2", "4", 5), ("3", "4", 6), ("1", "2", 7), ("1", "3", 8), ("2", "3", 9)] ++ 
          [("A" ++ show i, "A" ++ show (i + 1), i) | i <- [1..99]]

gVeryLarge :: RoadMap
gVeryLarge = [("0", "1", 1), ("0", "2", 2), ("0", "3", 3), ("1", "4", 4), ("2", "4", 5), ("3", "4", 6), ("1", "2", 7), ("1", "3", 8), ("2", "3", 9)] ++ 
              [("A" ++ show i, "A" ++ show (i + 1), i) | i <- [1..999]]

-- Timing functions
measureCPUTime :: String -> (RoadMap -> Path) -> RoadMap -> IO ()
measureCPUTime name func roadmap = do
    start <- getCPUTime
    let _ = func roadmap  -- Call the function
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)  -- Convert picoseconds to seconds
    putStrLn $ name ++ ": " ++ show diff ++ " seconds"

measureWallClockTime :: String -> (RoadMap -> Path) -> RoadMap -> IO ()
measureWallClockTime name func roadmap = do
    start <- getCurrentTime
    let _ = func roadmap  -- Call the function
    end <- getCurrentTime
    let diff = diffUTCTime end start
    putStrLn $ name ++ ": " ++ show diff ++ " seconds"

measureTimeMultipleRuns :: String -> (RoadMap -> Path) -> RoadMap -> Int -> IO ()
measureTimeMultipleRuns name func roadmap n = do
    times <- replicateM n $ do
        start <- getCurrentTime
        let _ = func roadmap  -- Call the function
        end <- getCurrentTime
        return (diffUTCTime end start)
    let avgTime = sum times / fromIntegral n
    putStrLn $ name ++ ": average time over " ++ show n ++ " runs is " ++ show avgTime ++ " seconds"

-- Test function to measure execution times
testTiming :: RoadMap -> IO ()
testTiming roadmap = do
    putStrLn "Testing travelSales function:"
    measureCPUTime "CPU Time" travelSales roadmap
    measureWallClockTime "Wall Clock Time" travelSales roadmap
    measureTimeMultipleRuns "Multiple Runs" travelSales roadmap 10

measureTime :: (String, RoadMap) -> IO ()
measureTime (label, roadmap) = do
    start <- POSIX.getPOSIXTime
    let result = travelSales roadmap
    end <- POSIX.getPOSIXTime
    let diff = end - start
    -- Measure printing time
    printStart <- POSIX.getPOSIXTime
    putStrLn $ label ++ ": " ++ show result ++ " (Time: " ++ show diff ++ " seconds)"
    printEnd <- POSIX.getPOSIXTime
    let printDiff = printEnd - printStart
    putStrLn $ "Printing Time: " ++ show printDiff ++ " seconds"

-- Main function to run tests
main :: IO ()
main = do
    -- Define small, medium, large, and very large graphs
    let smallGraph = [("A", "B", 1), ("B", "C", 2), ("C", "A", 3)]
    let mediumGraph = [("A", "B", 1), ("A", "C", 2), ("B", "C", 1), ("C", "D", 4), ("D", "E", 2), ("E", "F", 1), ("F", "A", 5)]
    let largeGraph = [("City" ++ show i, "City" ++ show j, (i + j)) | i <- [1..100], j <- [1..100], i /= j]
    let largerGraph1 = [("City" ++ show i, "City" ++ show j, (i + j)) | i <- [1..150], j <- [1..150], i /= j]
    let largerGraph2 = [("City" ++ show i, "City" ++ show j, (i + j)) | i <- [1..200], j <- [1..200], i /= j]
    let largerGraph3 = [("City" ++ show i, "City" ++ show j, (i + j)) | i <- [1..250], j <- [1..250], i /= j]
    
    -- Measure and print the time taken for each graph
    mapM_ measureTime
        [ ("Small Graph", smallGraph)
        , ("Medium Graph", mediumGraph)
        , ("Large Graph", largeGraph)
        , ("Larger Graph 1 (100 Cities)", largerGraph1)
        , ("Larger Graph 2 (150 Cities)", largerGraph2)
        , ("Larger Graph 3 (200 Cities)", largerGraph3)
        ]

{-
    testTiming gTest1
    testTiming gTest2
    testTiming gTest3
    testTiming gSmall
    testTiming gMedium
    testTiming gLarge
    testTiming gVeryLarge
    testTiming smallGraph
    testTiming mediumGraph
    testTiming largeGraph
    testTiming largerGraph1
    testTiming largerGraph2
    testTiming largerGraph3
-}