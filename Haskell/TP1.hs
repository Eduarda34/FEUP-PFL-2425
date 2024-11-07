import qualified Data.List
-- import qualified Data.Array
-- import qualified Data.Bits
import Data.Time
import System.CPUTime
import Control.Monad (replicateM)
import Control.Monad
import Test.QuickCheck (Small(getSmall))
import System.IO (withFile, IOMode(..), hPutStrLn, hFlush)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.CPUTime (getCPUTime)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

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
adjacent roadmap city = Data.List.nub [(c2, d) | (c1, c2, d) <- roadmap, c1 == city] ++ Data.List.nub [(c1, d) | (c1, c2, d) <- roadmap, c2 == city]

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
lookupAdjacent adjList city = 
    case lookup city adjList of
        Just neighbors -> neighbors
        Nothing -> []

-- Helper function to get distance from adjacency list
lookupDistance :: [(City, [(City, Distance)])] -> City -> City -> Maybe Distance
lookupDistance adjList from to = 
    case lookup from adjList of
        Just neighbors -> lookup to neighbors
        Nothing -> Nothing

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
                newEntries = [(n, currentPath ++ [n], currentDist + d) | (n, d) <- neighbors, n `notElem` visited]
                newQueue = Data.List.sortOn (\(_, _, d) -> d) (queue ++ newEntries)
            in dijkstra adjList newQueue (currentCity : visited) paths

-- Helper function to find a greedy path for the TSP recursively
findGreedyPath :: [(City, [(City, Distance)])] -> City -> [City] -> Distance -> Path
findGreedyPath adjList start visited totalDistance
    | length visited == length (map fst adjList) =
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
travelSales roadMap = Data.List.minimumBy distanceCompare possiblePaths
  where
    cities = Data.List.nub $ concat [[a, b] | (a, b, _) <- roadMap] -- nub to get all unique cities
    possiblePaths = [path ++ [head path] | path <- Data.List.permutations cities] -- permutations to ensure the path returns to the start
    distanceCompare path1 path2 = case (pathDistance roadMap path1, pathDistance roadMap path2) of
        (Just d1, Just d2) -> compare d1 d2
        _ -> EQ

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Timing functions

-- Helper function to log timing results to a file, overwriting the file each time
-- Helper function to log timing results to a file
logTimingToFile :: String -> String -> IO ()
logTimingToFile label result = do
    withFile "timing_results.txt" AppendMode $ \handle -> do
        hPutStrLn handle (label ++ ": " ++ result)

-- Modified timing functions for travelSales
measureCPUTimeTravel :: String -> (RoadMap -> Path) -> RoadMap -> IO ()
measureCPUTimeTravel name func roadmap = do
    start <- getCPUTime
    let _ = func roadmap  -- Call the function
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)  -- Convert picoseconds to seconds
    let result = show diff ++ " seconds"
    putStrLn $ name ++ ": " ++ result
    logTimingToFile name result  -- Log to file

measureWallClockTimeTravel :: String -> (RoadMap -> Path) -> RoadMap -> IO ()
measureWallClockTimeTravel name func roadmap = do
    start <- getCurrentTime
    let _ = func roadmap  -- Call the function
    end <- getCurrentTime
    let diff = diffUTCTime end start
    let result = show diff ++ " seconds"
    putStrLn $ name ++ ": " ++ result
    logTimingToFile name result  -- Log to file

measureTimeMultipleRunsTravel :: String -> (RoadMap -> Path) -> RoadMap -> Int -> IO ()
measureTimeMultipleRunsTravel name func roadmap n = do
    times <- replicateM n $ do
        start <- getCurrentTime
        let _ = func roadmap  -- Call the function
        end <- getCurrentTime
        return (diffUTCTime end start)
    let avgTime = sum times / fromIntegral n
    let result = "average time over " ++ show n ++ " runs is " ++ show avgTime ++ " seconds"
    putStrLn $ name ++ ": " ++ result
    logTimingToFile name result  -- Log to file

-- Timing functions for shortestPath
measureCPUTimeShortest :: String -> (RoadMap -> City -> City -> [Path]) -> RoadMap -> City -> City -> IO ()
measureCPUTimeShortest label f roadmap startCity endCity = do
    start <- getCPUTime
    let result = f roadmap startCity endCity
    end <- getCPUTime
    let diff = fromIntegral (end - start) * 1e-12  -- Convert to seconds
    let resultStr = show diff ++ " seconds"
    putStrLn $ label ++ ": " ++ resultStr
    logTimingToFile label resultStr  -- Log to file

measureWallClockTimeShortest :: String -> (RoadMap -> City -> City -> [Path]) -> RoadMap -> City -> City -> IO ()
measureWallClockTimeShortest label f roadmap startCity endCity = do
    start <- getCurrentTime
    let result = f roadmap startCity endCity
    end <- getCurrentTime
    let diff = diffUTCTime end start
    let resultStr = show diff ++ " seconds"
    putStrLn $ label ++ ": " ++ resultStr
    logTimingToFile label resultStr  -- Log to file

measureTimeMultipleRunsShortest :: String -> (RoadMap -> City -> City -> [Path]) -> RoadMap -> City -> City -> Int -> IO ()
measureTimeMultipleRunsShortest label f roadmap startCity endCity n = do
    times <- forM [1..n] $ \_ -> do
        start <- getCPUTime
        let result = f roadmap startCity endCity
        end <- getCPUTime
        let diff = fromIntegral (end - start) * 1e-12
        return diff
    let total = sum times
    let average = total / fromIntegral n
    let resultStr = "Average Time: " ++ show average ++ " seconds"
    putStrLn $ label ++ ": " ++ resultStr
    logTimingToFile label resultStr  -- Log to file

-- Test function to measure execution times for both travelSales and shortestPath
testTiming :: RoadMap -> (City, City) -> IO ()
testTiming roadmap (startCity, endCity) = do
    putStrLn "\nTesting shortestPath function:"
    measureCPUTimeShortest "CPU Time (shortestPath)" shortestPath roadmap startCity endCity
    measureWallClockTimeShortest "Wall Clock Time (shortestPath)" shortestPath roadmap startCity endCity
    measureTimeMultipleRunsShortest "Multiple Runs (shortestPath)" shortestPath roadmap startCity endCity 10

    putStrLn "\nTesting travelSales function:"
    measureCPUTimeTravel "CPU Time (travelSales)" travelSales roadmap
    measureWallClockTimeTravel "Wall Clock Time (travelSales)" travelSales roadmap
    measureTimeMultipleRunsTravel "Multiple Runs (travelSales)" travelSales roadmap 10

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gSmallSparse :: RoadMap
gSmallSparse = [("A", "B", 5), ("B", "C", 3), ("C", "D", 4)]

gSmallDense :: RoadMap
gSmallDense = [("A", "B", 2), ("A", "C", 3), ("A", "D", 4), ("B", "C", 1), ("B", "D", 3), ("C", "D", 2)]

gMediumSparse :: RoadMap
gMediumSparse = [("A", "B", 5), ("B", "C", 3), ("C", "D", 4), ("D", "E", 6), ("E", "F", 2)]

gMediumDense :: RoadMap
gMediumDense = [("A", "B", 1), ("A", "C", 4), ("A", "D", 6), ("A", "E", 8), ("B", "C", 3), ("B", "D", 5), ("B", "E", 7), ("C", "D", 2), ("C", "E", 6), ("D", "E", 3)]

gLargeSparse :: RoadMap
gLargeSparse = [("A", "B", 5), ("B", "C", 7), ("C", "D", 9), ("D", "E", 11), ("E", "F", 13), ("F", "G", 15), ("G", "H", 17), ("H", "I", 19), ("I", "J", 21)]

gLargeDense :: RoadMap
gLargeDense = [("A", "B", 2), ("A", "C", 3), ("A", "D", 4), ("A", "E", 5), ("A", "F", 6), ("B", "C", 1), ("B", "D", 3), ("B", "E", 2), ("B", "F", 4), ("C", "D", 2), ("C", "E", 5), ("C", "F", 3), ("D", "E", 1), ("D", "F", 6), ("E", "F", 4)]

gVeryLargeDense :: RoadMap
gVeryLargeDense = [("A", "B", 1), ("A", "C", 2), ("A", "D", 3), ("A", "E", 4), ("A", "F", 5), ("A", "G", 6), ("B", "C", 1), ("B", "D", 2), ("B", "E", 3), ("B", "F", 4), ("B", "G", 5), ("C", "D", 1), ("C", "E", 2), ("C", "F", 3), ("C", "G", 4), ("D", "E", 1), ("D", "F", 2), ("D", "G", 3), ("E", "F", 1), ("E", "G", 2), ("F", "G", 1)]

-- Main function to run tests
main :: IO ()
main = do
    -- Define the list of graphs for testing
    let testGraphs = 
            [ ("Test Graph 1", gTest1)
            , ("Test Graph 2", gTest2)
            , ("Test Graph 3 (Unconnected)", gTest3)
            , ("Small Sparse Graph", gSmallSparse)
            , ("Small Dense Graph", gSmallDense)
            , ("Medium Sparse Graph", gMediumSparse)
            , ("Medium Dense Graph", gMediumDense)
            , ("Large Sparse Graph", gLargeSparse)
            , ("Large Dense Graph", gLargeDense)
            , ("Very Large Dense Graph", gVeryLargeDense)
            ]

    -- Measure and print the time taken for each graph
    -- mapM_ measureTime testGraphs

    -- Run testTiming for each graph with descriptive labels
    putStrLn "Testing timing for gTest1"
    testTiming gTest1 ("7", "8")
    putStrLn ""

    putStrLn "Testing timing for gTest2"
    testTiming gTest2 ("0", "3")
    putStrLn ""

    putStrLn "Testing timing for gTest3"
    testTiming gTest3 ("0", "1")  -- Test with an unconnected graph
    putStrLn ""

    putStrLn "Testing timing for gSmallSparse"
    testTiming gSmallSparse ("A", "D")
    putStrLn ""

    putStrLn "Testing timing for gSmallDense"
    testTiming gSmallDense ("A", "D")
    putStrLn ""

    putStrLn "Testing timing for gMediumSparse"
    testTiming gMediumSparse ("A", "F")
    putStrLn ""

    putStrLn "Testing timing for gMediumDense"
    testTiming gMediumDense ("A", "E")
    putStrLn ""

    putStrLn "Testing timing for gLargeSparse"
    testTiming gLargeSparse ("A", "J")
    putStrLn ""

    putStrLn "Testing timing for gLargeDense"
    testTiming gLargeDense ("A", "F")
    putStrLn ""

    putStrLn "Testing timing for gVeryLargeDense"
    testTiming gVeryLargeDense ("A", "G")
    putStrLn ""
