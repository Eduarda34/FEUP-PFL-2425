# PFL - Project 1

T16_G07
---
Maria Eduarda Pacheco Mendes Araújo - up202004473 --> percentagem

Miguel Ângelo Pacheco Valente - up201704608 --> percentagem

---
## General Description
In this project we're asked to implement the functions requested, testing the quality and efficiency of the code.

---

## shortestPath function

The **shortestPath** funciton is an adaptation of **Dijkstra's algorithm** to find the shortest path between two cities (*start* and *end*) in a *RoadMap*.


### Data Structures:
- **City** --> *String* --> City name
- **Path** --> *List* --> Represents a path
- **Distance** --> *Int* --> Distance between cities
- **RoadMap** --> *List of tuples* --> Represents direct connections between cities with the respective distance

### Helper Funtions:
- **cities** --> Extracts all unique cities from *RoadMap*
- **areAdjacent** and **distance** --> Checks adjacency between two cities and retrieves the distance
- **adjacent** --> Returns all cities directly connected to a specific city along with their respective distances
- **toAdjList** --> Converts the *RoadMap* into an adjacency list, which facilitates Dijkstra's algorithm
- **lookupAdjacent** and **lookupDistance** --> Helps access information about direct connections and distances between cities from the adjacency list

---

## travelSales function

The **travelSales** function provides a solution to the  **Traveling Salesman Problem** (*TSP*) by using *greedy algorithm*.

**TSP** aims to find the shortest possible route that visits every city exactly once and returns to the starting city.

### Data Structures:
- **City** --> *String* --> City name
- **Path** --> *List* --> Represents a path
- **Distance** --> *Int* --> Distance between cities
- **RoadMap** --> *List of tuples* --> Represents direct connections between cities with the respective distance

### Helper Functions:
- **toAdjList** --> Converts the *RoadMap* into an adjacency list format making it easier to access neighboring cities and their distances
- **lookupAdjacent** --> Finds all cities directly connected to a given city in the adjacency list.
- **lookupDistance** --> Retrieves the distance between two cities from the adjacency list