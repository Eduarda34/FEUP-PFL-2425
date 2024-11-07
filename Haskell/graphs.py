import matplotlib.pyplot as plt
import numpy as np

# Function to parse the timing results file
def parse_timing_file(filename="timing_results.txt"):
    graphs = [
        "gTest1", "gTest2", "gTest3", "gSmallSparse", "gSmallDense", 
        "gMediumSparse", "gMediumDense", "gLargeSparse", "gLargeDense", "gVeryLargeDense"
    ]
    
    shortest_path_times = []
    travel_sales_times = []

    with open(filename, "r") as file:
        lines = file.readlines()

        # Debug: Print out all lines to see the structure
        print(f"Total lines in file: {len(lines)}")
        print("First few lines:", lines[:10])  # Print the first 10 lines for inspection

        for i in range(0, len(lines), 6):  # Processing each block of 6 lines (3 for shortestPath, 3 for travelSales)
            print(f"Processing lines {i} to {i+5}: {lines[i:i+6]}")  # Debug: Show the current set of lines being processed

            if i < len(lines):  # Ensure we're not exceeding the length of the lines
                # Extract CPU time for shortestPath and remove any non-numeric characters (like 's')
                shortest_path_time = float(lines[i].split(":")[1].strip().split()[0].rstrip('s'))
                shortest_path_times.append(shortest_path_time)

            if i + 3 < len(lines):  # Ensure there's a next line for travelSales times (3 lines after shortestPath)
                # Extract CPU time for travelSales and remove any non-numeric characters (like 's')
                travel_sales_time = float(lines[i + 3].split(":")[1].strip().split()[0].rstrip('s'))
                travel_sales_times.append(travel_sales_time)

    # Debug: Check lengths before returning
    print(f"Length of Graphs: {len(graphs)}")
    print(f"Length of Shortest Path Times: {len(shortest_path_times)}")
    print(f"Length of Travel Sales Times: {len(travel_sales_times)}")
    
    # Ensure we only have 10 values for each timing list
    assert len(shortest_path_times) == len(travel_sales_times) == len(graphs), \
        f"Mismatch: {len(shortest_path_times)} vs {len(graphs)} or {len(travel_sales_times)}"

    return graphs, shortest_path_times, travel_sales_times

# Parse the timing data from the file
graphs, shortest_path_times, travel_sales_times = parse_timing_file()

# Check if the lengths match
print(f"Length of Graphs: {len(graphs)}")
print(f"Length of Shortest Path Times: {len(shortest_path_times)}")
print(f"Length of Travel Sales Times: {len(travel_sales_times)}")

# Convert times to log scale for visualization
log_shortest_path_times = np.log10(shortest_path_times)
log_travel_sales_times = np.log10(travel_sales_times)

# Set up the plot
fig, ax = plt.subplots(figsize=(10, 6))

# Plotting both shortestPath and travelSales times
ax.plot(graphs, log_shortest_path_times, label="Shortest Path", marker='o')
ax.plot(graphs, log_travel_sales_times, label="Travel Sales", marker='o')

# Adding labels and title
ax.set_xlabel("Graphs")
ax.set_ylabel("Log10(CPU Time) [seconds]")
ax.set_title("CPU Time Comparison (Shortest Path vs Travel Sales)")
ax.set_xticklabels(graphs, rotation=45, ha="right")

# Add legend
ax.legend()

# Show the plot
plt.tight_layout()
plt.show()
