import sys

def read_input_file(input_file): #Function to read the input file
    graph = {} #Creating an empty dictionary to store the graph
    try:
        with open(input_file, 'r') as f: #Opening the file for reading
            for line in f: #Reading in each line of the file
                nodes = list(map(int, line.split())) #Splitting the line of the file read using whitespace into a list of integers
                node = nodes[0] #Asigns the first integer in the list to node
                neighbours = set(nodes[1:]) #Converts the remaining integers in the list into a set of neighours
                graph[node] = neighbours #Adds the node and its neighbours to the graph dictionary
    except FileNotFoundError: #Throws an error if the input file is not found
        print("Error : File " + input_file + " not found")
        sys.exit(1)
    except ValueError as e: #Handles the error case where there is a value error
        print("Error: " + e)
    return graph

def write_output_file(output_file, node_colours): #Function to write to the output file
    try:
        with open(output_file, 'w') as f: #Opening the output file for writing
            for node, colour in sorted(node_colours.items()): #Loop through each node and its colour in alphabetical order
                f.write(f"{node} {colour}\n") #Writes each node and its assigned colour to the file
    except IOError: #Handles the error case when there is an IO error
        print("Error; Unable to write to file " + output_file)
        sys.exit(1)
    
def allocate_registers(graph): #Function to allocate register colours to graph nodes
    nodes = list(graph.keys()) #Gets a list of the nodes in the graph
    nodes.sort(key=lambda n: (-len(graph[n]), n)) #Sorts the nodes by the number of neighbours and node number
    colours = {} #Creating a dictionary to store the colours assigned to each node
    for node in nodes: #Loops through each node in the sorted list
        neighbours = graph[node] #gets the neighbours of the current node
        used_colours = set(colours[n] for n in neighbours if n in colours) #Gets the set of colours used by the neighbours of the current node
        available_colours = set('ABCDEFGHIJKLMNOPQRSTUVWXYZ') - used_colours #Gets the set of colours that are not used by the neighbours of the current node
        if not available_colours: #If there are no colours available, raises an exception
            raise ValueError("Unable to allocate a colour for a node.")
        colour = min(available_colours) #Chooses the smallest available colour
        colours[node] = colour #Sets the current node the smallest available colour
    return colours

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Please use the command line in the following way: python3 compilers.py input_file output_file")
        sys.exit(1)
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    graph = read_input_file(input_file)
    node_colours = allocate_registers(graph)
    write_output_file(output_file, node_colours)