Better project for Ocaml project on Ford-Fulkerson. The min-cost max-flow algorithm implemented is the Busacker-Gowen algorithm.

This branch features 2 versions of the project : 
* The basic version to test the algorithm with any graph having a cost and a capacity for every edge.  
The input file's format is almost the same as the one for the acceptable project (see gfile.ml and graphs/graph1). 

  
* The advanced version to test bipartite matching problems from a certain input file's format (see BPgfile.ml and graphs/graph2).


A makefile provides some useful commands:
 - `make build` to compile an algorithm which will accept an advanced file entry. This creates an ftest_advanced.native executable.
 - `make advanced` to compile an algorithm which will accept an advanced file entry. This creates an ftest_advanced.native executable.
 - `make basic` to compile an algorithm which will accept a basic file entry. This creates an ftest_basic.native executable.
 - `make demo_advanced` to run the `ftest_advanced` program with some arguments
 - `make demo_basic` to run the `ftest_basic` program with some arguments
 - `make format` to indent the entire project
 - `make edit` to open the project in VSCode
 - `make clean` to remove build artifacts on the ftest_advanced.native file
 - `make clean_advanced` to remove build artifacts on the ftest_advanced.native file
 - `make clean_basic` to remove build artifacts on the ftest_basic.native file  
 

You can also test the 2 versions individually with any graph of your choice as long as the file you use follows the appropriate format :
- The command to test the basic version is "./ftest_basic.native **[path_input_file] [path_output_file] [source] [sink]**"  
- The command to test the advanced version is "./ftest_advanced.native **[path_input_file] [path_output_file]**"  
**[path_input_file]** is the path of your file containing the graph's data.  
**[path_output_file]** is the file's path where the results will be stored after the algorithm is executed. **It does not have to exist prior to running the command.**  
**[source]** is the id of the source in your graph (a number)  
**[sink]** is the id of the sink in your graph (a number)

remarks for improvement: 

- always work on integers rather than floats because of the round up than can lead to a huge ga between values
- biased choice : 
        reflect on people who choose only one option out of three, will they be advantaged or on the contrary, left behind
        what do we want to achieve first, the maximum people or attribute the maximum first choices ?
- small random cost to everyone in order to avoid biased choice from one roud of attribution to another
        
                  



