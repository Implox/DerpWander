DerpWander
==========

Simulates the evolution of Derps - creatures which wander about in search of food - using a genetic algorithm and probabilistic action sampling.

Run using DerpStart.bat, which automatically inputs worldgen parameters, allowing for tweaking of values without needing to recompile

Command line parameters for debug mode are set under project>properties>debug>DerpStart.bat is automatically copied into the build directory

An example set of parameters are: 128 96 25 "Fastest" 0.60 0.60 0.20 3 "Random" 0.50 "Clumped" "Nearby"

Argument explanations

1. The width of the world in cells (int).
2. The height of the world in cells (int).
3. The number of pairs of derps in the world (int).
4. The speed at which the world is simulated, as selected from a set of options (string).
5. The probability of mutation for each genome (double).
6. The probability of mutation for each codon (double).
7. The probability that a given codon from the dominant partner will be selected over the subordinate partner's codon (double).
8. The number of states in each derp's brain (int).
9. The pattern in which derps spawn at the beginning of each generation, as selected from a set of options (string).
10. The probability of a given plant respawning somewhere else in the world upon its consumption (double).
11. The pattern in which plants spawn at the beginning of each generation, as selected from a set of options (string).
12. The pattern in which plants respawn after being eaten, as selected from a set of options (string).
13. The name of the file to which all logging data is written (optional string).
