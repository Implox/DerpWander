DerpWander
==========

Simulates the evolution of Derps - creatures which wander about in search of food - using a genetic algorithm and probabilistic action sampling.

Run using DerpStart.bat, which automatically inputs worldgen parameters, allowing for tweaking of values without needing to recompile

Command line parameters for debug mode are set under project>properties>debug DerpStart.bat is automatically copied into the build directory

An example set of parameters are: 128 96 25 "Fastest" 0.60 0.60 0.20 3 "Random" 0.50 "Clumped" "Nearby"

Argument explanations

1. (int): width of world
2. (int): height of world
3. (int): derp count
4. (string): world gen speed from a list of options
5. (double): genome mutation threshold
6. (double): codon mutation threshold
7. (double): dominance threshold
8. (int): derp brain state count
9. (string): derp spawning option
10. (double): plant respawn threshold
11. (string): plant growth pattern
12. (string): plant respawn options
