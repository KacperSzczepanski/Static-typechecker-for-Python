# Python type inferer and checker

Command `make` creates programs `main` and `mainU`. The first one is main part of the project, the second one works with simplest unification algorithm designed for Python.

Usage for both programs:
+ `./main` - reads program from standard input
+ `./main path_to_file` - reads program from file

Script `test.sh` runs all examples in `tests/` and compares results to prepared outputs from `correct_outputs/`. Script does not test unification.

To get informations inferred from functions and classes too, go to file `src/Utils.hs` and uncomment lines 56, 57 and 58.

Tests `classes/simpleInheritance.py` and `classes/thirdLevel.py` have `- 0`s in initialization functions to prevent program from using lots of resources (expected types of variables in examples remain the same). They should be removed though if you plan to test original tests.

Use `make clean` to clean temporary files and outputs of tests.