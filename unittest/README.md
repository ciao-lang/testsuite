To run tests, follow steps below:

```
?- use_module(library(unittest)).
?- run_tests(unittest_tests, [], [check, briefcompare]).
```

You can check the differences with:

```
?- run_tests(unittest_tests, [], [compare]).
```

To save the current results execute:

```
?- run_tests(unittest_tests, [], [save]).
```

