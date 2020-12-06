help:
  @just --list

# Run puzzles: `just run` will run everything; `just run 5 -2 7-9 12-` will run days up to 2, then 5, then 7-9, then everything from 12 on
run *day-specs:
  clojure -M:run {{day-specs}}

# Runs tests suite
test:
  clojure -M:test

# Prints clojure dependency tree
tree:
  clojure -Srepro -Stree
