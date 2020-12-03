help:
  @just --list

# Runs all puzzles
run *day-specs:
  clojure -M:run {{day-specs}}

# Runs tests suite
test:
  clojure -M:test

# Prints clojure dependency tree
tree:
  clojure -Srepro -Stree
