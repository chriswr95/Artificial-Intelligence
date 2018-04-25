# Heuristic #
Cost: number of turns to goal state
Heuristic: one turn per state not geographically reachable from tail of list.
Idea: Can solve at least one adjascency per turn.
Note: Have to start by fixing one of the end states (states with only one adjascency) to the head or tail of the list if there are any. Otherwise could build out a list almost entirely adjascent but very far from goal state in terms of number of swaps. For example (Oregon, California, Nevada, Washinton).

