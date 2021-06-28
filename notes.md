CycleSeg n PcSet will take a fragmented interval cycle and return a list that reduces the fragmented set into the number of adjacent elements. This functions as a representation as to how concentrated the set is within a particluar interval cycle fragmentation.
- [0, 3, 6, 9] [1, _, _, _] [_, 5, _, _] --> [4, 1, 1]
- [0, _, _, 6, _, _] [1, 3, 5, _, 9, _] --> [3, 1, 1, 1]