# Haskell interpreter with simple parsing

This is an example of how you might build the parse tree from the
galaxy.txt file by just splitting the line into whitespace-separated
tokens.

The parser returns an AlienCode data object and the list of tokens still
remaining. The basic idea is that when you see AP you recursively call
the parse routine to get the left child of AP. Then you recursively
call it again with the list of tokens returned by the first call to
get the right child.

It keeps track of the arity of functions and when it evaluates an Apply
node, it either executes the function if all the parameters are present
or it returns a PartialFunc node that contains the parameters so far.

