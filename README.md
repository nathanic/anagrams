# Multi-word anagram generator in Haskell

This is a small command-line program that, given a word list file and a term, generates multi-word anagrams for the term.

## Algorithm
The core data structure is the [AnaTree](https://en.wikipedia.org/wiki/Anatree) which is also described [here](http://blog.notdot.net/2007/10/Damn-Cool-Algorithms-Part-3-Anagram-Trees) in some depth but unfortunately the images have linkrotted.

It's an N-ary tree of height `1 + length alphabet`, so in our case 27 layers.  All layers but the last layer are associated with a symbol in the alphabet.  Each alphabet node has a child indexed by the frequency of the occurence of that layer's character in any terms stored below.  At the bottom layer of the tree are terms that occur with those frequencies.  

For example, if your term is "cool", the path to that term in the tree would involve taking the 0th branch from most characters, branch 1 from `c` and `l` nodes, and branch 2 from the `o` node.

This allows us to quickly search the tree for all terms that have a given character distribution, which means we can pretty instantly come up with a single-word anagram.  To do multiple words, we have to search the tree many times, each time we with an updated search character distribution with decremented counts for any "used" characters.  Exhaustively searching the tree in this way can take hours for long terms.

In an attempt to mitigate that, I have been trying to add a random mode where we sample the space of anagram solutions without searching it exhaustively.


## Applications(?)

I wrote this to be used as a plugin in my IRC bot, to generate randomly-selected anagrams for the delight of my pals.
