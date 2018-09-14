Fomorian is a Haskell library that lets you produce OpenGL graphics by
manipulating a scene graph.

current proof-of-concept data for a simple scene:

```haskell
testScene = ortho2DView $ group [
              translate2d (0,0) $ simpleSquare "owl.png",
              translate2d (100,100) $ simpleSquare "sad-crab.png"
              ]
```

to run the exampleinstall stack then:

```
stack build
stack exec example
```
