Remorhaz is a Haskell library that lets you produce OpenGL graphics by
manipulating a scene graph.

current proof-of-concept code:

```haskell
buildScene :: IO (SceneNode)
buildScene = do
  return $   (#shader =: "linez")
          :& (#shaderParameters =: RNil)
          :& (#vertexBuffers =: [
                                 V2Data [V2 10 10, V2 100 10, V2 10 100, V2 100 100],
                                 IndexData [0,1,2, 2,1,3]
                                ])
          :& (#children =: ())
          :& RNil
```

to run install stack then:

```
stack build
stack exec remorhaz
```
