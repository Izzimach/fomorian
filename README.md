Fomorian is a Haskell library that lets you produce 3D graphics by manipulating a scene graph using either OpenGL or Vulkan.

for an example here is a test scene graph from [the Sample.hs file](src/fomorian/Sample.hs)

```haskell
testScene3d :: SceneGraph NeutralSceneTarget DefaultDrawFrameParams 
testScene3d = neutral3DSceneRoot $
                perspectiveProject config $
                  autoAspect $
                    cameraLookAt (V3 5 10 0) (V3 0 0 0) (V3 0 0 1) $ 
                      group [
                        scale3d (V3 2 2 2) someCube,
                        translate3d (V3 3 0 0) $ spin3d (V3 0.7071 0.7071 0) 2 someCube
                        ]
  where
    config = PerspectiveProject  1.2 {-fov-} 1.0 {-aspect-} 0.1 {-near plane-} 1000 {-far plane-}

    someCube :: (DrawReq NeutralSceneTarget dr) => SceneGraph NeutralSceneTarget dr
    someCube = wavefrontMesh "unlit3d" "testcube.obj" ["sad-crab.png"]
```

This produces the display:
![Sample pic](fomorianpic1.jpg)

to run the example install stack then:

```
stack build
stack exec example
```
