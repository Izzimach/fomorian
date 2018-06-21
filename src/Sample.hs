{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs, TypeSynonymInstances, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

module Sample where

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU
import qualified Graphics.UI.GLFW as GLFW
import Linear
import Data.Vinyl
import Data.Word (Word32)
import Graphics.VinylGL

import Data.IORef
import Data.Maybe (mapMaybe)
import Control.Monad
import Control.Lens
import Control.Exception
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import System.FilePath ((</>))


type Pos2 = '("pos2", V2 GLfloat)
type Pos3 = '("pos3", V3 GLfloat)
type VertIndex = '("index", Int)

pos2 :: SField Pos2
pos2 = SField
pos3 :: SField Pos3
pos3 = SField
vertindex :: SField VertIndex
vertindex = SField

type AppInfo = FieldRec '[  '("projectionMatrix", M44 GLfloat),
                            '("resources", ResourceMap), 
                            '("scene",SceneNode) ]

type SceneNode = FieldRec '[ '("shader", String),
                             '("shaderParameters", FieldRec '[]),
                             '("vertexBuffers", [VertexSourceData]),
                             '("children", ()) ]

data ResourceMap = Resources { shaders :: (M.Map String GLU.ShaderProgram),
                               v2Buffers :: (M.Map VertexSourceData (BufferedVertices '[Pos2], Word32)),
                               v3Buffers :: (M.Map VertexSourceData (BufferedVertices '[Pos3], Word32)),
                               indexBuffers :: (M.Map VertexSourceData (BufferObject, Word32)) }

data ResourceList = ResourceList { shaderfiles :: S.Set String,
                                   vertexfiles :: S.Set VertexSourceData }

type RenderParams = FieldRec '[ '("transformMatrix", M44 GLfloat) ]

data VertexSourceData = V2Data [V2 GLfloat]
                  | V3Data [V3 GLfloat]
                  | IndexData  [Int]
                  deriving (Ord, Eq)

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

listResources :: SceneNode -> ResourceList
listResources scenenode = listResources' scenenode (ResourceList S.empty S.empty)
  where
    listResources' scenenode resourcelist =
      let shaderfile         = rvalf #shader scenenode
          vertexbufferlist   = rvalf #vertexBuffers scenenode
          currentshaders     = shaderfiles resourcelist
          currentvertexfiles = vertexfiles resourcelist
      in
        resourcelist {
                    shaderfiles = S.insert shaderfile currentshaders,
                    vertexfiles = foldr S.insert currentvertexfiles vertexbufferlist
                  }

loadBuffers :: [VertexSourceData] -> ResourceMap -> IO ResourceMap
loadBuffers vs r = foldM loadBufferM r vs
  where
    loadBufferM racc vv@(V2Data v2) = do vb <- buildVertices2 v2
                                         let vblen = fromIntegral $ length v2
                                         return $ racc { v2Buffers = M.insert vv (vb, vblen) (v2Buffers r) }
    loadBufferM racc vv@(V3Data v3) = do vb <- buildVertices3 v3
                                         let vblen = fromIntegral $ length v3
                                         return $ racc { v3Buffers = M.insert vv (vb, vblen) (v3Buffers r) }
    loadBufferM racc vv@(IndexData i) = do ib <- buildIndices i
                                           let iblen = fromIntegral $ length i
                                           return $ racc { indexBuffers = M.insert vv (ib, iblen) (indexBuffers r) }

loadResources :: ResourceList -> ResourceMap -> IO ResourceMap
loadResources neededresources loadedresources = do
  let needshaders = S.toList $ S.difference (shaderfiles neededresources) (M.keysSet $ shaders loadedresources)
  loadingshaders  <- traverse initShader needshaders
  let newshaders = (M.union (M.fromList $ zip needshaders loadingshaders) (shaders loadedresources)) 

  loadBuffers (S.toList $ vertexfiles neededresources) (loadedresources { shaders = newshaders })

{-}  let needv2buffers = S.toList $ S.difference (vertexfiles neededresources) (M.keysSet $ v2Buffers loadedresources) 
  loadingv2buffers <- traverse buildVertices needv2buffers

  let needv3buffers = S.toList $ S.difference (vertexfiles neededresources) (M.keysSet $ v3Buffers loadedresources) 
  loadingv3buffers <- traverse buildVertices needv3buffers

  return $ Resources allshaders
                     (M.union (M.fromList $ zip needv2buffers loadingv2buffers) (v2Buffers loadedresources))
                     (M.union (M.fromList $ zip needv3buffers loadingv3buffers) (v3Buffers loadedresources))
-}

setProjectionMatrix :: IORef AppInfo -> Int -> Int -> IO ()
setProjectionMatrix s w h = do
  modifyIORef' s f
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  where
    w1 = 1.0 / (fromIntegral w)
    h1 = 1.0 / (fromIntegral h)
    scaleMatrix x y z = V4 (V4 x 0 0 0)
                           (V4 0 y 0 0)
                           (V4 0 0 z 0)
                           (V4 0 0 0 1)
    translationMatrix x y z = V4 (V4 1 0 0 x)
                                 (V4 0 1 0 y)
                                 (V4 0 0 1 z)
                                 (V4 0 0 0 1)
    finalMatrix = ((translationMatrix (-1) (-1) 0) !*! (scaleMatrix (w1*2) (h1*2) 1))
    f :: AppInfo -> AppInfo
    f app = rputf #projectionMatrix finalMatrix app

resizeWindow :: IORef AppInfo -> GLFW.WindowSizeCallback
resizeWindow s = \_ w h -> setProjectionMatrix s w h

    
initShader :: String -> IO GLU.ShaderProgram
initShader shadername = do
  GLU.loadShaderFamily $ "resources" </> "shaders" </> shadername

buildVertices2 :: [V2 GLfloat]  -> IO (BufferedVertices '[Pos2])
buildVertices2 v2s = bufferVertices $ fmap (pos2 =:=) v2s

buildVertices3 :: [V3 GLfloat] -> IO (BufferedVertices '[Pos3])
buildVertices3 v3s = bufferVertices $ fmap (pos3 =:=) v3s

buildIndices :: [Int] -> IO (BufferObject)
buildIndices idxs = GLU.bufferIndices $ fmap (fromIntegral) idxs


initWindow = do
  GLFW.init
  GLFW.defaultWindowHints
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 4)
  GLFW.windowHint (GLFW.WindowHint'Resizable False)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  Just win <- GLFW.createWindow 400 400 "Title" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  appIORef <- initAppState
  GLFW.setWindowSizeCallback win (Just $ resizeWindow appIORef)
  setProjectionMatrix appIORef 400 400
  return (win, appIORef)

initAppState :: IO (IORef AppInfo)
initAppState = do
  defaultVAO <- fmap head (genObjectNames 1)
  bindVertexArrayObject $= Just defaultVAO
  GLU.printErrorMsg "bindVAO"
  sceneGraph <- buildScene
  resources <- loadResources (listResources sceneGraph) (Resources M.empty M.empty M.empty M.empty)
  appIORef <- newIORef $ (#projectionMatrix =: identity)
                      :& (#resources =: resources)
                      :& (#scene =: sceneGraph)
                      :& RNil
  return appIORef

  
terminateWindow win = do
  GLFW.destroyWindow win
  GLFW.terminate


main :: IO ()
main = do
  (win, appIORef) <- initWindow
  renderloop win appIORef
  terminateWindow win

renderloop win appstateref = loop
  where
    loop = do
        appstate <- get appstateref
        render appstate
        GLFW.swapBuffers win
        p <- GLFW.getKey win GLFW.Key'Escape
        unless (p == GLFW.KeyState'Pressed) $ do
          GLFW.pollEvents
          windowOpen <- GLFW.windowShouldClose win
          unless (windowOpen) $ loop

render :: AppInfo -> IO ()
render appstateref = do
  GL.clearColor $= Color4 0 0 0 0
  GL.clear [GL.ColorBuffer]
  let resourceMap = rvalf #resources appstateref
  let renderparams = (#transformMatrix =: (rvalf #projectionMatrix appstateref)) :& RNil
  let scene = rvalf #scene appstateref
  renderresult <- try $ renderSceneGraph scene resourceMap renderparams
  case renderresult of
    Left e -> do
      putStrLn $ displayException (e :: SomeException)
    Right () -> return ()

renderSceneGraph :: SceneNode -> ResourceMap -> RenderParams -> IO ()
renderSceneGraph scene resources renderparams = do
  let numLines = 4 ::Integer --length $ (lineCoords appstate)
  let vBufferValues = rvalf #vertexBuffers scene
  let v2Vertices = mapMaybe (\x -> M.lookup x (v2Buffers resources)) vBufferValues
  let v3Vertices = mapMaybe (\x -> M.lookup x (v3Buffers resources)) vBufferValues
  let indexVertices = mapMaybe (\x -> M.lookup x (indexBuffers resources)) vBufferValues
  let (Just shaderdata) = M.lookup (rvalf #shader scene) (shaders resources)
  currentProgram $= Just (GLU.program shaderdata)
  GLU.printErrorMsg "currentProgram"
  setUniforms shaderdata renderparams
  mapM (enableVertices' shaderdata . fst) v2Vertices
  mapM (bindVertices . fst) v2Vertices
  mapM (enableVertices' shaderdata . fst) v3Vertices
  mapM (bindVertices . fst) v3Vertices
  mapM (\x -> bindBuffer ElementArrayBuffer $= Just (fst x)) indexVertices
  GLU.printErrorMsg "bindBuffer"
  if (length indexVertices > 0) then
    -- index arrays are Word32 which maps to GL type UnsignedInt
    drawElements Triangles (fromIntegral . snd . head $ indexVertices) GL.UnsignedInt GLU.offset0
  else 
    if (length v2Vertices > 0) then
      drawArrays Triangles 0 (fromIntegral . snd . head $ v2Vertices)
    else
      drawArrays Triangles 0 (fromIntegral . snd . head $ v3Vertices)
  GLU.printErrorMsg "drawArrays"
  return ()

