{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-} {- oh no -}
{-# LANGUAGE ViewPatterns #-}



module Fomorian.OpenGL.OpenGLResources where



import Data.Row
import Data.Row.Variants (view)
import Data.Functor.Foldable

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Foreign.Ptr (nullPtr, plusPtr)
import System.FilePath

import Linear

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.GL.VertexArrays (VertexArrayDescriptor(..))
import qualified Graphics.GLUtil as GLUtil

import STMLoader.LoadUnload

import Fomorian.SceneNode
import Fomorian.NeutralSceneTarget
import Fomorian.SceneResources

data OpenGLTarget

type instance (InvokeReq OpenGLTarget ir) = (HasType "vertexarray" (FilePath, DataSource BasicDataSourceTypes) ir,
                                               HasType "textures" [DataSource BasicDataSourceTypes] ir)
type instance (DrawReq OpenGLTarget dr) = (HasType "modelMatrix" (M44 Float) dr,
                                               HasType "viewMatrix" (M44 Float) dr,
                                               HasType "projectionMatrix" (M44 Float) dr)

neutralToGLTargetAlg :: SceneGraphF NeutralSceneTarget dr (SceneGraph OpenGLTarget dr) -> SceneGraph OpenGLTarget dr
neutralToGLTargetAlg (InvokeF x) =
  let combinedVAO = (x .! #shader, x .! #geometry)
  in Invoke $   (#vertexarray .== combinedVAO)
             .+ (#textures    .== (x .! #textures))
neutralToGLTargetAlg (GroupF xs) = Group xs
neutralToGLTargetAlg (TransformerF f gr) = Transformer f (neutralToGLTarget gr)

neutralToGLTarget :: SceneGraph NeutralSceneTarget dr -> SceneGraph OpenGLTarget dr
neutralToGLTarget = cata neutralToGLTargetAlg

-- normally we can just load each resource separately. However,
-- a specific binding of vertex attributes is dependent on the shader we use. Each
-- combination of shader and vertex attributes needs a separate binding and a separate
-- vertex array object (VAO). We need to track the VAO and use it when drawing.
-- To track VAOs we use the 'BundledSource' constructor to specify a specific combination
-- of shader and vertex data. The vertex attributes are configured for that specific combination.
type GLDataSourceTypes = BasicDataSourceTypes .+
  ("vertexarray"     .== (FilePath, DataSource BasicDataSourceTypes))

newtype GLDataSources = GLDataSources (S.Set (DataSource GLDataSourceTypes))
  deriving (Eq, Show)
  deriving (Monoid,Semigroup) via S.Set (DataSource GLDataSourceTypes)

newtype OpenGLResources = OpenGLResources (M.Map (DataSource GLDataSourceTypes) (Resource GLResourceTypes))

-- | Given a scene node returns the resources used
oglResourcesAlgebra :: SceneGraphF OpenGLTarget dr GLDataSources -> GLDataSources
oglResourcesAlgebra (InvokeF x) =
  let geo = x .! #vertexarray
      txs = fmap bumpTex (x .! #textures)
  in
    GLDataSources $ S.fromList ([DataSource $ IsJust #vertexarray geo] ++ txs)
  where
    bumpTex (DataSource t) = DataSource $ diversify @("vertexarray" .== (FilePath, DataSource BasicDataSourceTypes)) t
oglResourcesAlgebra (GroupF cmds) = foldl (<>) mempty cmds
oglResourcesAlgebra (TransformerF _ gr) = oglResourcesScene gr

oglResourcesScene :: SceneGraph OpenGLTarget dr -> GLDataSources
oglResourcesScene sg = cata oglResourcesAlgebra sg


-- | A single source of vertex stream data. Examples are:
--    - Vertex 3d positions: @GLVertexSource <bufferobject> (VertexArrayDescriptor 3 Float 0 nullPtr)@
--    - UV Texture coordinate: @GLVertexSource <bufferobject> (VertexArrayDescript 2 Float 0 nullPtr)@
--   Typically this will get routed to a specific vertex attribute in the shader with a specific name.
data GLVertexSource = GLVertexSource String GL.BufferObject (VertexArrayDescriptor Float)
  deriving (Eq, Show)

-- | This contains multiple vertex sources, with each source mapped to a certain named
--   shader attribute. An example would be keys "pos3" "UV" and "normal" mapping to the
--   various 'GLVertexSource' values for those vertex attributes.
newtype GLVertexAttributes = GLVertexAttributes [GLVertexSource]
  deriving (Eq, Show)

vertexAttributes :: [GLVertexSource] -> GLVertexAttributes
vertexAttributes vs = GLVertexAttributes vs

type OpenGLGeometry = GeometryResource GL.BufferObject GL.BufferObject (VertexArrayDescriptor Float)

type GLResourceTypes =
     ("vertexBuffer" .==  GeometryResource GL.BufferObject GL.BufferObject (VertexArrayDescriptor Float))
  .+ ("shaderProgram" .== GLUtil.ShaderProgram)
  .+ ("textureObject" .== GL.TextureObject)
  .+ ("boundVertices" .== (GL.VertexArrayObject, GLUtil.ShaderProgram, GeometryResource GL.BufferObject GL.BufferObject (VertexArrayDescriptor Float)))
  
computeGLDependencies :: DataSource GLDataSourceTypes -> IO (Set (DataSource GLDataSourceTypes))
computeGLDependencies (DataSource (view #vertexarray -> Just bv)) = 
  let sp  = DataSource (IsJust #shaderPath (fst bv))
      vd = bumpVert (snd bv)
  in return $ S.fromList [ sp, vd ]
  where
    bumpVert :: DataSource BasicDataSourceTypes -> DataSource GLDataSourceTypes
    bumpVert (DataSource vd) = DataSource (diversify @("vertexarray" .== (FilePath, DataSource BasicDataSourceTypes)) vd)
computeGLDependencies _ = return S.empty

loadGLResource :: DataSource GLDataSourceTypes -> Map (DataSource GLDataSourceTypes) (Resource GLResourceTypes) -> IO (Resource GLResourceTypes)
loadGLResource (DataSource r) deps = 
  case trial r #vertexarray of
    Right y -> loadBoundVertices y deps
    Left x -> loadBasicGLResource (DataSource x)

unloadGLResource :: ResourceInfo (DataSource GLDataSourceTypes) (Resource GLResourceTypes) -> IO ()
unloadGLResource (ResourceInfo _ (Resource r) _) = switch r $
     (#vertexBuffer      .== unloadVertexBuffer)
  .+ (#shaderProgram     .== unloadShaderProgram)
  .+ (#textureObject     .== unloadTextureObject)
  .+ (#boundVertices     .== unloadBoundVertices)
  where
     -- iBuf is a maybe, so use mapM to delete if it's a 'Just'
    unloadVertexBuffer (GeometryResource vBuf iBuf _ _) = do GL.deleteObjectName vBuf; mapM_ GL.deleteObjectName iBuf
    unloadShaderProgram p = GL.deleteObjectName (GLUtil.program p)
    unloadTextureObject o = GL.deleteObjectName o
    unloadBoundVertices (vao,_s,_va) = GL.deleteObjectName vao     -- s and va are resources that get unloaded separately

loadBoundVertices :: (FilePath, DataSource BasicDataSourceTypes) -> Map (DataSource GLDataSourceTypes) (Resource GLResourceTypes) -> IO (Resource GLResourceTypes)
loadBoundVertices (fp, vs) deps =
  let sp = M.lookup (DataSource (IsJust #shaderPath fp)) deps
      vd = M.lookup (bumpVert vs) deps
  in case (sp, vd) of
       (Just (Resource s), Just (Resource v)) ->
         case (trial s #shaderProgram, trial v #vertexBuffer) of
           (Right a, Right b) -> generateBoundVertices a b
           (_,_)              -> error "argh"
       (_,_)            -> error "Argh"
  where
    bumpVert :: DataSource BasicDataSourceTypes -> DataSource GLDataSourceTypes
    bumpVert (DataSource vd) = DataSource (diversify @("vertexarray" .== (FilePath, DataSource BasicDataSourceTypes)) vd)


generateBoundVertices :: GLUtil.ShaderProgram -> GeometryResource GL.BufferObject GL.BufferObject (VertexArrayDescriptor Float) -> IO (Resource GLResourceTypes)
generateBoundVertices s v = do
  [vao] <- GL.genObjectNames 1
  putStrLn $ "vao: " ++ show vao
  let vSources = M.mapWithKey (\k a -> GLVertexSource k (vBuffer v) a) (attributeMap v)
  GL.bindVertexArrayObject $= Just vao
  -- look up each vertex attribute in the shader to find it's index
  mapM_ (bindVertexSource s) vSources
  GL.bindVertexArrayObject $= Nothing
  return $ Resource $ IsJust #boundVertices (vao, s, v)

-- | Given a 'GLVertexSource' checks to see if the shader uses a vertex attribute with
--   the same name as the 'GLVertexSource' and if so, binds it to the appropriate attribute.
bindVertexSource :: GLUtil.ShaderProgram -> GLVertexSource -> IO ()
bindVertexSource (GLUtil.ShaderProgram attribs _uniforms _prog) (GLVertexSource name bufferObj vad) =
  do
    -- look up the vertex source name to see if it is in the list of attributes for this program
    case (M.lookup name attribs) of
      Nothing -> return () -- this shader doesn't use this attribute
      Just (attribIndex,_) ->
        do 
           putStrLn $ "attrib: " ++ (show attribIndex) ++ " bufferObject: " ++ (show bufferObj)
           GL.bindBuffer GL.ArrayBuffer $= Just bufferObj
           GL.vertexAttribArray attribIndex $= GL.Enabled
           GL.vertexAttribPointer attribIndex $= (GL.ToFloat, vad)
           return ()


loadBasicGLResource :: DataSource BasicDataSourceTypes -> IO (Resource GLResourceTypes)
loadBasicGLResource (DataSource (view #shaderPath -> Just sp)) =
  do s <- GLUtil.simpleShaderProgram
            ("./resources" </> "shaders" </> sp ++ ".vert")
            ("./resources" </> "shaders" </> sp ++ ".frag")
     return $ Resource (IsJust #shaderProgram s)
loadBasicGLResource (DataSource (view #texturePath -> Just tp)) =
  do v <- GLUtil.readTexture ("./resources" </> "textures" </> tp)
     case v of
        Left err -> error ("Error loading texture " ++ tp ++ ": " ++ err)
        Right obj -> do GL.textureFilter GL.Texture2D $= ((GL.Nearest,Nothing), GL.Nearest)
                        GLUtil.texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
                        return $ Resource $ IsJust #textureObject obj
loadBasicGLResource ds = 
  do (Resource rs) <- loadBasicData ds
     --putStrLn $ "Loading thing " ++ show rs
     switch rs $
         (#vertexPositions .== loadGLVertexPositions)
      .+ (#vertexData      .== loadGLVertexData)
      .+ (#shaderBytes     .== undefined)
      .+ (#textureBytes    .== undefined)

attributesToGL :: VertexAttribute -> VertexArrayDescriptor Float
attributesToGL (VertexAttribute comp dType str offs) =
  VertexArrayDescriptor
    (fromIntegral comp)
    (convertDataType dType)
    (fromIntegral str)
    (plusPtr nullPtr offs)
  where
    convertDataType VertexFloat = GL.Float
    convertDataType VertexInt = GL.Int

loadGLVertexPositions :: GeometryResource [V3 Float] [Int] VertexAttribute -> IO (Resource GLResourceTypes)
loadGLVertexPositions (GeometryResource vs ix vc a) = do
  vBuf <- GLUtil.makeBuffer GL.ArrayBuffer vs
  -- we use 'traverse' since ix is a Maybe. So this makes an index buffer if ix is a Just
  iBuf <- traverse (\x -> GLUtil.makeBuffer GL.ElementArrayBuffer (fmap fromIntegral x :: [GLUtil.Word32])) ix
  let attr = M.map attributesToGL a
  return $ Resource $ IsJust #vertexBuffer (GeometryResource vBuf iBuf vc attr)

loadGLVertexData :: GeometryResource [Float] [Int] VertexAttribute -> IO (Resource GLResourceTypes)
loadGLVertexData (GeometryResource vs ix vc a) = do
  vBuf <- GLUtil.makeBuffer GL.ArrayBuffer vs
  iBuf <- traverse (\x -> GLUtil.makeBuffer GL.ElementArrayBuffer (fmap fromIntegral x :: [GLUtil.Word32])) ix
  let attr = M.map attributesToGL a
  return $ Resource $ IsJust #vertexBuffer (GeometryResource vBuf iBuf vc attr)


type GLResourceError = String

loaderGLConfig :: LoadUnloadCallbacks (DataSource GLDataSourceTypes) (Resource GLResourceTypes) GLResourceError
loaderGLConfig = LoadUnloadCallbacks {
  loadResource = loadGLResource,
  unloadResource = unloadGLResource,
  findDependencies = computeGLDependencies,
  processException = const Drop
  }


-- | Given a scene and a set of already-loaded resources, synchronously loads and unloads resources to match resources needed for the scene.
loadOpenGLResourcesScene :: SceneGraph OpenGLTarget dr -> ResourcesMap (DataSource GLDataSourceTypes) (Resource GLResourceTypes) -> IO (ResourcesMap (DataSource GLDataSourceTypes) (Resource GLResourceTypes))
loadOpenGLResourcesScene sg lr =
  do
    let (GLDataSources needsResources) = oglResourcesScene sg
    syncNewResources loaderGLConfig lr needsResources




