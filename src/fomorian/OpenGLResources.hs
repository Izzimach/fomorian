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



module Fomorian.OpenGLResources where

import GHC.Generics

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async.Pool

import Data.Maybe (isJust)
import Data.Row
import Data.Row.Variants (view)
import Data.Functor.Foldable
import Data.Foldable (find)
import qualified Data.Set as S
import qualified Data.Map as M

import Foreign.Storable (Storable, sizeOf)
import Foreign.Ptr (nullPtr, plusPtr)
import System.FilePath

import Linear

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.GL.VertexArrays (VertexArrayDescriptor(..))
import qualified Graphics.GLUtil as GLUtil

import LoadUnload

import Fomorian.SceneNode
import Fomorian.SceneResources
import Fomorian.ProcessWavefront (OBJBufferRecord, loadWavefrontOBJFile)

data OpenGLTarget

type instance (InvokeReq OpenGLTarget sreq) = (HasType "geometry" (FilePath, DataSource BasicDataSourceTypes) sreq,
                                               HasType "textures" [DataSource BasicDataSourceTypes] sreq)
type instance (DrawReq OpenGLTarget dreq) = (HasType "modelMatrix" (M44 Float) dreq,
                                               HasType "viewMatrix" (M44 Float) dreq,
                                               HasType "projectionMatrix" (M44 Float) dreq)



-- normally we can just load each resource separately. However,
-- a specific binding of vertex attributes is dependent on the shader we use. Each
-- combination of shader and vertex attributes needs a separate binding and a separate
-- vertex array object (VAO). We need to track the VAO and use it when drawing.
-- To track VAOs we use the 'BundledSource' constructor to specify a specific combination
-- of shader and vertex data. The vertex attributes are configured for that specific combination.
type GLDataSourceTypes = BasicDataSourceTypes .+
  ("boundVertices"     .== (FilePath, DataSource BasicDataSourceTypes))

newtype GLDataSources = GLDataSources (S.Set (DataSource GLDataSourceTypes))
  deriving (Eq, Show)
  deriving (Monoid,Semigroup) via S.Set (DataSource GLDataSourceTypes)

newtype OpenGLResources = OpenGLResources (M.Map (DataSource GLDataSourceTypes) (Resource GLResourceTypes))

-- | Given a scene node returns the resources used
oglResourcesAlgebra :: SceneGraphF dreq OpenGLTarget GLDataSources -> GLDataSources
oglResourcesAlgebra (InvokeF x) = GLDataSources $ S.singleton $ DataSource $ IsJust #boundVertices (x .! #geometry)
oglResourcesAlgebra (GroupF cmds) = foldl (<>) mempty cmds
oglResourcesAlgebra (TransformerF _ gr) = oglResourcesScene gr

oglResourcesScene :: SceneGraph dreq OpenGLTarget -> GLDataSources
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
  
{-
loadOBJFile :: String -> IO GLResourceRecord
loadOBJFile fileName =
  do
    r <- loadWavefrontOBJFile ("resources" </> "geometry" </> fileName)
    case r of
      Left e -> error e
      Right (vertdata, indexdata) -> do
        --let vlength = fromIntegral $ length vertdata
        vbuf <- GLUtil.makeBuffer GL.ArrayBuffer vertdata
        -- there are multiple vertex attributes for this buffer, that
        -- start at different places
        let stride = fromIntegral $ sizeOf (undefined :: OBJBufferRecord)
        let floatSize = fromIntegral $ sizeOf (undefined :: Float)
        let attribs = GLVertexAttributes [
              (GLVertexSource "pos3" vbuf (VertexArrayDescriptor 3 GL.Float stride nullPtr)),
              (GLVertexSource "texCoord" vbuf (VertexArrayDescriptor 2 GL.Float stride (plusPtr nullPtr (3*floatSize)))),
              (GLVertexSource "normal" vbuf (VertexArrayDescriptor 3 GL.Float stride (plusPtr nullPtr (5*floatSize))))
              ]
        let ilength = fromIntegral $ length indexdata
        ibuf <- GLUtil.makeBuffer GL.ElementArrayBuffer ((fmap fromIntegral indexdata) :: [GLUtil.Word32])
        return (GLVertexArray attribs (Just ibuf) ilength)
-}

computeGLDependencies :: DataSource GLDataSourceTypes -> IO [DataSource GLDataSourceTypes]
computeGLDependencies (DataSource (view #boundVertices -> Just bv)) = 
  let sp  = DataSource (IsJust #shaderPath (fst bv))
      vd = bumpVert (snd bv)
  in return [ sp, vd ]
  where
    bumpVert :: DataSource BasicDataSourceTypes -> DataSource GLDataSourceTypes
    bumpVert (DataSource vd) = DataSource (diversify @("boundVertices" .== (FilePath, DataSource BasicDataSourceTypes)) vd)
computeGLDependencies _ = return []

loadGLResource :: DataSource GLDataSourceTypes -> [Resource GLResourceTypes] -> IO (Resource GLResourceTypes)
loadGLResource (DataSource r) deps = 
  case (trial r #boundVertices) of
    Right x -> loadBoundVertices deps
    Left x -> loadBasicGLResource (DataSource x)

unloadGLResource :: DataSource GLDataSourceTypes -> Resource GLResourceTypes -> IO ()
unloadGLResource _ (Resource r) = switch r $
     (#vertexBuffer      .== unloadVertexBuffer)
  .+ (#shaderProgram     .== unloadShaderProgram)
  .+ (#textureObject     .== unloadTextureObject)
  .+ (#boundVertices     .== unloadBoundVertices)
  where
     -- iBuf is a maybe, so use mapM to delete if it's a 'Just'
    unloadVertexBuffer (GeometryResource vBuf iBuf vc _) = do GL.deleteObjectName vBuf; mapM_ GL.deleteObjectName iBuf
    unloadShaderProgram p = GL.deleteObjectName (GLUtil.program p)
    unloadTextureObject o = GL.deleteObjectName o
    unloadBoundVertices (vao,_s,_va) = GL.deleteObjectName vao     -- s and va are resources that get unloaded separately



loadBoundVertices :: [Resource GLResourceTypes] -> IO (Resource GLResourceTypes)
loadBoundVertices deps =
  let sh = findResource #shaderProgram deps
      vb = findResource #vertexBuffer deps
  in case (sh,vb) of
       (Just s, Just v) -> generateBoundVertices s v
       (_,_)            -> error "Argh"
  where
    findResource _ [] = Nothing
    findResource l (Resource (view l -> Just x) : _) = Just x
    findResource l (_ : xs) = findResource l xs

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
loadBasicGLResource ds =
  do (Resource rs) <- loadBasicData ds
     putStrLn $ "Loading thing " ++ show rs
     switch rs $
         (#vertexPositions .== loadGLVertexPositions)
      .+ (#vertexData      .== loadGLVertexData)
      .+ (#shaderBytes     .== undefined)
      .+ (#textureBytes    .== undefined)

{-
loadBuffer :: (Storable a) => GL.BufferTarget -> String -> GL.GLint -> [a] -> IO GLResourceRecord
loadBuffer t name components d =
  do r <- GLUtil.makeBuffer t d
     let vsrc = GLVertexSource name r (VertexArrayDescriptor components GL.Float 0 nullPtr)
     let attributes = GLVertexAttributes [vsrc]
     return (GLVertexArray attributes Nothing (fromIntegral $ length d))
-}

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
  -- we use 'traverse' since ix is a Maybe. So this make an index buffer if ix is a Just
  iBuf <- traverse (\x -> GLUtil.makeBuffer GL.ElementArrayBuffer ((fmap fromIntegral x) :: [GLUtil.Word32])) ix
  let attr = M.map attributesToGL a
  return $ Resource $ IsJust #vertexBuffer (GeometryResource vBuf iBuf vc attr)

loadGLVertexData :: GeometryResource [Float] [Int] VertexAttribute -> IO (Resource GLResourceTypes)
loadGLVertexData (GeometryResource vs ix vc a) = do
  vBuf <- GLUtil.makeBuffer GL.ArrayBuffer vs
  iBuf <- traverse (\x -> GLUtil.makeBuffer GL.ElementArrayBuffer ((fmap fromIntegral x) :: [GLUtil.Word32])) ix
  let attr = M.map attributesToGL a
  return $ Resource $ IsJust #vertexBuffer (GeometryResource vBuf iBuf vc attr)

{-
loadGLGeometry :: GeometryDataSource -> IO GLResourceRecord
loadGLGeometry (RawV2 v2data) = loadBuffer GL.ArrayBuffer "pos2" 2 v2data
loadGLGeometry (RawV3 v3data) = loadBuffer GL.ArrayBuffer "pos3" 3 v3data
loadGLGeometry (RawIndexedV3 v3data ixdata) =
  do
    r_i <- GLUtil.makeBuffer GL.ElementArrayBuffer ixdata
    (GLVertexArray attrib _ _) <- loadBuffer GL.ArrayBuffer "pos3" 3 v3data
    return (GLVertexArray attrib (Just r_i) (fromIntegral $ length ixdata))
loadGLGeometry (OBJFile filePath) = loadOBJFile filePath



loadGLMaterial :: MaterialDataSource -> IO GLResourceRecord
-- for shaders we need seperate files for vertex and fragment shaders
loadGLMaterial (ShaderPath path) =
  do s <- GLUtil.simpleShaderProgram
            ("./resources" </> "shaders" </> path ++ ".vert")
            ("./resources" </> "shaders" </> path ++ ".frag")
     return (GLShaderProgram s)
loadGLMaterial (TexturePath filePath) = do v <- GLUtil.readTexture filePath
                                           case v of
                                             Left err -> error ("Error loading texture " ++ filePath ++ ": " ++ err)
                                             Right obj -> do GL.textureFilter GL.Texture2D $= ((GL.Nearest,Nothing), GL.Nearest)
                                                             GLUtil.texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
                                                             return (GLTextureObject obj)
loadGLMaterial (TextureBytes bytes) = undefined




unloadGLResource :: GLResourceRecord -> IO ()
unloadGLResource (GLVertexArray (GLVertexAttributes a) i l)     =
  do mapM_ unloadSource a
     case i of
       Nothing -> return ()
       Just ib -> GL.deleteObjectName ib
  where
    unloadSource (GLVertexSource _ vbo _) = GL.deleteObjectName vbo
unloadGLResource (GLBoundVertices vao s va) = GL.deleteObjectName vao -- s and va are resources that get unloaded separately
unloadGLResource (GLShaderProgram p)     = GL.deleteObjectName (GLUtil.program p)
unloadGLResource (GLTextureObject o)     = GL.deleteObjectName o



-- | Given a 'GLVertexSource' checks to see if the shader uses a vertex attribute with
--   the same name as the 'GLVertexSource' and if so, binds it to the appropriate attribute.
bindVertexSource :: GLUtil.ShaderProgram -> GLVertexSource -> IO ()
bindVertexSource (GLUtil.ShaderProgram attribs uniforms prog) (GLVertexSource name bufferObj vad) =
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
      


syncBindVAO :: Resources GeneralDataSource GLResourceRecord -> Resources CombinedGLDataSource GLResourceRecord -> CombinedGLDataSource -> IO (Resources CombinedGLDataSource GLResourceRecord)
syncBindVAO r m b@(BoundVertices s v) =
  do
    let sR = lookupResource (MaterialData s) r
    let vR = lookupResource (GeometryData v) r
    let vaoR = lookupResource b m
    case vaoR of
      Just _ -> return m
      Nothing -> 
        case (sR,vR) of
          (Just (GLShaderProgram sObj), Just vR'@(GLVertexArray (GLVertexAttributes vattribs) ixBuf bufSize)) ->
            do
              [vao] <- GL.genObjectNames 1
              putStrLn $ "vao: " ++ show vao
              GL.bindVertexArrayObject $= Just vao
              -- look up each vertex attribute in the shader to find it's index
              mapM_ (bindVertexSource sObj) vattribs
              GL.bindVertexArrayObject $= Nothing
              let newR = (GLBoundVertices vao sObj vR')
              return (insertResource b newR m)
          (_,_)                -> undefined

-}

loaderGLConfig :: ResourceLoaderConfig (DataSource GLDataSourceTypes) (Resource GLResourceTypes)
loaderGLConfig = ResourceLoaderConfig {
  loadIO = loadGLResource,
  unloadIO = unloadGLResource,
  dependenciesIO = computeGLDependencies
  }


-- | Given a scene and a set of already-loaded resources, makes sure all the resources are loaded for this scene. Synchronously loads
--   any resources needed for the scene.
loadOpenGLResourcesScene :: SceneGraph dreq OpenGLTarget -> LoadedResources (DataSource GLDataSourceTypes) (Resource GLResourceTypes) -> IO (LoadedResources (DataSource GLDataSourceTypes) (Resource GLResourceTypes))
loadOpenGLResourcesScene sg lr =
  do
    let (GLDataSources needsResources) = oglResourcesScene sg
    let needAdds = S.toList $ needsResources `S.difference` (topLevelResources lr)
    syncLoad loaderGLConfig lr needAdds




