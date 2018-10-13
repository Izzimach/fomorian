#version 330
// Basic shader for unlit 3d objects.
// The vertex buffer can include position, texture coords, and normals
// Use this for imported OBJ files.

uniform mat4 worldTransform;
uniform mat4 cameraProjection;
in vec3 pos3;
in vec2 texCoord;
in vec3 normal;

out vec2 texCoordFrag;
out vec3 normalFrag;

void main() {
  gl_Position = cameraProjection * worldTransform * vec4(pos3, 1);
  texCoordFrag = texCoord;
  normalFrag = normal;
}
