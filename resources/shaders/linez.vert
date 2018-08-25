#version 330
uniform mat4 worldTransform;
uniform mat4 cameraProjection;
in vec2 pos2;

out vec2 texCoordFrag;

void main() {
  gl_Position = cameraProjection * worldTransform * vec4(pos2, 0, 1);
  texCoordFrag = pos2*vec2(0.003,0.003);
}
