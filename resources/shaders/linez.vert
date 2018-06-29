#version 330
uniform mat4 transformMatrix;
in vec2 pos2;

out vec2 texCoordFrag;

void main() {
  gl_Position = transformMatrix * vec4(pos2, 0, 1);
  texCoordFrag = pos2*vec2(0.003,0.003);
}
