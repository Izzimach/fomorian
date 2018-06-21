#version 150
uniform mat4 transformMatrix;
in vec2 pos2;

void main() {
  gl_Position = transformMatrix * vec4(pos2, 0, 1);
}
