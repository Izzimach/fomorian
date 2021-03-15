#version 330
uniform mat4 modelMatrix;
uniform mat4 viewMatrix;
uniform mat4 projectionMatrix;
in vec2 pos2;
//in vec2 texCoord;

//out vec2 texCoordFrag;

void main() {
  gl_Position = projectionMatrix * viewMatrix * modelMatrix * vec4(pos2, 0, 1);
  //gl_Position = vec4(pos2,0,1);
  //texCoordFrag = texCoord;
}
