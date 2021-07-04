#version 330
uniform mat4 modelMatrix;
uniform mat4 viewMatrix;
uniform mat4 projectionMatrix;
in vec3 position;
//in vec2 texCoord;

//out vec2 texCoordFrag;

void main() {
  gl_Position = projectionMatrix * viewMatrix * modelMatrix * vec4(position, 1);
  //gl_Position = vec4(pos2,0,1);
  //texCoordFrag = texCoord;
}
