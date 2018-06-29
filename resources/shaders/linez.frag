#version 330

uniform sampler2D tex;
in vec2 texCoordFrag;
out vec4 fragColor;

void main() {
  fragColor = texture (tex, texCoordFrag) + vec4(0.1,0.1,0.1,0);
              //vec4(0,1,1,0);
}