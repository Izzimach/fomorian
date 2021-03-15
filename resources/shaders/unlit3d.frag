#version 330

// basic fragment shader for unlit 3d objects.
// specify a texture 'tex' to be applied to the object

//uniform sampler2D tex;
//in vec2 texCoordFrag;
//in vec3 normalFrag;

out vec4 fragColor;

void main() {
  fragColor = vec4(1,1,0,1);//texture (tex, texCoordFrag);
}
