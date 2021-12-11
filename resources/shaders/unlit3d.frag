// glsl fragment shader for the 'unlit3d' resource used by the OpenGL renderer
#version 330

// basic fragment shader for unlit 3d objects.
// specify a texture 'tex' to be applied to the object

uniform sampler2D tex;
in vec2 texCoordFrag;
in vec3 normalFrag;

out vec4 fragColor;

void main() {
  fragColor = texture (tex, texCoordFrag);
}
