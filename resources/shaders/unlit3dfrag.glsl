// fragment shader for 'unlit3d' shader used by the Vulkan renderer. After changing
// this you need to recompile it into "unlit3dfrag.spv" using glslc or equivalent
#version 450
#extension GL_ARB_separate_shader_objects : enable

layout (location = 0) in vec3 fragNormal;
layout (location = 1) in vec2 fragTexCoord;

layout (binding = 1) uniform sampler2D texSampler;

layout(location = 0) out vec4 outColor;

void main() {
    outColor = texture(texSampler, fragTexCoord);
}
