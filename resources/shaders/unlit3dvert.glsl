// vertex shader for 'unlit3d' shader used by the Vulkan renderer. After changing
// this you need to recompile it into "unlit3dvert.spv" using glslc or equivalent

#version 450
#extension GL_ARB_separate_shader_objects : enable

layout (binding = 0) uniform UniformBufferObject {
    mat4 modelMatrix;
    mat4 viewMatrix;
    mat4 projectionMatrix;
} mvp;

layout (location = 0) in vec3 inPosition;
layout (location = 1) in vec2 inTexCoord;
layout (location = 2) in vec3 inNormal;

layout (location = 0) out vec3 fragNormal;
layout (location = 1) out vec2 fragTexCoord;

void main() {
    gl_Position = mvp.projectionMatrix * mvp.viewMatrix * mvp.modelMatrix * vec4(inPosition, 1.0);
    fragNormal = inNormal;
    fragTexCoord = inTexCoord;
}