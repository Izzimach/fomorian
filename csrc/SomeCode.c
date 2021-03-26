#include <stdio.h>
#include <stdlib.h>
#include <vulkan/vulkan.h>

VKAPI_ATTR VkBool32 VKAPI_CALL
debugCallback(VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
              VkDebugUtilsMessageTypeFlagsEXT messageType,
              const VkDebugUtilsMessengerCallbackDataEXT *pCallbackData,
              void *pUserData) {
  fprintf(stderr, "Validation: %s\n", pCallbackData->pMessage);
  return VK_FALSE;
}

int32_t foo(int32_t a) {
  return a+2;
}

int32_t foo2(int32_t (*haskellfunc)(int32_t)) {
  int32_t retval = haskellfunc(3);
  return retval;
}