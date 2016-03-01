#pragma once
#include <glm/glm.hpp>
#include <glm/gtx/transform.hpp>

glm::mat4 matFromVecs(glm::vec3 o, glm::vec3 vx, glm::vec3 vy, glm::vec3 vz);

glm::vec3 vpol(float rho, float phi);

glm::mat4 buildTMatrixFromPoints(float pos_x, float pos_y, float pos_z, float pos_x_2, float pos_y_2, float pos_z_2);
