#pragma once
#ifndef CONTROLS_HPP
#define CONTROLS_HPP

void savePos();
void computeMatricesFromInputs();
glm::mat4 getViewMatrix();
glm::mat4 getProjectionMatrix();

#endif