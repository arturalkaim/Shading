#define GLEW_STATIC
#include <GL/glew.h>
//#include <SFML/Window.hpp>
#include <GLFW/glfw3.h>

#include <glm/glm.hpp>
#include <glm/gtx/transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtc/random.hpp>
#include <glm/gtc/noise.hpp>
#include "glm/ext.hpp"
#include <iostream>
#include <sstream>      // std::ostringstream

#include "shaders.h"
//#include "main.h"

const float PI = 3.1415926;

#define VALUES_PER_POINT 19
int n_points = 0;
int vecSize = 10;
GLfloat* points = (float *)calloc(sizeof(float), VALUES_PER_POINT * 10);



float* buildPoints(int n) {
	float* ret = (float *)malloc(sizeof(float)*VALUES_PER_POINT*n);
	vecSize = n;
	for (int i = 0, j = 0; i < n; ++i, j = 0)
	{
		// pos-x, pos-y, type, n_sides, size-x, size-y
		ret[i*VALUES_PER_POINT + (j++)] = ((float)rand() / (RAND_MAX / 1000)) - 200.5f;
		ret[i*VALUES_PER_POINT + (j++)] = ((float)rand() / (RAND_MAX / 1000)) - 200.5f;
		ret[i*VALUES_PER_POINT + (j++)] = ((float)rand() / (RAND_MAX));
		ret[i*VALUES_PER_POINT + (j++)] = (i % 5) + 1;
		ret[i*VALUES_PER_POINT + (j++)] = ((float)(rand() % 40));
		ret[i*VALUES_PER_POINT + (j++)] = ((float)rand() / (RAND_MAX)) + 0.02f;
		ret[i*VALUES_PER_POINT + (j++)] = ((float)rand() / (RAND_MAX)) + 0.02f;
		ret[i*VALUES_PER_POINT + (j++)] = ((float)rand() / (RAND_MAX)) + 0.6f;
		ret[i*VALUES_PER_POINT + (j++)] = 0.0;
		ret[i*VALUES_PER_POINT + (j++)] = 0.0;
		ret[i*VALUES_PER_POINT + (j++)] = 0.0;
		ret[i*VALUES_PER_POINT + (j++)] = 0.0;
	}
	return ret;
}

void printMatrix(glm::mat4 mat) {
	printf("%s\n", glm::to_string(mat).c_str());
}

void printVec(glm::vec3 vec) {
	printf("%s\n", glm::to_string(vec).c_str());
}

void printMatrixV(int n) {
	for (int i = 0; i<16; i++) {
		printf("%f ", points[n*VALUES_PER_POINT + i]);
		if (i % 4 == 3)
			printf("\n");
	}
}

glm::mat4 cleanColor(glm::mat4 mat) {

	glm::mat4 ret = mat;

	glm::vec4 gColor = glm::vec4(mat[0][3], mat[1][3], mat[2][3], 1.0);

	ret[0][3] = 0.0;
	ret[1][3] = 0.0;
	ret[2][3] = 0.0;

	return ret;
}

int buildPoint(int n,
	glm::mat4 mat, float type, float n_sides, float scale, glm::vec3 color) {

	n_points++;
	//printMatrix(mat);

	//printf("Build Point n: %d\n", n);

	if (n_points >= vecSize) {
		points = (float *)realloc(points, sizeof(float)*VALUES_PER_POINT*vecSize * 2);
		vecSize = 2 * vecSize;
		printf("TEST %d  :  %d\n", vecSize, n_points);
	}

	//printMatrix(mat);

	memcpy(points + n*VALUES_PER_POINT, glm::value_ptr(mat), 16 * sizeof(float));

	//printf("%f %f\n", points[n*VALUES_PER_POINT+15], mat[1][1]);
	points[n*VALUES_PER_POINT + 3] = color[0];
	points[n*VALUES_PER_POINT + 7] = color[1];
	points[n*VALUES_PER_POINT + 11] = color[2];

	points[n*VALUES_PER_POINT + 16] = type;
	points[n*VALUES_PER_POINT + 17] = n_sides;
	points[n*VALUES_PER_POINT + 18] = scale;


	//printMatrixV(n);
	return n;
}


extern "C" __declspec(dllexport) int createPoints(int in) {
	n_points += in;
	points = buildPoints(n_points);
	return 0;
}

void printObject(int n) {

	printf("Object n: %d\n", n);
	switch ((int)points[n*VALUES_PER_POINT + 16]) {
	case 1:
		printf("BOX:\n");
		break;
	case 2:
		printf("NONE\n");
		break;
	case 3:
		printf("Cylinder\n");
		break;
	case 4:
		printf("Pyramid\n");
		break;
	case 5:
	case 6:
	case 7:
		printf("Sphere\n");
		break;
	default:
		printf("Unknown Object!!!!\n");
	}



}

glm::mat4 matFromVecs(glm::vec3 o, glm::vec3 vx, glm::vec3 vy, glm::vec3 vz) {
	glm::mat4 ret(
		vx.x, vy.x, vz.x, o.x,
		vx.y, vy.y, vz.y, o.y,
		vx.z, vy.z, vz.z, o.z,
		0.0f, 0.0f, 0.0f, 1.0f);
	glm::mat4 ret1(
		vx.x, vx.y, vx.z, 0.0f,
		vy.x, vy.y, vy.z, 0.0f,
		vz.x, vz.y, vz.z, 0.0f,
		o.x, o.y, o.z, 1.0f);
	//printf("matFromVecs:\n");
	
	//printMatrix(ret);

	//printf("transpose matFromVecs:\n");
	ret = glm::transpose(ret);
	//printMatrix(ret);

/*	glm::mat4 ret(vx.x, vy.x, vz.x, 0.0f, vx.y, vy.y, vz.y, 0.0f, vx.z, vy.z, vz.z, 0.0f, o.x, o.y, o.z, 1.0f);
	ret[0][0] = vx.x; ret[0][1] = vx.y; ret[0][2] = vx.z; ret[0][3] = o.x;
	ret[1][0] = vy.x; ret[1][1] = vy.y; ret[1][2] = vy.z; ret[1][3] = o.x;
	ret[2][0] = vz.x; ret[2][1] = vz.y; ret[2][2] = vz.z; ret[2][3] = o.x;
	ret[3][0] = 0.0f; ret[3][1] = 0.0f; ret[3][2] = 0.0f; ret[3][3] = 1.0f;*/

	return ret1;
}

glm::vec3 vpol(float rho, float phi) {
	return glm::vec3(rho * cos(phi), rho * sin(phi),0.0f);
}

glm::mat4 buildTMatrixFromPoints(float pos_x, float pos_y, float pos_z, float pos_x_2, float pos_y_2, float pos_z_2) {

	glm::vec3 n, o, vx, vy, vz;
	n = glm::vec3(pos_x_2, pos_y_2, pos_z_2) - glm::vec3(pos_x, pos_y, pos_z);
	o = glm::vec3(pos_x + pos_x_2, pos_y + pos_y_2, pos_z + pos_z_2)/ 2.0f;
	//glm::vec3(pos_x, pos_y, pos_z); //
	
	vx = vpol(1.0f, glm::atan(n.y, n.x) - (PI/2.0));
	//if (vx.x < 0 || vx.y < 0 || vx.z < 0)
	//	vx = -vx;
	vy = glm::normalize(glm::cross(n, vx));
	vz = glm::normalize(n);
	
	/*
	(define (cs-from-o-vz [o : Loc] [n : Vec])
	  (let ((o (loc-in-world o))
			(n (vec-in-world n)))
		(let ((vx (vpol 1 (+ (sph-phi n) pi/2))))
		  (let ((vy (unitize (v*v n vx))))
			(let ((vz (unitize n)))
			  (cs-from-o-vx-vy-vz o vx vy vz))))))
		 
	printf("Vecs x:\n");
	printVec(vx);
	printf("Vecs y:\n");
	printVec(vy);
	printf("Vecs z:\n");
	printVec(vz); */

	return matFromVecs(o,vx,vy,vz);
}


extern "C" __declspec(dllexport) int building(float pos_x, float pos_y, float pos_z, float w, float l, float h, int divs, float r, float g, float b) {
	//printf("Build Box\n");
	return buildPoint(n_points, glm::scale(glm::translate(glm::mat4(), glm::vec3(pos_x, pos_y, pos_z)), glm::vec3(w, l, h)),
		8, divs, w, glm::vec3(r, g, b));
}
extern "C" __declspec(dllexport) int buildingv(float pos_x, float pos_y, float pos_z, float w, float l, float h, int divs, glm::vec3 color) {
	//printf("Build Box\n");
	return buildPoint(n_points, glm::scale(glm::translate(glm::mat4(), glm::vec3(pos_x, pos_y, pos_z)), glm::vec3(w, l, h)),
		8, divs, w, color);
}

extern "C" __declspec(dllexport) int tree(float pos_x, float pos_y, float pos_z, float w, float l, float h, int divs, glm::vec3 color) {
	//printf("Build Box\n");
	return buildPoint(n_points, glm::scale(glm::translate(glm::mat4(), glm::vec3(pos_x, pos_y, pos_z)), glm::vec3(w, l, h)),
		9, divs, w, color);
}

extern "C" __declspec(dllexport) int boxv(float pos_x, float pos_y, float pos_z, float w, float l, float h, glm::vec3 color) {
	//printf("Build Box\n");
	return buildPoint(n_points, glm::scale(glm::translate(glm::mat4(), glm::vec3(pos_x, pos_y, pos_z)), glm::vec3(w, l, h)),
		1, 4, w, color);
}

extern "C" __declspec(dllexport) int box(float pos_x, float pos_y, float pos_z, float w, float l, float h, float red, float g, float b, float angle, float vx, float vy, float vz) {
	//printf("Build Box\n");
	return buildPoint(n_points, glm::scale(glm::rotate(glm::translate(glm::mat4(1.0f), glm::vec3(pos_x, pos_y, pos_z)), angle, glm::vec3(vx, vy, vz)), glm::vec3(w, l, h)),
		1, 4, w, glm::vec3(red, g, b));
}

extern "C" __declspec(dllexport) int prism(float pos_x, float pos_y, float pos_z, float l, float w, float h, float sides, float red, float g, float b, float angle, float vx, float vy, float vz) {
	//printf("Build Cylinder\n");
	return buildPoint(n_points, glm::scale(glm::rotate(glm::translate(glm::mat4(1.0f), glm::vec3(pos_x, pos_y, pos_z)), angle, glm::vec3(vx, vy, vz)), glm::vec3(l, w, h)),
		3, sides, w, glm::vec3(red, g, b));
}

extern "C" __declspec(dllexport) int prismpts(float pos_x, float pos_y, float pos_z, float pos_x_2, float pos_y_2, float pos_z_2, float l, float w, float h, float sides, float red, float g, float b) {
	//printf("Build Cylinder\n");
	return buildPoint(n_points, glm::scale(buildTMatrixFromPoints(pos_x, pos_y, pos_z, pos_x_2, pos_y_2, pos_z_2), glm::vec3(l, w, h)),
		3, sides, w, glm::vec3(red, g, b));
}

extern "C" __declspec(dllexport) int cylinder(float pos_x, float pos_y, float pos_z, float r, float h, float red, float g, float b, float angle, float vx, float vy, float vz) {
	//printf("Build Cylinder\n");
	return buildPoint(n_points, glm::scale(glm::rotate(glm::translate(glm::mat4(1.0f), glm::vec3(pos_x, pos_y, pos_z)), angle, glm::vec3(vx, vy, vz)), glm::vec3(r, r, h)),
		3, 20, r, glm::vec3(red, g, b));
}
extern "C" __declspec(dllexport) int cylinderv(float pos_x, float pos_y, float pos_z, float r, float h, glm::vec3 color) {
	//printf("Build Cylinder\n");
	return buildPoint(n_points, glm::scale(glm::translate(glm::mat4(1.0f), glm::vec3(pos_x, pos_y, pos_z)), glm::vec3(r, r, h)),
		3, 20, r, color);
}
extern "C" __declspec(dllexport) int pyramid(float pos_x, float pos_y, float pos_z, float w, float l, float h, float sides, glm::vec3 color) {
	return buildPoint(n_points, glm::scale(
		glm::translate(
			glm::mat4(),
			glm::vec3(pos_x, pos_y, pos_z)),
		glm::vec3(w, l, h)),
		4, sides, w, color);
}
extern "C" __declspec(dllexport) int spherev(float pos_x, float pos_y, float pos_z, float r, glm::vec3 color) {
	buildPoint(n_points, glm::scale(
		glm::translate(
			glm::mat4(),
			glm::vec3(pos_x, pos_y, pos_z)),
		glm::vec3(r, r, r)),
		5, 20, r, color);
	buildPoint(n_points, glm::scale(
		glm::translate(
			glm::mat4(),
			glm::vec3(pos_x, pos_y, pos_z)),
		glm::vec3(r, r, r)),
		6, 20, r, color);
	return buildPoint(n_points, glm::scale(
		glm::translate(
			glm::mat4(),
			glm::vec3(pos_x, pos_y, pos_z)),
		glm::vec3(r, r, r)),
		7, 20, r, color);
	/*return buildPoint(n_points, glm::scale(
	glm::translate(
	glm::mat4(),
	glm::vec3(pos_x, pos_y,pos_z)),
	glm::vec3(r, r,r)),
	2, 23,color);*/
}
extern "C" __declspec(dllexport) int sphere(float pos_x, float pos_y, float pos_z, float r, float red, float g, float b) {
	buildPoint(n_points, glm::scale(
		glm::translate(
			glm::mat4(),
			glm::vec3(pos_x, pos_y, pos_z)),
		glm::vec3(r, r, r)),
		5, 23, r, glm::vec3(red, g, b));
	buildPoint(n_points, glm::scale(
		glm::translate(
			glm::mat4(),
			glm::vec3(pos_x, pos_y, pos_z)),
		glm::vec3(r, r, r)),
		6, 23, r, glm::vec3(red, g, b));
	return buildPoint(n_points, glm::scale(
		glm::translate(
			glm::mat4(),
			glm::vec3(pos_x, pos_y, pos_z)),
		glm::vec3(r, r, r)),
		7, 23, r, glm::vec3(red, g, b));
	/*return buildPoint(n_points, glm::scale(
	glm::translate(
	glm::mat4(),
	glm::vec3(pos_x, pos_y,pos_z)),
	glm::vec3(r, r,r)),
	2, 23,color);*/
}

extern "C" __declspec(dllexport) int rotate(int n,
	float angle,
	float vx, float vy, float vz) {

	if (points[n*VALUES_PER_POINT + 16] == 5)
	{
		rotate(n + 1, angle, vx, vy, vz);
		rotate(n + 2, angle, vx, vy, vz);
	}

	float v_length = sqrt((vx * vx) + (vy * vy) + (vz * vz));

	//printf("CENAS ROTATE\n");

	glm::mat4 mat1 = glm::mat4(points[n*VALUES_PER_POINT + 0], points[n*VALUES_PER_POINT + 1], points[n*VALUES_PER_POINT + 2], 0.0f,
		points[n*VALUES_PER_POINT + 4], points[n*VALUES_PER_POINT + 5], points[n*VALUES_PER_POINT + 6], 0.0f,
		points[n*VALUES_PER_POINT + 8], points[n*VALUES_PER_POINT + 9], points[n*VALUES_PER_POINT + 10], 0.0f,
		points[n*VALUES_PER_POINT + 12], points[n*VALUES_PER_POINT + 13], points[n*VALUES_PER_POINT + 14], 0.0f);

	glm::mat4 res = glm::rotate(mat1, angle, glm::vec3(vx / v_length, vy / v_length, vz / v_length));

	printMatrix(res);

	points[n*VALUES_PER_POINT+0]=res[0][0];points[n*VALUES_PER_POINT+1]=res[0][1];points[n*VALUES_PER_POINT+2]=res[0][2];//points[n*VALUES_PER_POINT+3]=res[0][3];
	points[n*VALUES_PER_POINT+4]=res[1][0];points[n*VALUES_PER_POINT+5]=res[1][1];points[n*VALUES_PER_POINT+6]=res[1][2];//points[n*VALUES_PER_POINT+7]=res[1][3];
	points[n*VALUES_PER_POINT+8]=res[2][0];points[n*VALUES_PER_POINT+9]=res[2][1];points[n*VALUES_PER_POINT+10]=res[2][2];//points[n*VALUES_PER_POINT+11]=res[2][3];
	points[n*VALUES_PER_POINT+12]=res[3][0];points[n*VALUES_PER_POINT+13]=res[3][1];points[n*VALUES_PER_POINT+14]=res[3][2];//points[n*VALUES_PER_POINT+15]=res[3][3];

	//memcpy(points+n*VALUES_PER_POINT*sizeof(float),glm::value_ptr(res),16*sizeof(float));

	printf("ROTATE %f• (%f,%f,%f)\n",angle,vx/v_length,vy/v_length,vz/v_length);
	printMatrixV(n);

	return n;
}

extern "C" __declspec(dllexport) int move(int n,
	float vx, float vy, float vz) {

	if (points[n*VALUES_PER_POINT + 16] == 5)
	{
		move(n + 1, vx, vy, vz);
		move(n + 2, vx, vy, vz);
	}


	glm::mat4 mat1 = glm::mat4(
		points[n*VALUES_PER_POINT + 0], points[n*VALUES_PER_POINT + 1], points[n*VALUES_PER_POINT + 2], points[n*VALUES_PER_POINT + 3],
		points[n*VALUES_PER_POINT + 4], points[n*VALUES_PER_POINT + 5], points[n*VALUES_PER_POINT + 6], points[n*VALUES_PER_POINT + 7],
		points[n*VALUES_PER_POINT + 8], points[n*VALUES_PER_POINT + 9], points[n*VALUES_PER_POINT + 10], points[n*VALUES_PER_POINT + 11],
		points[n*VALUES_PER_POINT + 12], points[n*VALUES_PER_POINT + 13], points[n*VALUES_PER_POINT + 14], points[n*VALUES_PER_POINT + 15]);

	//printf("MAT %d ", n);
	//printMatrix(mat1);
	glm::mat4 res = glm::translate(mat1, glm::vec3(vx, vy, vz));
	//printf("RES %d ", n);
	//printMatrix(res);

	memcpy(points + n*VALUES_PER_POINT*sizeof(float), glm::value_ptr(res), 16 * sizeof(float));

	//printf("memcpy DONE!!\n");
	return n;
}

extern "C" __declspec(dllexport) int scale(int n,
	float vx, float vy, float vz) {


	if (points[n*VALUES_PER_POINT + 16] == 5)
	{
		scale(n + 1, vx, vy, vz);
		scale(n + 2, vx, vy, vz);
	}


	glm::mat4 mat1 = glm::mat4(points[n*VALUES_PER_POINT + 0], points[n*VALUES_PER_POINT + 1], points[n*VALUES_PER_POINT + 2], points[n*VALUES_PER_POINT + 3],
		points[n*VALUES_PER_POINT + 4], points[n*VALUES_PER_POINT + 5], points[n*VALUES_PER_POINT + 6], points[n*VALUES_PER_POINT + 7],
		points[n*VALUES_PER_POINT + 8], points[n*VALUES_PER_POINT + 9], points[n*VALUES_PER_POINT + 10], points[n*VALUES_PER_POINT + 11],
		points[n*VALUES_PER_POINT + 12], points[n*VALUES_PER_POINT + 13], points[n*VALUES_PER_POINT + 14], points[n*VALUES_PER_POINT + 15]);
	printMatrix(mat1);
	glm::mat4 res = glm::scale(mat1, glm::vec3(vx, vy, vz));

	memcpy(points + n*VALUES_PER_POINT*sizeof(float), glm::value_ptr(res), 16 * sizeof(float));

	return n;
}


// The following is an 8x8 checkerboard pattern using // GL_RED, GL_UNSIGNED_BYTE data.
static const GLubyte tex_checkerboard_data[] =
{
	0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00,
	0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF,
	0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00,
	0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF,
	0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00,
	0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF,
	0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00,
	0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF
};
// The following data represents a 2x2 texture with red, // green, blue, and yellow texels represented as GL_RGBA, // GL_FLOAT data.
static const GLfloat tex_color_data[] =
{
	// Red texels               Green texel
	1.0f, 0.0f, 0.0f, 1.0f,     0.0f, 1.0f, 0.0f, 1.0f,
	// Blue texel               Yellow texel
	0.0f, 0.0f, 1.0f, 1.0f,     1.0f, 1.0f, 0.0f, 1.0f
};


// Camera
glm::vec3 cameraPos = glm::vec3(0.0f, 70.0f, 30.0f);
glm::vec3 cameraFront = glm::vec3(0.0f, -1.0f, 0.0f);
glm::vec3 cameraUp = glm::vec3(0.0f, 0.0f, 1.0f);
glm::vec3 posLookAt = glm::vec3(0.0f);
bool keys[1024];

void scroll_callback(GLFWwindow* window, double xoffset, double yoffset)
{
	if (std::abs(xoffset) < std::abs(yoffset))
		cameraPos += ((GLfloat)yoffset * 0.1f) * cameraUp;
	else
		cameraPos += glm::normalize(glm::cross(cameraFront, cameraUp)) * ((GLfloat)yoffset) * 40.0f;
}
// Is called whenever a key is pressed/released via GLFW
void key_callback(GLFWwindow* window, int key, int scancode, int action, int mode)
{
	if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS)
		glfwSetWindowShouldClose(window, GL_TRUE);
	if (key >= 0 && key < 1024)
	{
		//printf("KEY %d\n", key);
		if (action == GLFW_PRESS)
			keys[key] = true;
		else if (action == GLFW_RELEASE)
			keys[key] = false;
	}


}
double xpos_1, ypos_1, xpos_2, ypos_2, speed = 1;
GLenum pressed = GL_FALSE;

void mouse_button_callback(GLFWwindow* window, int button, int action, int mods)
{

	if (button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_PRESS) {
		pressed = GL_TRUE;
		glfwGetCursorPos(window, &xpos_1, &ypos_1);
	}
	if (button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_RELEASE) {
		pressed = GL_FALSE;
	}
	if (button == GLFW_MOUSE_BUTTON_RIGHT && action == GLFW_RELEASE)
		posLookAt = glm::vec3(0.0f);
}

void do_movement() {

	//float distance = glm::distance( posLookAt , cameraPos);
	//printf("distance : %f\n", distance);
	// Camera controls
	GLfloat cameraSpeed = 0.05f;
	if (keys[GLFW_KEY_W]) {
		cameraPos += cameraSpeed * cameraFront;
		cameraFront = -cameraPos + posLookAt;

	}
	if (keys[GLFW_KEY_S]) {
		cameraPos -= cameraSpeed * cameraFront;
		cameraFront = -cameraPos + posLookAt;

	}
	if (keys[GLFW_KEY_A]) {
		cameraPos -= glm::normalize(glm::cross(cameraFront, cameraUp)) * cameraSpeed * glm::distance(cameraPos, glm::vec3(0.0f));
		cameraFront = -cameraPos + posLookAt;

	}
	if (keys[GLFW_KEY_D]) {
		cameraPos += glm::normalize(glm::cross(cameraFront, cameraUp)) * cameraSpeed * glm::distance(cameraPos, glm::vec3(0.0f));
		cameraFront = -cameraPos + posLookAt;

	}
	if (keys[GLFW_KEY_RIGHT])
	{
		cameraPos += glm::normalize(glm::cross(cameraFront, cameraUp)) * cameraSpeed * 50.0f;
		posLookAt += glm::normalize(glm::cross(cameraFront, cameraUp)) * cameraSpeed * 50.0f;
		cameraFront = -cameraPos + posLookAt;

	}
	if (keys[GLFW_KEY_LEFT])
	{
		cameraPos -= glm::normalize(glm::cross(cameraFront, cameraUp)) * cameraSpeed * 50.0f;
		posLookAt -= glm::normalize(glm::cross(cameraFront, cameraUp)) * cameraSpeed * 50.0f;
		cameraFront = -cameraPos + posLookAt;

	}
	if (keys[GLFW_KEY_UP])
	{
		cameraPos = glm::vec3(cameraPos.x + cameraFront.x*cameraSpeed, cameraPos.y + cameraFront.y*cameraSpeed, cameraPos.z);
		posLookAt = glm::vec3(posLookAt.x + cameraFront.x*cameraSpeed, posLookAt.y + cameraFront.y*cameraSpeed, posLookAt.z);
		cameraFront = -cameraPos + posLookAt;

	}
	if (keys[GLFW_KEY_DOWN])
	{
		cameraPos = glm::vec3(cameraPos.x - cameraFront.x*cameraSpeed, cameraPos.y - cameraFront.y*cameraSpeed, cameraPos.z);
		posLookAt = glm::vec3(posLookAt.x - cameraFront.x*cameraSpeed, posLookAt.y - cameraFront.y*cameraSpeed, posLookAt.z);
		cameraFront = -cameraPos + posLookAt;

	}

	if (keys[GLFW_KEY_R]) {
		cameraPos += cameraSpeed * cameraUp;
		posLookAt += cameraSpeed * cameraUp;
		cameraFront = -cameraPos + posLookAt;

	}
	if (keys[GLFW_KEY_F]) {
		cameraPos -= cameraSpeed * cameraUp;
		posLookAt -= cameraSpeed * cameraUp;
		cameraFront = -cameraPos + posLookAt;

	}


	if (keys[GLFW_KEY_P]) {
		glPointSize(8.0f);
		glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);

	}
	if (keys[GLFW_KEY_L]) {
		glEnable(GL_LINE_SMOOTH);
		glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

	}
	if (keys[GLFW_KEY_O]) {
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

	}
}

int nbFrames = 0;
double lastTime = 0.0;
void showFPS()
{
	// Measure speed
	double currentTime = glfwGetTime();
	nbFrames++;
	if (currentTime - lastTime >= 1.0) { // If last cout was more than 1 sec ago
		printf("%f\n", 1000.0 / double(nbFrames));
		nbFrames = 0;
		lastTime += 1.0;
	}
}

double calcFPS(GLFWwindow* window, double timeInterval = 1.0, std::string windowTitle = "NONE")
{
	// Static values which only get initialised the first time the function runs
	static double startTime = glfwGetTime(); // Set the initial time to now
	static double fps = 0.0;           // Set the initial FPS value to 0.0

									   // Set the initial frame count to -1.0 (it gets set to 0.0 on the next line). Because
									   // we don't have a start time we simply cannot get an accurate FPS value on our very
									   // first read if the time interval is zero, so we'll settle for an FPS value of zero instead.
	static double frameCount = -1.0;

	// Here again? Increment the frame count
	frameCount++;

	// Ensure the time interval between FPS checks is sane (low cap = 0.0 i.e. every frame, high cap = 10.0s)
	if (timeInterval < 0.0)
	{
		timeInterval = 0.0;
	}
	else if (timeInterval > 10.0)
	{
		timeInterval = 10.0;
	}

	// Get the duration in seconds since the last FPS reporting interval elapsed
	// as the current time minus the interval start time
	double duration = glfwGetTime() - startTime;

	// If the time interval has elapsed...
	if (duration > timeInterval)
	{
		// Calculate the FPS as the number of frames divided by the duration in seconds
		fps = frameCount / duration;

		// If the user specified a window title to append the FPS value to...
		if (windowTitle != "NONE")
		{
			// Convert the fps value into a string using an output stringstream
			std::ostringstream stream;
			stream << fps;
			std::string fpsString = stream.str();

			// Append the FPS value to the window title details
			windowTitle += " | FPS: " + fpsString;

			// Convert the new window title to a c_str and set it
			const char* pszConstString = windowTitle.c_str();
			glfwSetWindowTitle(window, pszConstString);
		}
		else // If the user didn't specify a window to append the FPS to then output the FPS to the console
		{
			std::cout << "FPS: " << fps << std::endl;
		}

		// Reset the frame count to zero and set the initial time to be now
		frameCount = 0.0;
		startTime = glfwGetTime();
	}

	// Return the current FPS - doesn't have to be used if you don't want it!
	return fps;
}

GLuint vbo;
GLFWwindow* window;
GLenum shader1 = GL_TRUE;
GLuint shaderProgram, shaderProgram2;
glm::mat4 model, view, projection;
GLint modelLoc, viewLoc, projLoc;

extern "C" __declspec(dllexport) int init(int n) {
	vecSize = n;
	points = (float *)calloc(sizeof(float), VALUES_PER_POINT*n);
	const GLuint WIDTH = 1250, HEIGHT = 800;

	glfwInit();

	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
	glfwWindowHint(GLFW_DOUBLEBUFFER, GL_TRUE);
	glfwWindowHint(GLFW_RESIZABLE, GL_TRUE);
	glfwWindowHint(GL_DEPTH_BITS, 49);
	glfwWindowHint(GLFW_SAMPLES, 16);
	glfwWindowHint(GLFW_STICKY_KEYS, GL_TRUE);

	window = glfwCreateWindow(WIDTH, HEIGHT, "Fast OpenGL", NULL, NULL); // Windowed

	glfwMakeContextCurrent(window);

	int width, height;
	glfwGetFramebufferSize(window, &width, &height);
	glViewport(0, 0, width, height);

	// Set the required callback functions
	glfwSetKeyCallback(window, key_callback);
	glfwSetScrollCallback(window, scroll_callback);
	glfwSetMouseButtonCallback(window, mouse_button_callback);


	// Initialize GLEW
	glewExperimental = GL_TRUE;
	GLenum err = glewInit();
	//printf("glew Error: %s\n", glewGetErrorString(err));
	//printf("glu Error: %s\n", gluErrorString(err));
	//testError("INIT ");


	//   glShadeModel(GL_SMOOTH);                            // Enable Smooth Shading

	glClearDepth(1.0f);                                    // Depth Buffer Setup
	glEnable(GL_DEPTH_TEST);                            // Enables Depth Testing
	glDepthFunc(GL_LEQUAL);                                // The Type Of Depth Testing To Do


														   // Compile and activate shaders
	GLuint vertexShader = createShader(GL_VERTEX_SHADER, vertexShaderSrc);

	GLuint geometryShader = createShader(GL_GEOMETRY_SHADER, geometryShaderSrc);
	//GLuint geometryShader2 = createShader(GL_GEOMETRY_SHADER, geometryShaderSrc2);

	GLuint fragmentShader = createShader(GL_FRAGMENT_SHADER, fragmentShaderSrc);

	GLuint tessEvalShader = createShader(GL_TESS_EVALUATION_SHADER, tesselationEvalShaderSrc);
	GLuint tessCtrlShader = createShader(GL_TESS_CONTROL_SHADER, tesselationCtrlShaderSrc);

	shaderProgram = glCreateProgram();

	glAttachShader(shaderProgram, vertexShader);
	glAttachShader(shaderProgram, geometryShader);
	//glAttachShader(shaderProgram, geometryShader2);
	glAttachShader(shaderProgram, fragmentShader);
	//glAttachShader(shaderProgram, tessEvalShader);
	//glAttachShader(shaderProgram, tessCtrlShader);
	glLinkProgram(shaderProgram);


	glUseProgram(shaderProgram);


	// Create VBO with point coordinates

	glGenBuffers(1, &vbo);

	glBindBuffer(GL_ARRAY_BUFFER, vbo);
	glBufferData(GL_ARRAY_BUFFER, sizeof(float)*VALUES_PER_POINT*n_points, points, GL_STATIC_DRAW);

	// Create VAO
	GLuint vao;
	glGenVertexArrays(1, &vao);
	glBindVertexArray(vao);
	testError("GOGO 111");

	// Specify layout of point data
	GLint posAttrib = glGetAttribLocation(shaderProgram, "mat_1");
	glEnableVertexAttribArray(posAttrib);
	glVertexAttribPointer(posAttrib, 4, GL_FLOAT, GL_FALSE, VALUES_PER_POINT * sizeof(GLfloat), 0);

	// Specify layout of point data
	posAttrib = glGetAttribLocation(shaderProgram, "mat_2");
	glEnableVertexAttribArray(posAttrib);
	glVertexAttribPointer(posAttrib, 4, GL_FLOAT, GL_FALSE, VALUES_PER_POINT * sizeof(GLfloat), (void*)(4 * sizeof(GLfloat)));

	// Specify layout of point data
	posAttrib = glGetAttribLocation(shaderProgram, "mat_3");
	glEnableVertexAttribArray(posAttrib);
	glVertexAttribPointer(posAttrib, 4, GL_FLOAT, GL_FALSE, VALUES_PER_POINT * sizeof(GLfloat), (void*)(8 * sizeof(GLfloat)));

	// Specify layout of point data
	posAttrib = glGetAttribLocation(shaderProgram, "mat_4");
	glEnableVertexAttribArray(posAttrib);
	glVertexAttribPointer(posAttrib, 4, GL_FLOAT, GL_FALSE, VALUES_PER_POINT * sizeof(GLfloat), (void*)(12 * sizeof(GLfloat)));

	// Specify layout of point data
	posAttrib = glGetAttribLocation(shaderProgram, "type");
	glEnableVertexAttribArray(posAttrib);
	glVertexAttribPointer(posAttrib, 1, GL_FLOAT, GL_FALSE, VALUES_PER_POINT * sizeof(GLfloat), (void*)(16 * sizeof(GLfloat)));

	// Specify layout of point data
	posAttrib = glGetAttribLocation(shaderProgram, "sides");
	glEnableVertexAttribArray(posAttrib);
	glVertexAttribPointer(posAttrib, 1, GL_FLOAT, GL_FALSE, VALUES_PER_POINT * sizeof(GLfloat), (void*)(17 * sizeof(GLfloat)));

	// Specify layout of point data
	posAttrib = glGetAttribLocation(shaderProgram, "scale");
	glEnableVertexAttribArray(posAttrib);
	glVertexAttribPointer(posAttrib, 1, GL_FLOAT, GL_FALSE, VALUES_PER_POINT * sizeof(GLfloat), (void*)(18 * sizeof(GLfloat)));

	// Create transformations
	model = glm::mat4(1.0f);
	view = glm::mat4(1.0f);
	projection = glm::mat4(1.0f);
	projection = glm::perspective(glm::radians(45.0f), (GLfloat)width / (GLfloat)height, 0.1f, 1000.0f);

	// Get their uniform location
	modelLoc = glGetUniformLocation(shaderProgram, "model");
	viewLoc = glGetUniformLocation(shaderProgram, "view");
	projLoc = glGetUniformLocation(shaderProgram, "projection");

	/*
	GLint tex_checkerboard = glGetUniformLocation(shaderProgram, "tex_checkerboard");
	GLint tex_color = glGetUniformLocation(shaderProgram, "tex_color");

	glBindTexture(GL_TEXTURE_2D, tex_checkerboard);
	// Allocate storage for the texture data
	glTexStorage2D(GL_TEXTURE_2D, 4, GL_R8, 8, 8);
	// Specify the data for the texture
	glTexSubImage2D(GL_TEXTURE_2D,
	0,
	0, 0,
	8, 8,
	GL_RED, GL_UNSIGNED_BYTE,
	tex_checkerboard_data);

	glBindTexture(GL_TEXTURE_2D, tex_color);
	// Allocate storage
	glTexStorage2D(GL_TEXTURE_2D, 2, GL_RGBA32F, 2, 2);
	// Specify the data
	glTexSubImage2D(GL_TEXTURE_2D,
	0,
	0, 0,
	2, 2,
	GL_RGBA, GL_FLOAT,
	tex_color_data);*/

	return 0;
}

extern "C" __declspec(dllexport) int send_data() {
	glBindBuffer(GL_ARRAY_BUFFER, vbo);
	glBufferData(GL_ARRAY_BUFFER, sizeof(float)*VALUES_PER_POINT*n_points, points, GL_STATIC_DRAW);
	return 0;
}

int citysize = 10;
int cycle_n = 0;
int p_npoints = 0;

extern "C" __declspec(dllexport) int end_cycle() {
	if (!glfwWindowShouldClose(window)) {
		return -1;
	}
	else {
		glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		glfwTerminate();

		free(points);
		p_npoints = n_points;
		n_points = 0;
		citysize = 10;
		cycle_n = 0;
		printf("p_npoints = %d\n", p_npoints);
		//exit(0);
		return 1;
	}

}

extern "C" __declspec(dllexport) void pool() {
	//printf("POOL Thread ID - %x ---- PID - %d \n", pthread_self(), getpid());
	glfwPollEvents();
	do_movement();
}

extern "C" __declspec(dllexport) void cycle() {

	//int width, height;
	//glfwGetFramebufferSize(window, &width, &height);
	//glViewport(0, 0, width, height);
	//printf("Testing Cycle %d\n",TestCycle++);
	if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS)
		glfwSetWindowShouldClose(window, GL_TRUE);

	//        if (glfwGetKey(window, GLFW_KEY_H) == GLFW_PRESS){

	/*if ( cycle_n++ == -10 ){
	cycle_n = 0;
	n_points = 0;
	city(citysize);
	send_data();
	citysize = (citysize+5)%100;
	}*/
	//            printf("citysize: %d\n",citysize );
	//        }
	/*        if (glfwGetKey(window, GLFW_KEY_J) == GLFW_PRESS){
	n_points = 0;
	citysize-=20;
	city(citysize);
	send_data();
	printf("citysize: %d\n",citysize );
	}*/
	// Clear the colorbuffer
	glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	//       posLookAt = glm::vec3(0.0f);
	view = glm::lookAt(cameraPos, posLookAt, cameraUp);
	//view = glm::lookAt(glm::vec3(camX, camY, 0.0f), glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f));//        model = glm::mat4(1.0f);
	//view = glm::mat4(1.0f);
	//projection = glm::mat4(1.0f);



	GLint camPos = glGetUniformLocation(shaderProgram, "cameraPos");
	GLint lookPos = glGetUniformLocation(shaderProgram, "lookat");
	GLint mvpLoc = glGetUniformLocation(shaderProgram, "MVP");

	// Pass them to the shaders
	glm::mat4 mvp = projection * view * model;
	glUniformMatrix4fv(mvpLoc, 1, GL_FALSE, glm::value_ptr(mvp));
	//glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(model));
	//glUniformMatrix4fv(viewLoc, 1, GL_FALSE, glm::value_ptr(view));
	//glUniformMatrix4fv(projLoc, 1, GL_FALSE, glm::value_ptr(projection));
	glUniform3fv(camPos, 1, glm::value_ptr(cameraPos));
	glUniform3fv(lookPos, 1, glm::value_ptr(cameraFront));

	//printf("Testing n_points %d\n",n_points);
	//        glEnable(GL_LINE_SMOOTH);
	//        glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
	glDrawArrays(GL_POINTS, 0, n_points);


	/*        n_points=0;
	city(100);
	send_data();
	glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
	glDrawArrays(GL_POINTS, 0, n_points);
	/*
	//glfwSwapBuffers(window);

	/*
	glViewport(width-400, height - 400, 300, 300);

	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
	//glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);


	view = glm::lookAt(glm::vec3(0.0f, 0.0f, 100.0f), glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(0.0f, 1.0f, 0.0f));
	//view = glm::lookAt(glm::vec3(camX, camY, 0.0f), glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f));//        model = glm::mat4(1.0f);
	//view = glm::mat4(1.0f);
	//projection = glm::mat4(1.0f);

	// Pass them to the shaders
	glUniformMatrix4fv(viewLoc, 1, GL_FALSE, glm::value_ptr(view));
	glUniform3fv(camPos, 1, glm::value_ptr(cameraPos));
	glUniform3fv(lookPos, 1, glm::value_ptr(cameraFront));

	//printf("Testing n_points %d\n",n_points);

	glDrawArrays(GL_POINTS, 0, n_points);
	*/
	glfwSwapBuffers(window);

	calcFPS(window);

}

extern "C" __declspec(dllexport) int start() {
	//init(200);

	//printf("CENAS\n");
	//fflush(stdout);
	int coisas = 0;
	int frame = 0;
	while (!glfwWindowShouldClose(window))
	{
		glfwPollEvents();

		//printf("STUF\n");
		//fflush(stdout);

		do_movement();

		//printf("CENAS\n");
		//fflush(stdout);
		/*if(frame++>100)
			glfwSetWindowShouldClose(window, GL_TRUE);
			*/
		if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS)
			glfwSetWindowShouldClose(window, GL_TRUE);

		if (glfwGetKey(window, GLFW_KEY_SPACE) == GLFW_PRESS)
			shader1 = !shader1;


		if (shader1)
			glUseProgram(shaderProgram);
		else
			glUseProgram(shaderProgram2);
		// Clear the colorbuffer
		glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);



		posLookAt = glm::vec3(0.0f);
		view = glm::lookAt(cameraPos, posLookAt, cameraUp);
		//view = glm::lookAt(glm::vec3(camX, camY, 0.0f), glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f));//        model = glm::mat4(1.0f);
		//view = glm::mat4(1.0f);
		//projection = glm::mat4(1.0f);


		GLint camPos = glGetUniformLocation(shaderProgram, "cameraPos");
		GLint mvpLoc = glGetUniformLocation(shaderProgram, "MVP");

		// Pass them to the shaders
		glm::mat4 mvp = projection * view * model;
		glUniformMatrix4fv(mvpLoc, 1, GL_FALSE, glm::value_ptr(mvp));
		//glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(model));
		//glUniformMatrix4fv(viewLoc, 1, GL_FALSE, glm::value_ptr(view));
		//glUniformMatrix4fv(projLoc, 1, GL_FALSE, glm::value_ptr(projection));
		glUniform3fv(camPos, 1, glm::value_ptr(cameraPos));

		//glPatchParameteri(GL_PATCH_VERTICES, 4);       // tell OpenGL that every patch has 16 verts
		//glDrawArrays(GL_PATCHES, 0, 5);
		//printf("Testing n_points %d\n",n_points);

		//printf("CENAS\n");
		//fflush(stdout);

		glDrawArrays(GL_POINTS, 0, n_points);

		//printf("CENAS\n");
		//fflush(stdout);

		glfwSwapBuffers(window);
		//showFPS();
		//usleep(10);
		calcFPS(window);

	}

	glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glfwTerminate();

	free(points);
	p_npoints = n_points;
	n_points = 0;
	citysize = 10;
	printf("p_npoints = %d\n", p_npoints);


	return p_npoints;
}


#define PI          3.14159265358979
#define BLACK   glm::vec3(0.0f,0.0f,0.0f)
#define WHITE   glm::vec3(1.0f,1.0f,1.0f)
#define GREY    glm::vec3(0.5f,0.5f,0.5f)
#define GREEN   glm::vec3(0.4f,0.5f,0.0f)
#define RED     glm::vec3(1.0f,0.2f,0.2f)
#define YELLOW  glm::vec3(1.0f,1.0f,0.0f)
#define BLUE    glm::vec3(0.2f,0.2f,1.0f)
#define WATER   glm::vec3(0.118f,0.298f,0.4f)
#define STEEL   glm::vec3(0.439f,0.502f,0.564f)
#define CYAN    float {0.0f,0.0f,0.0f}

void forest(int size);

float gaussiana2d(float x, float y, float sigma) {
	return exp(-(pow(x / sigma, 2) + pow(y / sigma, 2)));
}


int NUMBER_OF_BUILDINGS = 0;




glm::vec3 buildingColor() {
	int val = glm::linearRand(0, 10);
	switch (val) {
	case 0:
		return GREY;
	case 1:
		return STEEL;
	case 2:
		return GREY*0.5f;
	case 9:
		return STEEL;
	default:
		return GREY*0.1f*(float)val;
	}
}

void city(int size) {
	float h, h2;
	int forestSize = 30;
	for (int i = -2 * size; i < 2 * size; i += 8)
	{
		for (int j = -2 * size; j < 2 * size; j += 8)
		{
			if (std::abs(i)<forestSize * 3 + 4 && std::abs(j)<forestSize + 4)
				continue;
			NUMBER_OF_BUILDINGS++;

			h = 10 * (2 + 7 * fmax(gaussiana2d(i + 110, j, 30.2), gaussiana2d(i + 140, j, 2.2)));
			h *= std::abs(glm::simplex(glm::vec2(i / 16.f, j / 16.f)));
			//box(i, j, 0.0f, 5.5f, 5.5f, h);
			buildingv(glm::linearRand(i - 1.0f, i + 1.0f), glm::linearRand(j - 1.0f, j + 1.0f), 0.0f, 5.5f, 5.5f, h, glm::linearRand(1.0, 5.0), buildingColor());
		}

	}
	boxv(0.0f, 0.0f, 0.0f, 6 * size, 6 * size, 0.01, GREY*0.2f);
	forest(forestSize);
}

void forest(int size) {
	float h, h2;
	for (int i = -3 * size; i < 3 * size; i += 4)
	{
		for (int j = -size; j < size; j += glm::linearRand(2, 4))
		{
			NUMBER_OF_BUILDINGS++;
			h = 0.3 + fmax(fmax(gaussiana2d(i - 10, j, 25.2), gaussiana2d(i + 50, j - 20, 18.2)), gaussiana2d(i - 70, j + 10, 15.2));
			//h *= 3;
			//h = std::abs(glm::simplex(glm::vec2(i / 16.f, j / 16.f)));
			if (h<0.45)
			{
				if (h>0.4)
				{
					boxv(i + glm::linearRand(-2, 2), j, 0.15f, 2.2f, 5.2f, 0.1f, GREY);
					if (h>0.4 && h<0.43)
						boxv(i + glm::linearRand(-2, 2), j, 0.0f, 0.2f, 0.2f, 2.0f, glm::linearRand(0, 1)<1 ? RED : BLUE);
				}
			}
			else
				tree(i + glm::linearRand(-2, 2), j, 0.0f, h, h, glm::linearRand(2.0, 5.0), 0.0f, GREY);
		}

	}
	for (int i = -3 * size; i <= 3 * size; i += glm::linearRand(2, 4))
	{
		for (int j = -size; j <= size; j += 2 * size)
		{
			NUMBER_OF_BUILDINGS++;
			h = 1.0f;

			tree(i, j, 0.0f, h, h, glm::linearRand(1.0, 3.0), 0.0f, GREY);
		}

	}
	boxv(0.0f, 0.0f, 0.0f, 9 * size, 3 * size, 0.1, GREEN);
	pyramid(64.0f, 20.0f, 0.1f, 40.0, 15.0, -0.1, 13.0, WATER);
	pyramid(53.0f, 14.0f, 0.1f, 16.0, 29.0, -0.1, 7.0, WATER);
	pyramid(-50.0f, -17.0f, 0.1f, 40.0, 20.0, -0.1, 7.0, WATER);

	pyramid(-80.0f, 0.0f, 0.1f, 15.0, 30.0, -0.1, 20.0, WHITE);
}



int CITY_SIZE = 200;
extern "C" __declspec(dllexport) int main() {
	printf("A - S - W - D to move R - F or scrol for camera height\n");
	printf("Insert city radius (NUMBER_OF_BUILDINGS = (radius*2)^2):\n");
	//scanf("%d",&CITY_SIZE);
	int TestCycle = 1;
	//

	/*init(CITY_SIZE*2*CITY_SIZE*2);

	city(CITY_SIZE);
	*/

	init(500);
	//building(0.0,   0.0,   -9.0, 13.0, 2.5,3.5);
	//tree(0.0,0.0,-5.0,5.0,5.0,10.0,5);
	//forest(40);
	city(CITY_SIZE);
	//sphere(0.0, 0.0, 0.0, 4.0);


	/*move(rotate(box(0.0,   0.0,   0.0, 1.0, 1.0,2.0),PI/3,1.0,0.0,0.0),3.0,0.0,0.0);
	scale(rotate(box(0.0,   0.0,   0.0, 1.0, 1.0,2.0),PI/3,0.0,1.0,0.0),1.0,1.0,3.0);
	rotate(box(0.0,   0.0,   0.0, 1.0, 1.0,2.0),PI/3,1.0,1.0,0.0);
	rotate(sphere(0.0,  0.0,   0.0, 2.0),PI/2,1.0,0.0,0.0);*/
	//init(500);
	//createPoints(100);
	//cylinder(0.0,  0.0,  0.0, 1.0, 10.0, 1.0, 0.0, 0.0, PI/2,0.0,1.0,0.0);
	//rotate(box(0.0,   0.0,   0.0, 10.0, 1.0,1.0, BLUE),PI/3,1.0,1.0,1.0);
	//box(0.0,   0.0,   -9.0, 3.0, 2.5,7.5, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 0.0);

	//sphere(0.0, 0.0, 0.0, 4.0);
	/*cylinder(2.0,  6.0,  0.0, 1.0, 5.0);
	cylinder(2.0,  1.0,  2.0, 2.0, 2.0);
	cylinder(4.0,  16.0, 1.0, 0.5, 7.0);
	cylinder(1.0,  6.0,  1.0, 0.5, 5.0);

	pyramid(1.0,  6.0,  8.0, 0.5,0.5, 2.0,4);

	printf("TEST %d\n", TestCycle++);*/
	//move(sphere(0.0,  0.0,   0.0, 0.7),7.0,0.0,0.0);
	//move(sphere(0.0,  0.0,   0.0, 0.9),10.0,1.0,0.0);
	// sphere(0.0,  0.0,   0.0, 10.0);
	//move(sphere(0.0,  0.0,   0.0, 0.3),15.0,-10.0,0.0);
	//move(sphere(0.0,  0.0,   0.0, 0.2),17.0,0.0,0.0);
	/*box(0.0,   0.0,   0.0, 1.0, 1.0,0.6);
	cylinder(2.0,  -6.0,   -1.0, 1.0, 1.2);
	cylinder(0.0,  -12.0,   -1.0, 1.0, 1.2);
	cylinder(-2.0,  -18.0,   -1.0, 1.0, 1.2);
	cylinder(-4.0,  -24.0,   -1.0, 1.0, 1.2);*/
	int n_points = 0;

	//city(10);

	send_data();
	printf("TEST %d\n", TestCycle++);
	while (end_cycle()<0) {
		pool();
		cycle();
		//showFPS();
	}
	//start();

	//for (int i = 0; i < p_npoints; ++i)
	//{
	//    printObject(i);    
	//}

	//printMatrix(glm::translate(glm::scale(glm::mat4(),glm::vec3(1.0f)),glm::vec3(3.0f)));
	//printObject(1);

	printf("NUMBER_OF_BUILDINGS=%d NUMBER_OF_BLOCKS=%d\n", NUMBER_OF_BUILDINGS, n_points);

	return 0;
	//start();

}
