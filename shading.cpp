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

//#include "main.h"
#include "TrackballControls.h"
#include "shaders.h"
#include "poly2tri.h"
//#include "main.h"

const float PI = 3.1415926;

#define VALUES_PER_POINT 20
int n_points = 0;
int vecSize = 10;
GLfloat* points = (float *)calloc(sizeof(float), VALUES_PER_POINT * 10);
GLfloat* points_lines = (float *)calloc(sizeof(float), VALUES_PER_POINT * 10);


// Camera
glm::vec3 cameraPos = glm::vec3(0.0f, 70.0f, 30.0f);
glm::vec3 cameraFront = glm::vec3(0.0f, -1.0f, 0.0f);
glm::vec3 cameraUp = glm::vec3(0.0f, 0.0f, 1.0f);
glm::vec3 posLookAt = glm::vec3(0.0f);
bool keys[1024];

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

void printMatrix(glm::mat3 mat) {
	printf("%s\n", glm::to_string(mat).c_str());
}

void printVec(glm::vec3 vec) {
	printf("%s\n", glm::to_string(vec).c_str());
}

void printMatrixV(int n) {
	printf("\n");
	for (int i = 0; i < 16; i++) {
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
	glm::mat4 mat, float type, float n_sides, float scale, glm::vec3 color, float ratio = 1.0) {

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

	if (color[0] > 1.0 || color[1] > 1.0 || color[2] > 1.0)
		color /= 250.0f;

	//printf("%f %f\n", points[n*VALUES_PER_POINT+15], mat[1][1]);
	points[n*VALUES_PER_POINT + 3] = color[0];
	points[n*VALUES_PER_POINT + 7] = color[1];
	points[n*VALUES_PER_POINT + 11] = color[2];

	points[n*VALUES_PER_POINT + 16] = type;
	points[n*VALUES_PER_POINT + 17] = n_sides;
	points[n*VALUES_PER_POINT + 18] = scale;
	points[n*VALUES_PER_POINT + 19] = ratio;

	//printMatrix(mat);
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
	//ret = glm::transpose(ret);
	//printMatrix(ret);

/*	glm::mat4 ret(vx.x, vy.x, vz.x, 0.0f, vx.y, vy.y, vz.y, 0.0f, vx.z, vy.z, vz.z, 0.0f, o.x, o.y, o.z, 1.0f);
	ret[0][0] = vx.x; ret[0][1] = vx.y; ret[0][2] = vx.z; ret[0][3] = o.x;
	ret[1][0] = vy.x; ret[1][1] = vy.y; ret[1][2] = vy.z; ret[1][3] = o.x;
	ret[2][0] = vz.x; ret[2][1] = vz.y; ret[2][2] = vz.z; ret[2][3] = o.x;
	ret[3][0] = 0.0f; ret[3][1] = 0.0f; ret[3][2] = 0.0f; ret[3][3] = 1.0f;*/

	return ret1;
}

glm::vec3 vpol(float rho, float phi) {
	return glm::vec3(rho * cos(phi), rho * sin(phi), 0.0f);
}

glm::mat4 buildTMatrixFromPoints(float pos_x, float pos_y, float pos_z, float pos_x_2, float pos_y_2, float pos_z_2) {

	glm::vec3 n, o, vx, vy, vz;
	n = glm::vec3(pos_x_2, pos_y_2, pos_z_2) - glm::vec3(pos_x, pos_y, pos_z);
	o = glm::vec3(pos_x + pos_x_2, pos_y + pos_y_2, pos_z + pos_z_2) / 2.0f;
	//glm::vec3(pos_x, pos_y, pos_z); //
	printf("Vecs o:\n");
	printVec(o);
	vx = vpol(1.0f, glm::atan(n.y, n.x) + (PI / 2.0f));
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
			  */
	printf("Vecs x:\n");
	printVec(vx);
	printf("Vecs y:\n");
	printVec(vy);
	printf("Vecs z:\n");
	printVec(vz);

	return matFromVecs(o, vx, vy, vz);
}

glm::mat4 buildTMatrixFromIrregularPoint(float x_bottom, float y_bottom, float z_bottom, float x_up, float y_up, float z_up,
	float p_1_length, float p_1_angle, float p_2_length, float p_2_angle, float p_3_length, float p_3_angle) {

	glm::mat4 ret1(
		x_bottom, y_bottom, z_bottom, 0.0f,
		p_1_length, p_2_length, p_3_length, 0.0f,
		p_1_angle, p_2_angle, p_3_angle, 0.0f,
		x_up, y_up, z_up, 1.0f);

	//printMatrix(ret1);
	return ret1;

}

glm::mat4 listToMat4(float *trs) {
	return glm::mat4(
	glm::vec4(trs[0], trs[4], trs[8], trs[12]),
	glm::vec4(trs[1], trs[5], trs[9], trs[13]),
	glm::vec4(trs[2], trs[6], trs[10], trs[14]),
	glm::vec4(trs[3], trs[7], trs[11], trs[15]));
	/*return glm::mat4(
		glm::vec4(trs[0], trs[4], trs[8], trs[12]),
		glm::vec4(trs[1], trs[5], trs[9], trs[13]),
		glm::vec4(trs[2], trs[6], trs[10], trs[14]),
		glm::vec4(0.0f, 0.0f, 0.0f, 1.0f));*/	
}

float getType(int n) {
	return points[n*VALUES_PER_POINT + 16];
}

float getSides(int n) {
	return points[n*VALUES_PER_POINT + 17];
}

float getScale(int n) {
	return 	points[n*VALUES_PER_POINT + 18];
}

glm::vec3 getColor(int n) {
	return glm::vec3(points[n*VALUES_PER_POINT + 3], points[n*VALUES_PER_POINT + 7], points[n*VALUES_PER_POINT + 11]);
}



glm::mat4 buildMirroredMatrixOf(int n, float * pt, float * vec) {
	float x = pt[0], y = pt[1], z = pt[2];
	float a = vec[0], b = vec[1], c = vec[2];

	float d = -a*x - b*y - c*z;
	glm::mat4 mat2(-2 * a*a + 1, -2 * b*a, -2 * c*a, 0,
		-2 * a*b, -2 * b*b + 1, -2 * c*b, 0,
		-2 * a*c, -2 * b*c, -2 * c*c + 1, 0,
		-2 * a*d, -2 * b*d, -2 * c*d, 1);

	if (getType(n) < 12) {

		glm::mat4 mat1(points[n*VALUES_PER_POINT + 0], points[n*VALUES_PER_POINT + 1], points[n*VALUES_PER_POINT + 2], 0.0f,
			points[n*VALUES_PER_POINT + 4], points[n*VALUES_PER_POINT + 5], points[n*VALUES_PER_POINT + 6], 0.0f,
			points[n*VALUES_PER_POINT + 8], points[n*VALUES_PER_POINT + 9], points[n*VALUES_PER_POINT + 10], 0.0f,
			points[n*VALUES_PER_POINT + 12], points[n*VALUES_PER_POINT + 13], points[n*VALUES_PER_POINT + 14], 1.0f);

		return mat2 * mat1;
	}
	else if (getType(n) > 11) {

		glm::vec4 v1(points[n*VALUES_PER_POINT + 0], points[n*VALUES_PER_POINT + 1], points[n*VALUES_PER_POINT + 2], 1.0f);
		glm::vec4 v2(points[n*VALUES_PER_POINT + 4], points[n*VALUES_PER_POINT + 5], points[n*VALUES_PER_POINT + 6], 1.0f);
		glm::vec4 v3(points[n*VALUES_PER_POINT + 8], points[n*VALUES_PER_POINT + 9], points[n*VALUES_PER_POINT + 10], 1.0f);
		glm::vec4 v4(points[n*VALUES_PER_POINT + 12], points[n*VALUES_PER_POINT + 13], points[n*VALUES_PER_POINT + 14], 1.0f);


		glm::mat4 mat1(mat2 * v1, mat2 * v2, mat2 * v3, mat2 * v4);
		/*printMatrix(mat1);
		printMatrix(mat2);
		printMatrix(mat1 * mat2);
		printMatrix(mat2 * mat1);*/
		return mat1;
	}


}


float scaleFromPointsList(float * pts) {
	return pts[0] - pts[3];
}

glm::mat4 buildTMatrixFromPointsList(int n, float * pts) {
	glm::mat4 ret(1.0f);

	int j = 0;
	for (int i = 0; i < n; i++) {
			ret[i][0] = pts[j++]; ret[i][1] = pts[j++]; ret[i][2] = pts[j++];
	}
	return ret;
}

glm::mat4 buildTMatrixFromPointVec(float pos_x, float pos_y, float pos_z, float vec_x, float vec_y, float vec_z) {

	glm::vec3 n, o, vx, vy, vz;
	n = glm::vec3(vec_x, vec_y, vec_z) - glm::vec3(pos_x, pos_y, pos_z);
	o = glm::vec3(pos_x, pos_y, pos_z);
	//glm::vec3(pos_x, pos_y, pos_z); //

	vx = vpol(1.0f, glm::atan(n.y, n.x) - (PI / 2.0));
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

	return matFromVecs(o, vx, vy, vz);
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
extern "C" __declspec(dllexport) int point(float pos_x, float pos_y, float pos_z, float w, float red, float g, float b) {
	//printf("Build Box\n");
	return buildPoint(n_points, glm::scale(glm::translate(glm::mat4(1.0f), glm::vec3(pos_x, pos_y, pos_z)), glm::vec3(w, w, w)),
		0, 4, w, glm::vec3(red, g, b));
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

extern "C" __declspec(dllexport) int trunkpts(float pos_x, float pos_y, float pos_z, float pos_x_2, float pos_y_2, float pos_z_2, float l, float w, float h, float w1, float h1, float sides, float red, float g, float b) {
	//printf("Build Cylinder\n");
	return buildPoint(n_points, glm::scale(buildTMatrixFromPoints(pos_x, pos_y, pos_z, pos_x_2, pos_y_2, pos_z_2), glm::vec3(l, w, h)),
		9, sides, w, glm::vec3(red, g, b), w / w1);
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
extern "C" __declspec(dllexport) int pyramid(float pos_x, float pos_y, float pos_z, float w, float l, float h, float sides, float red, float g, float b) {
	return buildPoint(n_points, glm::scale(
		glm::translate(
			glm::mat4(),
			glm::vec3(pos_x, pos_y, pos_z)),
		glm::vec3(w, l, h)),
		4, sides, w, glm::vec3(red, g, b));
}
extern "C" __declspec(dllexport) int pyramidpts(float pos_x, float pos_y, float pos_z, float pos_x_2, float pos_y_2, float pos_z_2, float w, float l, float h, float sides, float red, float g, float b) {
	return buildPoint(n_points, glm::scale(buildTMatrixFromPoints(pos_x, pos_y, pos_z, pos_x_2, pos_y_2, pos_z_2), glm::vec3(l, w, h)),
		4, sides, w, glm::vec3(red, g, b));
}
extern "C" __declspec(dllexport) int pyramidv(float pos_x, float pos_y, float pos_z, float w, float l, float h, float sides, glm::vec3 color) {
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

extern "C" __declspec(dllexport) int regSurface(float pos_x, float pos_y, float pos_z, float pos_x_2, float pos_y_2, float pos_z_2,
	float sides, float w, float l, float red, float g, float b, float angle) {
	//printf("Build Cylinder\n");  glm::scale(buildTMatrixFromPoints(pos_x, pos_y, pos_z, pos_x_2, pos_y_2, pos_z_2)
	return buildPoint(n_points, glm::scale(buildTMatrixFromPointVec(pos_x, pos_y, pos_z, pos_x_2, pos_y_2, pos_z_2), glm::vec3(w, l, 1.0f)),
		10, sides, l, glm::vec3(red, g, b), angle);
}

extern "C" __declspec(dllexport) int regLine(float pos_x, float pos_y, float pos_z, float pos_x_2, float pos_y_2, float pos_z_2,
	float sides, float w, float l, float red, float g, float b, float angle) {
	return buildPoint(n_points, glm::scale(buildTMatrixFromPointVec(pos_x, pos_y, pos_z, pos_x_2, pos_y_2, pos_z_2), glm::vec3(w, l, 1.0f)),
		11, sides, l, glm::vec3(red, g, b), angle);
}


extern "C" __declspec(dllexport) int irregularPyramid3(float x_bottom, float y_bottom, float z_bottom, float x_up, float y_up, float z_up,
	float p_1_length, float p_1_angle, float p_2_length, float p_2_angle, float p_3_length, float p_3_angle,
	float r, float g, float b) {
	//printf("Build Box\n");
	return buildPoint(n_points, buildTMatrixFromIrregularPoint(x_bottom, y_bottom, z_bottom, x_up, y_up, z_up,
		p_1_length, p_1_angle, p_2_length, p_2_angle, p_3_length, p_3_angle),
		12, 3, p_1_length, glm::vec3(r, g, b));
}

extern "C" __declspec(dllexport) int line(int n, float *pts, float r, float g, float b) {
	printf("List n: %d\n", n);
	return buildPoint(n_points, buildTMatrixFromPointsList(n, pts), 13, n, scaleFromPointsList(pts), glm::vec3(r, g, b));
}


extern "C" __declspec(dllexport) int triangle(float *pts, float r, float g, float b) {

	return 	buildPoint(n_points, buildTMatrixFromPointsList(3, pts), 14, 3, scaleFromPointsList(pts), glm::vec3(r, g, b));
}

void triangulate(int n, float *pts, float r, float g, float b) {
	std::vector<p2t::Point*> polyline;

	for (int i = 0; i < n; i += 3)
	{
		polyline.push_back(new p2t::Point(pts[i], pts[i + 1]));
	}

	p2t::CDT* cdt = new p2t::CDT(polyline);
	cdt->Triangulate();
	std::vector<p2t::Triangle*> a = cdt->GetTriangles();
	for each (p2t::Triangle* var in a)
	{
		float tpoints[] = { var->GetPoint(0)->x, var->GetPoint(0)->y, var->GetPoint(1)->x, var->GetPoint(1)->y, var->GetPoint(2)->y, var->GetPoint(2)->y };
		triangle(tpoints, r, g, b);
	}

}

extern "C" __declspec(dllexport) void Polygon(int n, float *pts, float r, float g, float b) {
	triangulate(n, pts, r, g, b);
}

extern "C" __declspec(dllexport) int mirror(int n, float *pt, float *vec) {

	return buildPoint(n_points, buildMirroredMatrixOf(n, pt, vec), getType(n), getSides(n), getScale(n), getColor(n));
}

extern "C" __declspec(dllexport) int setView(float pos_x, float pos_y, float pos_z, float pos_x_2, float pos_y_2, float pos_z_2) {

	cameraPos = glm::vec3(pos_x, pos_y, pos_z);
	cameraUp = glm::vec3(0.0f, 0.0f, 1.0f);
	posLookAt = glm::vec3(pos_x_2, pos_y_2, pos_z_2);
	return 0;
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
		points[n*VALUES_PER_POINT + 12], points[n*VALUES_PER_POINT + 13], points[n*VALUES_PER_POINT + 14], 1.0f);

	glm::mat4 res = glm::rotate(mat1, angle, glm::vec3(vx / v_length, vy / v_length, vz / v_length));

	//printMatrix(res);

	points[n*VALUES_PER_POINT + 0] = res[0][0]; points[n*VALUES_PER_POINT + 1] = res[0][1]; points[n*VALUES_PER_POINT + 2] = res[0][2];//points[n*VALUES_PER_POINT+3]=res[0][3];
	points[n*VALUES_PER_POINT + 4] = res[1][0]; points[n*VALUES_PER_POINT + 5] = res[1][1]; points[n*VALUES_PER_POINT + 6] = res[1][2];//points[n*VALUES_PER_POINT+7]=res[1][3];
	points[n*VALUES_PER_POINT + 8] = res[2][0]; points[n*VALUES_PER_POINT + 9] = res[2][1]; points[n*VALUES_PER_POINT + 10] = res[2][2];//points[n*VALUES_PER_POINT+11]=res[2][3];
	points[n*VALUES_PER_POINT + 12] = res[3][0]; points[n*VALUES_PER_POINT + 13] = res[3][1]; points[n*VALUES_PER_POINT + 14] = res[3][2];//points[n*VALUES_PER_POINT+15]=res[3][3];

	memcpy(points+n*VALUES_PER_POINT*sizeof(float),glm::value_ptr(res),16*sizeof(float));

	//printf("ROTATE %f• (%f,%f,%f)\n", angle, vx / v_length, vy / v_length, vz / v_length);
	//printMatrixV(n);

	return n;
}

extern "C" __declspec(dllexport) int move(int n,
	float vx, float vy, float vz) {

	if (points[n*VALUES_PER_POINT + 16] == 5)
	{
		move(n + 1, vx, vy, vz);
		move(n + 2, vx, vy, vz);
	}


	glm::vec4 color(points[n*VALUES_PER_POINT + 3], points[n*VALUES_PER_POINT + 7], points[n*VALUES_PER_POINT + 11], points[n*VALUES_PER_POINT + 15]);

	glm::mat4 mat1 = glm::mat4(points[n*VALUES_PER_POINT + 0], points[n*VALUES_PER_POINT + 1], points[n*VALUES_PER_POINT + 2], 0.0f,
		points[n*VALUES_PER_POINT + 4], points[n*VALUES_PER_POINT + 5], points[n*VALUES_PER_POINT + 6], 0.0f,
		points[n*VALUES_PER_POINT + 8], points[n*VALUES_PER_POINT + 9], points[n*VALUES_PER_POINT + 10], 0.0f,
		points[n*VALUES_PER_POINT + 12], points[n*VALUES_PER_POINT + 13], points[n*VALUES_PER_POINT + 14], 1.0f);

	glm::mat4 res = glm::translate(mat1, glm::vec3(vx, vy, vz));
	
	memcpy(points + n*VALUES_PER_POINT*sizeof(float), glm::value_ptr(res), 16 * sizeof(float));

	points[n*VALUES_PER_POINT + 3] = color[0];
	points[n*VALUES_PER_POINT + 7] = color[1];
	points[n*VALUES_PER_POINT + 11] = color[2];

	return n;
}

extern "C" __declspec(dllexport) int scale(int n,
	float vx, float vy, float vz) {


	if (points[n*VALUES_PER_POINT + 16] == 5)
	{
		scale(n + 1, vx, vy, vz);
		scale(n + 2, vx, vy, vz);
	}

	glm::vec4 color(points[n*VALUES_PER_POINT + 3], points[n*VALUES_PER_POINT + 7], points[n*VALUES_PER_POINT + 11], points[n*VALUES_PER_POINT + 15]);

	glm::mat4 mat1 = glm::mat4(points[n*VALUES_PER_POINT + 0], points[n*VALUES_PER_POINT + 1], points[n*VALUES_PER_POINT + 2], 0.0f,
		points[n*VALUES_PER_POINT + 4], points[n*VALUES_PER_POINT + 5], points[n*VALUES_PER_POINT + 6], 0.0f,
		points[n*VALUES_PER_POINT + 8], points[n*VALUES_PER_POINT + 9], points[n*VALUES_PER_POINT + 10], 0.0f,
		points[n*VALUES_PER_POINT + 12], points[n*VALUES_PER_POINT + 13], points[n*VALUES_PER_POINT + 14], 1.0f);

	glm::mat4 res = glm::scale(mat1, glm::vec3(vx, vy, vz));

	memcpy(points + n*VALUES_PER_POINT*sizeof(float), glm::value_ptr(res), 16 * sizeof(float));
	
	points[n*VALUES_PER_POINT + 3] = color[0];
	points[n*VALUES_PER_POINT + 7] = color[1];
	points[n*VALUES_PER_POINT + 11] = color[2];
	
	return n;
}

extern "C" __declspec(dllexport) int transform(int n, float* trs) {

	if (points[n*VALUES_PER_POINT + 16] == 5)
	{
		transform(n + 1, trs);
		transform(n + 2, trs);
	}

	glm::vec4 color(points[n*VALUES_PER_POINT + 3], points[n*VALUES_PER_POINT + 7], points[n*VALUES_PER_POINT + 11], points[n*VALUES_PER_POINT + 15]);

	glm::mat4 mat1 = glm::mat4(points[n*VALUES_PER_POINT + 0], points[n*VALUES_PER_POINT + 1], points[n*VALUES_PER_POINT + 2], 0.0f,
		points[n*VALUES_PER_POINT + 4], points[n*VALUES_PER_POINT + 5], points[n*VALUES_PER_POINT + 6], 0.0f,
		points[n*VALUES_PER_POINT + 8], points[n*VALUES_PER_POINT + 9], points[n*VALUES_PER_POINT + 10], 0.0f,
		points[n*VALUES_PER_POINT + 12], points[n*VALUES_PER_POINT + 13], points[n*VALUES_PER_POINT + 14], 1.0f);

	//printMatrix(mat1);
	glm::mat4 tr_mat = listToMat4(trs);
	//printMatrix(tr_mat);
	glm::mat4 res = tr_mat * mat1;
	//printMatrix(res);



	memcpy(points + n*VALUES_PER_POINT, glm::value_ptr(res), 16 * sizeof(float));
	//memcpy(points + n*VALUES_PER_POINT, glm::value_ptr(mat), 16 * sizeof(float));
	points[n*VALUES_PER_POINT + 3] = color[0];
	points[n*VALUES_PER_POINT + 7] = color[1];
	points[n*VALUES_PER_POINT + 11] = color[2];
	//printMatrixV(n);
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


bool firstMouse = true;
const GLuint WIDTH = 1250, HEIGHT = 800;
GLfloat yaw = -90.0f;	// Yaw is initialized to -90.0 degrees since a yaw of 0.0 results in a direction vector pointing to the right (due to how Eular angles work) so we initially rotate a bit to the left.
GLfloat pitch = 0.0f;
GLfloat lastX = WIDTH / 2.0;
GLfloat lastY = HEIGHT / 2.0;
GLfloat fov = 45.0f;
double xpos_1, ypos_1, xpos_2, ypos_2, speed = 1;
GLenum pressed = GL_FALSE;

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

GLuint shaderProgram, shaderProgram1, shaderProgram2;
glm::mat4 model, view, projection;
GLint modelLoc, viewLoc, projLoc;
sasmaster::Camera3D* tCam;
//Init trackball instance :
sasmaster::TrackballControls* tball;


extern "C" __declspec(dllexport) int clean() {
	vecSize = 10;
	points = (float *)calloc(sizeof(float), VALUES_PER_POINT * 10);
	n_points = 0;
	return 0;
}

extern "C" __declspec(dllexport) int init(int n) {
	using namespace sasmaster;
	vecSize = n;
	points = (float *)calloc(sizeof(float), VALUES_PER_POINT*n);


	glfwInit();

	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
	glfwWindowHint(GLFW_DOUBLEBUFFER, GL_TRUE);
	glfwWindowHint(GLFW_RESIZABLE, GL_TRUE);
	glfwWindowHint(GL_DEPTH_BITS, 50);
	glfwWindowHint(GLFW_SAMPLES, 16);
	glfwWindowHint(GLFW_STICKY_KEYS, GL_TRUE);

	window = glfwCreateWindow(WIDTH, HEIGHT, "Fast OpenGL", NULL, NULL); // Windowed

	glfwMakeContextCurrent(window);

	int width, height;
	glfwGetFramebufferSize(window, &width, &height);
	glViewport(0, 0, width, height);

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

	glm::vec3 cameraPos = glm::vec3(0.0f, 70.0f, 30.0f);
	// Compile and activate shaders
	GLuint vertexShader = createShader(GL_VERTEX_SHADER, vertexShaderSrc);

	GLuint geometryShader = createShader(GL_GEOMETRY_SHADER, geometryShaderSrc);
	GLuint geometryShader1 = createShader(GL_GEOMETRY_SHADER, geometryShaderSrc1);
	GLuint geometryShader2 = createShader(GL_GEOMETRY_SHADER, geometryShaderSrc2);

	GLuint fragmentShader = createShader(GL_FRAGMENT_SHADER, fragmentShaderSrc);

	GLuint tessEvalShader = createShader(GL_TESS_EVALUATION_SHADER, tesselationEvalShaderSrc);
	GLuint tessCtrlShader = createShader(GL_TESS_CONTROL_SHADER, tesselationCtrlShaderSrc);


	shaderProgram = glCreateProgram();
	shaderProgram1 = glCreateProgram();
	shaderProgram2 = glCreateProgram();

	glAttachShader(shaderProgram, vertexShader);
	glAttachShader(shaderProgram, geometryShader);
	glAttachShader(shaderProgram, fragmentShader);
	//glAttachShader(shaderProgram, tessEvalShader);
	//glAttachShader(shaderProgram, tessCtrlShader);
	glAttachShader(shaderProgram1, vertexShader);
	glAttachShader(shaderProgram1, geometryShader1);
	glAttachShader(shaderProgram1, fragmentShader);

	glAttachShader(shaderProgram2, vertexShader);
	glAttachShader(shaderProgram2, geometryShader2);
	glAttachShader(shaderProgram2, fragmentShader);

	glLinkProgram(shaderProgram);

	glLinkProgram(shaderProgram1);

	glLinkProgram(shaderProgram2);

	//glUseProgram(shaderProgram1);

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

	// Specify layout of point data
	posAttrib = glGetAttribLocation(shaderProgram, "ratio");
	glEnableVertexAttribArray(posAttrib);
	glVertexAttribPointer(posAttrib, 1, GL_FLOAT, GL_FALSE, VALUES_PER_POINT * sizeof(GLfloat), (void*)(19 * sizeof(GLfloat)));

	//init camera object:
	tCam = new Camera3D(cameraPos);
	//Init trackball instance :
	tball = &TrackballControls::GetInstance(tCam, glm::vec4(0.0f, 0.0f, width, height));
	//Init GLFW callbacks:
	tball->Init(window);

	// Camera/View transformation
	// view = glm::lookAt(cameraPos, cameraPos + cameraFront, cameraUp);
	// Projection     
	//projection = glm::perspective(fov, (GLfloat)WIDTH / (GLfloat)HEIGHT, 0.1f, 1000.0f);

	// Create transformations
	model = glm::mat4(1.0f);
	/*view = glm::mat4(1.0f);
	projection = glm::mat4(1.0f);
	projection = glm::perspective(glm::radians(45.0f), (GLfloat)width / (GLfloat)height, 0.1f, 1000.0f);
	*/
	// Get their uniform location
	//modelLoc = glGetUniformLocation(shaderProgram, "model");
	//viewLoc = glGetUniformLocation(shaderProgram, "view");
	//projLoc = glGetUniformLocation(shaderProgram, "projection");

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

extern "C" __declspec(dllexport) int close() {
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
		return 0;
}
extern "C" __declspec(dllexport) void pool() {
	//printf("POOL Thread ID - %x ---- PID - %d \n", pthread_self(), getpid());
	glfwPollEvents();
	// do_movement();
}

extern "C" __declspec(dllexport) void cycle() {

	int width, height;
	glfwGetFramebufferSize(window, &width, &height);
	glViewport(0, 0, width, height);

	if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS)
		glfwSetWindowShouldClose(window, GL_TRUE);	

	tball->Update();

	// Clear the colorbuffer
	glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);


	// Projection 
	projection = glm::perspective(fov, (GLfloat)WIDTH / (GLfloat)HEIGHT, 0.001f, 1000.0f);
	// View
	view = tCam->m_viewMatr;

	GLint camPos = glGetUniformLocation(shaderProgram, "cameraPos");
	GLint lookPos = glGetUniformLocation(shaderProgram, "lookat");
	GLint mvpLoc = glGetUniformLocation(shaderProgram, "MVP");

	// Pass them to the shaders
	glm::mat4 mvp = projection * view * model;

	switch (tball->shaderid)
	{
	case 0:
		glUseProgram(shaderProgram);
		glUniformMatrix4fv(mvpLoc, 1, GL_FALSE, glm::value_ptr(mvp));
		glUniform3fv(camPos, 1, glm::value_ptr(cameraPos));
		glUniform3fv(lookPos, 1, glm::value_ptr(cameraFront));
		glDrawArrays(GL_POINTS, 0, n_points);
		glUseProgram(shaderProgram2);
		glUniformMatrix4fv(mvpLoc, 1, GL_FALSE, glm::value_ptr(mvp));
		glUniform3fv(camPos, 1, glm::value_ptr(cameraPos));
		glUniform3fv(lookPos, 1, glm::value_ptr(cameraFront));
		glDrawArrays(GL_POINTS, 0, n_points);
		break;
	case 1:
		glUseProgram(shaderProgram1);
		glUniformMatrix4fv(mvpLoc, 1, GL_FALSE, glm::value_ptr(mvp));
		glUniform3fv(camPos, 1, glm::value_ptr(cameraPos));
		glUniform3fv(lookPos, 1, glm::value_ptr(cameraFront));
		glDrawArrays(GL_POINTS, 0, n_points);
		break;
	case 2:
		glUseProgram(shaderProgram1);
		glUniformMatrix4fv(mvpLoc, 1, GL_FALSE, glm::value_ptr(mvp));
		glUniform3fv(camPos, 1, glm::value_ptr(cameraPos));
		glUniform3fv(lookPos, 1, glm::value_ptr(cameraFront));
		glDrawArrays(GL_POINTS, 0, n_points);
		glUseProgram(shaderProgram);
		glUniformMatrix4fv(mvpLoc, 1, GL_FALSE, glm::value_ptr(mvp));
		glUniform3fv(camPos, 1, glm::value_ptr(cameraPos));
		glUniform3fv(lookPos, 1, glm::value_ptr(cameraFront));
		glDrawArrays(GL_POINTS, 0, n_points);
		break;
	default:
		glUseProgram(shaderProgram);
		glUniformMatrix4fv(mvpLoc, 1, GL_FALSE, glm::value_ptr(mvp));
		glUniform3fv(camPos, 1, glm::value_ptr(cameraPos));
		glUniform3fv(lookPos, 1, glm::value_ptr(cameraFront));
		glDrawArrays(GL_POINTS, 0, n_points);
		break;
	}


	glfwSwapBuffers(window);

	calcFPS(window);

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

extern "C" __declspec(dllexport) void city(int size) {
	float h, h2;
	int forestSize = 30;
	for (int i = -2 * size; i < 2 * size; i += 8)
	{
		for (int j = -2 * size; j < 2 * size; j += 8)
		{
			if (std::abs(i) < forestSize * 3 + 4 && std::abs(j) < forestSize + 4)
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
			if (h < 0.45)
			{
				if (h > 0.4)
				{
					boxv(i + glm::linearRand(-2, 2), j, 0.15f, 2.2f, 5.2f, 0.1f, GREY);
					if (h > 0.4 && h < 0.43)
						boxv(i + glm::linearRand(-2, 2), j, 0.0f, 0.2f, 0.2f, 2.0f, glm::linearRand(0, 1) < 1 ? RED : BLUE);
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
	pyramidv(64.0f, 20.0f, 0.1f, 40.0, 15.0, -0.1, 13.0, WATER);
	pyramidv(53.0f, 14.0f, 0.1f, 16.0, 29.0, -0.1, 7.0, WATER);
	pyramidv(-50.0f, -17.0f, 0.1f, 40.0, 20.0, -0.1, 7.0, WATER);

	pyramidv(-80.0f, 0.0f, 0.1f, 15.0, 30.0, -0.1, 20.0, WHITE);
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
	while (end_cycle() < 0) {
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
