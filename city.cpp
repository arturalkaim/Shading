#include <glm/gtx/transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtc/random.hpp>
#include <glm/gtc/noise.hpp>
#include <stdio.h>
#include <main.h>

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
extern "C" int main() {
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
	//box(0.0,   0.0,   -9.0, 3.0, 2.5,7.5);

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
	int n_points;

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

	//start();

}
