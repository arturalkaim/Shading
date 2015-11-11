#include <glm/glm.hpp>

extern "C" int building(float pos_x, float pos_y, float pos_z, float w, float l, float h, int divs, float r, float g, float b);

extern "C" int buildingv(float pos_x, float pos_y, float pos_z, float w, float l, float h, int divs, glm::vec3 color);

extern "C" int tree(float pos_x, float pos_y, float pos_z, float w, float l, float h, int divs, glm::vec3 color);

extern "C" int boxv(float pos_x, float pos_y, float pos_z, float w, float l, float h, glm::vec3 color);

extern "C" int box(float pos_x, float pos_y, float pos_z, float w, float l, float h, float red, float g, float b, float angle, float vx, float vy, float vz);

extern "C" int prism(float pos_x, float pos_y, float pos_z, float l, float w, float h, float sides, float red, float g, float b, float angle, float vx, float vy, float vz);

extern "C" int cylinder(float pos_x, float pos_y, float pos_z, float r, float h, float red, float g, float b, float angle, float vx, float vy, float vz);

extern "C" int cylinderv(float pos_x, float pos_y, float pos_z, float r, float h, glm::vec3 color);

extern "C" int pyramid(float pos_x, float pos_y, float pos_z, float w, float l, float h, float sides, glm::vec3 color);

extern "C" int spherev(float pos_x, float pos_y, float pos_z, float r, glm::vec3 color);

extern "C" int sphere(float pos_x, float pos_y, float pos_z, float r, float red, float g, float b);

extern "C" int rotate(int n,
	float angle = 0.0f,
	float vx = 0.0f, float vy = 1.0f, float vz = 0.0f);

extern "C" int move(int n,
	float vx = 0.0f, float vy = 0.0f, float vz = 0.0f);

extern "C" int scale(int n,
	float vx = 0.0f, float vy = 0.0f, float vz = 0.0f);



extern "C" int init(int n);

extern "C" int send_data();

extern "C" int end_cycle();

extern "C" void pool();

extern "C" void cycle();

extern "C" __declspec(dllexport) int start();