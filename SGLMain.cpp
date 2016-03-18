

#define GLEW_STATIC
#include <thread>         // std::thread
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtc/random.hpp>
#include <glm/gtc/noise.hpp>
#include <iostream>
#include <sstream> 
//#include "main.h"

#define PI          3.14159265358979
/* shader functions defined in shader.c */
extern void shaderAttachFromFile(GLuint, GLenum, const char *);

// Shader macro
#define GLSL(src) "#version 410 core\n" #src

// Vertex shader + vec4(sin(time)/1,cos(time)/1,0,1);
const GLchar* sVertexShaderSrc = GLSL(
	in vec3 pos;

uniform mat4 view;
uniform mat4 projection;
uniform mat4 model;

void main() {
	gl_Position = projection * view * model * vec4(pos.x, pos.y, pos.z, 1.0); //projection * view * model * 

}
);


// Fragment shader
const GLchar* sFragmentShaderSrc = GLSL(
	out vec4 outColor;

void main() {
	outColor = vec4(0.8, 0.9, 0.8, 1.0);
}
);

void testError(GLchar* src) {
	GLenum err = glGetError();
	if (err != GL_NO_ERROR)
	{
	printf("(%s) Error: %s\n", src, gluErrorString(err));
	}
}

// Shader creation helper
GLuint createSShader(GLenum type, const GLchar* src) {
	GLint length, result;
	GLuint shader = glCreateShader(type);

	glShaderSource(shader, 1, &src, NULL);
	glCompileShader(shader);
	glGetShaderiv(shader, GL_COMPILE_STATUS, &result);
	if (result == GL_FALSE)
	{
		char *log;

		/* get the shader info log */
		glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &length);
		log = (char*)malloc(length);
		glGetShaderInfoLog(shader, length, &result, log);

		/* print an error message and the info log */
		fprintf(stderr, "shaderCompileFromFile(): Unable to compile %s\n: %s\n", src, log);
		free(log);

		glDeleteShader(shader);
		return 1;
	}

	return shader;
}

#define VALUES_PER_POINT 3
int N_POINTS = 0;
int N_OBJECTS = 0;
int vecSize = 0;
int vec2Size = 1;
GLfloat* points;
GLint* vertices;


float* buildPoints(int n) {
	float* ret = (float *)malloc(sizeof(float)*VALUES_PER_POINT*n);
	vecSize = n;
	for (int i = 0, j = 0; i < n; ++i, j = 0)
	{
		// pos-x, pos-y, type, n_sides, size-x, size-y
		ret[i*VALUES_PER_POINT + (j++)] = ((float)rand() / (RAND_MAX / 10)) - 2.5f;
		ret[i*VALUES_PER_POINT + (j++)] = ((float)rand() / (RAND_MAX / 10)) - 2.5f;
		ret[i*VALUES_PER_POINT + (j++)] = ((float)rand() / (RAND_MAX)) / 10;
	}


	return ret;
}

void addNVertices(int id, int nvertices) {
	if (N_OBJECTS >= vec2Size) {
		vertices = (int *)realloc(vertices, sizeof(GLint)*N_OBJECTS * 2);
		vec2Size = 2 * vec2Size;
		//printf("N_OBJECTS vec2Size %d  :  %d\n", vec2Size, N_OBJECTS);
	}
	//printf("id %d - nvertices %d\n", id, nvertices);
	vertices[id] = nvertices;

}

float* buildPoint(glm::vec4 pos) {
	N_POINTS++;

	if (N_POINTS >= vecSize) {
		points = (float *)realloc(points, sizeof(float)*VALUES_PER_POINT*vecSize * 2);
		vecSize = 2 * vecSize;
		if (points == NULL)
		{
			printf("NULL NULL NULL NULL!!!!!!!!!!!!!!!!!!!!!!!\n");
		}
		//printf("TEST %d  :  %d\n", vecSize, N_POINTS);
	}

	// pos-x, pos-y, type, n_sides, size-x, size-y
	points[N_POINTS*VALUES_PER_POINT + 0] = pos.x;
	points[N_POINTS*VALUES_PER_POINT + 1] = pos.y;
	points[N_POINTS*VALUES_PER_POINT + 2] = pos.z;

	return points;
}

void makeCyl(glm::vec4 aux, float width, float l, float h, int sides, glm::mat4 transMatrix) {
	// Safe, GLfloats can represent small integers exactly
	for (int i = 0; i <= sides; i++) {
		// Angle between each side in radians
		float ang = ((PI * 2.0) / sides * i) + (PI / 4);

		//gColor = glm::vec3(ang/15,ang/15,ang/15,1.0);

		// Offset from center of point (0.3 to accomodate for aspect ratio)
		glm::vec4 offset = glm::vec4(cos(ang) * width, -sin(ang) * l, -h, 1.0f);
		buildPoint(aux + transMatrix * offset);

		offset = glm::vec4(cos(ang) * width, -sin(ang) * l, h, 1.0f);
		buildPoint(aux + transMatrix * offset);


	}

	// Safe, GLfloats can represent small integers exactly
	for (int i = 0; i <= sides; i++) {
		// Angle between each side in radians
		float ang = ((PI * 2.0) / sides * i) + (PI / 4);
		if (i % 2 == (sides % 2))
			buildPoint(aux + transMatrix * glm::vec4(0.0f, 0.0f, -h, 1.0f));


		//gColor = glm::vec3(ang/7,ang/7,ang/7,1.0);
		// Offset from center of point (0.3 to accomodate for aspect ratio)
		buildPoint(aux + transMatrix * glm::vec4(cos(ang) * width, -sin(ang) * l, -h, 1.0f));

	}
	// Safe, GLfloats can represent small integers exactly
	for (int i = 0; i <= sides; i++) {
		// Angle between each side in radians
		float ang = ((PI * 2.0) / sides * i) + (PI / 4);
		if (i % 2 == (sides % 2))
			buildPoint(aux + transMatrix * glm::vec4(0.0f, 0.0f, h, 1.0f));

		//gColor = vec3(ang/12,ang/12,ang/12,1.0);
		// Offset from center of point (0.3 to accomodate for aspect ratio)
		buildPoint(aux + transMatrix * glm::vec4(cos(ang) * width, -sin(ang) * l, h, 1.0f));

	}
}

void makeSphere(glm::vec4 aux, float radius, int sides, glm::mat4 transMatrix) {
	float leap2 = (PI * 2.0) / float(sides);
	float leap = PI / float(sides);

	// Safe, GLfloats can represent small integers exactly
	for (int i = 0; i <= sides; i++) {
		float ang = leap * float(i);
		float ang1 = leap * float(i + 1);
		// Angle between each side in radians
		for (int j = 0; j <= sides; j++) {
			float ang2 = leap2 * float(j);
			float ang3 = leap2 * float(j + 1);
			//gColor = vec4((sin(ang)+1.0)/3,(sin(ang)+1.0)/3,(sin(ang)+1.0)/3,1.0);

			buildPoint(aux + transMatrix * glm::vec4(sin(ang)*cos(ang2) * radius, sin(ang)*sin(ang2) * radius, cos(ang) * radius, 1.0f));

			buildPoint(aux + transMatrix * glm::vec4(sin(ang1)*cos(ang2) * radius, sin(ang1)*sin(ang2) * radius, cos(ang1) * radius, 1.0f));

		}
	}
}




extern "C" __declspec(dllexport) int create_points(int in) {
	N_POINTS = in;
	points = buildPoints(in);
	return 0;
}

extern "C" __declspec(dllexport) int box2(float pos_x, float pos_y, float pos_z, float w, float l, float h, float red, float g, float b, float angle, float vx, float vy, float vz) {
	N_OBJECTS++;
	int before = N_POINTS;
	makeCyl(glm::vec4(0.0f, 0.0f, 0.0f, 1.0f), 1.0f, 1.0f, 1.0f, 4,
		glm::scale(glm::rotate(glm::translate(glm::mat4(1.0f), glm::vec3(pos_x, pos_y, pos_z)), angle, glm::vec3(vx, vy, vz)), glm::vec3(w, l, h))
		);
	addNVertices(N_OBJECTS, N_POINTS - before);
	return 0;
}
extern "C" __declspec(dllexport) int cylinder2(float pos_x, float pos_y, float pos_z, float r, float h, float red, float g, float b, float angle, float vx, float vy, float vz) {
	N_OBJECTS++;
	int before = N_POINTS;
	makeCyl(glm::vec4(0.0f, 0.0f, 0.0f, 1.0f), 0.5f, 0.5f, 0.5f, 20,
		glm::scale(glm::rotate(glm::translate(glm::mat4(1.0f), glm::vec3(pos_x, pos_y, pos_z)), angle, glm::vec3(vx, vy, vz)), glm::vec3(r, r, h))
		);
	addNVertices(N_OBJECTS, N_POINTS - before);
	return 0;
}

extern "C" __declspec(dllexport) int sphere2(float pos_x, float pos_y, float pos_z, float r, float red, float g, float b) {
	N_OBJECTS++;
	int before = N_POINTS;
	makeSphere(glm::vec4(0.0f, 0.0f, 0.0f, 1.0f), 1.0f, 20,
		glm::scale(
			glm::translate(glm::mat4(1.0f), glm::vec3(pos_x, pos_y, pos_z)),
			glm::vec3(r, r, r)));

	addNVertices(N_OBJECTS, N_POINTS - before);
	return 0;
}

float gaussiana2d(float x, float y, float sigma) {
	return exp(-(pow(x / sigma, 2) + pow(y / sigma, 2)));
}

int NUMBER_OF_BUILDINGS = 0;

extern "C" void the_city(int size) {
	float h, h2;
	for (int i = -size; i < size; ++i)
	{
		for (int j = -size; j < size; ++j)
		{
			NUMBER_OF_BUILDINGS++;

			for (int k = 0, ch = 0; k < rand() % 4; ++k, ch += h)
			{

				h = 3 * (0.4 + fmax(gaussiana2d(i, j, 25.2), gaussiana2d(i - 30, j - 30, 10.2)));
				h *= std::abs(glm::simplex(glm::vec2(i / 16.f, j / 16.f)));
				//glm::perlin(glm::vec4(i / 16.f, j / 16.f, 0.5f, 0.5f));
				//std::abs(glm::vec4(glm::gaussRand(glm::vec3(0), glm::vec3(1)), 1).z);
				//h = fmax(h,4*(0.3+gaussiana2d(i-12,j+40,10.2)));
				if (h < 0.05) {
					sphere2(i, j, ch + h, 0.4 - (0.08*k), 0.0f, 0.0f, 0.0f);
					break;
				}
				if (k > 1 && ((float)rand() / (RAND_MAX))>0.2)
					cylinder2(i, j, ch + h, 0.4 - (0.08*k), h, 1.0f, 1.0f, 1.0f, 0.0f, 1.0f, 0.0f, 0.0f);
				else
					box2(i, j, ch + h, 0.4 - (0.08*k), 0.4 - (0.1*k), h, 1.0f, 1.0f, 1.0f, 0.0f, 1.0f, 0.0f, 0.0f);
			}
		}

	}

}


// Camera
glm::vec3 cameraPos = glm::vec3(0.0f, 70.0f, 30.0f);
glm::vec3 cameraFront = glm::vec3(0.0f, -1.0f, 0.0f);
glm::vec3 cameraUp = glm::vec3(0.0f, 0.0f, 1.0f);
glm::vec3 posLookAt = glm::vec3(0.0f);
bool keys[1024];

void scroll_callback(GLFWwindow* window, double xoffset, double yoffset)
{
	cameraPos += ((GLfloat)yoffset * 0.1f) * cameraUp;
}
// Is called whenever a key is pressed/released via GLFW
void key_callback(GLFWwindow* window, int key, int scancode, int action, int mode)
{
	if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS)
		glfwSetWindowShouldClose(window, GL_TRUE);
	if (key >= 0 && key < 1024)
	{
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

GLint modelLoc, viewLoc, projLoc;
GLuint shaderProgram;
GLFWwindow* window;

void do_movement(GLFWwindow* window)
{
	if (pressed) {
		glfwGetCursorPos(window, &xpos_2, &ypos_2);
		posLookAt += glm::vec3((xpos_2 - xpos_1)*speed, (ypos_2 - ypos_1)*speed, 0.0f);
		//glfwGetCursorPos(window, &xpos_1, &ypos_1);

	}
	// Camera controls
	GLfloat cameraSpeed = 0.05f;
	if (keys[GLFW_KEY_W])
		cameraPos += cameraSpeed * cameraFront;
	if (keys[GLFW_KEY_S])
		cameraPos -= cameraSpeed * cameraFront;
	if (keys[GLFW_KEY_A])
		cameraPos -= glm::normalize(glm::cross(cameraFront, cameraUp)) * cameraSpeed * 10.0f;
	if (keys[GLFW_KEY_D])
		cameraPos += glm::normalize(glm::cross(cameraFront, cameraUp)) * cameraSpeed * 10.0f;
	if (keys[GLFW_KEY_R])
		cameraPos += cameraSpeed * cameraUp;
	if (keys[GLFW_KEY_F])
		cameraPos -= cameraSpeed * cameraUp;
	cameraFront = -cameraPos + posLookAt;
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

glm::mat4 model = glm::mat4(1.0f);
glm::mat4 view;
glm::mat4 projection;

GLuint vbo;

void go() {
	const GLuint WIDTH = 1250, HEIGHT = 800;
	printf("STUFF ON GO\n");

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

	window = glfwCreateWindow(WIDTH, HEIGHT, "Simple OpenGL", NULL, NULL); // Windowed

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
	testError("INIT2 ");


	//   glShadeModel(GL_SMOOTH);                            // Enable Smooth Shading
	//    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);                // Black Background
	glClearDepth(1.0f);                                    // Depth Buffer Setup
														   //    glViewport(0, 0, WIDTH, HEIGHT);
	glEnable(GL_DEPTH_TEST);                            // Enables Depth Testing
	glDepthFunc(GL_LEQUAL);                                


	GLuint vertexShader = createSShader(GL_VERTEX_SHADER, sVertexShaderSrc);

	//GLuint geometryShader = createSShader(GL_GEOMETRY_SHADER, geometryShaderSrc);

	GLuint fragmentShader = createSShader(GL_FRAGMENT_SHADER, sFragmentShaderSrc);

	//GLuint tessEvalShader = createSShader(GL_TESS_EVALUATION_SHADER, tesselationEvalShaderSrc);
	//GLuint tessCtrlShader = createSShader(GL_TESS_CONTROL_SHADER, tesselationCtrlShaderSrc);

	shaderProgram = glCreateProgram();

	glAttachShader(shaderProgram, vertexShader);
	//glAttachShader(shaderProgram, geometryShader);
	glAttachShader(shaderProgram, fragmentShader);
	//glAttachShader(shaderProgram, tessEvalShader);
	//glAttachShader(shaderProgram, tessCtrlShader);
	glLinkProgram(shaderProgram);

	testError("GOGO 000");

	// Create VBO with point coordinates

	glGenBuffers(1, &vbo);
	glBindBuffer(GL_ARRAY_BUFFER, vbo);
	glBufferData(GL_ARRAY_BUFFER, sizeof(float) * 3 * N_POINTS, points, GL_STATIC_DRAW);

	// Create VAO
	GLuint vao;
	glGenVertexArrays(1, &vao);
	glBindVertexArray(vao);

	// Specify layout of point data
	GLint posAttrib = glGetAttribLocation(shaderProgram, "pos");
	testError("posAttrib");
	glEnableVertexAttribArray(posAttrib);
	glVertexAttribPointer(posAttrib, 3, GL_FLOAT, GL_FALSE, VALUES_PER_POINT * sizeof(GLfloat), 0);


	// Create transformations


	projection = glm::perspective(45.0f, (GLfloat)width / (GLfloat)height, 0.1f, 300.0f);

	// Get their uniform location
	modelLoc = glGetUniformLocation(shaderProgram, "model");
	viewLoc = glGetUniformLocation(shaderProgram, "view");
	projLoc = glGetUniformLocation(shaderProgram, "projection");
}

extern "C" __declspec(dllexport) int send_data2() {
	glBindBuffer(GL_ARRAY_BUFFER, vbo);
	glBufferData(GL_ARRAY_BUFFER, sizeof(float)*VALUES_PER_POINT*N_POINTS, points, GL_STATIC_DRAW);
	return 0;
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


extern "C" __declspec(dllexport) int init2(int n) {
	vecSize = n;
	points = (float *)calloc(sizeof(float), VALUES_PER_POINT*n);
	//go();   
	return 0;
}

int p_npoints = 0;

extern "C" __declspec(dllexport) int end_cycle2() {

		glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		glfwTerminate();

		free(points);
		p_npoints = N_POINTS;
		N_POINTS = 0;
		printf("p_npoints = %d\n", p_npoints);
		//exit(0);
		return 1;
}

extern "C" void pool2() {
	//printf("POOL Thread ID - %x ---- PID - %d \n", pthread_self(), getpid());
	glfwPollEvents();
	do_movement(window);
}

extern "C"  __declspec(dllexport) float cycle2() {
	go();

	printf("START CYCLE2\n");
	glfwPollEvents();
	do_movement(window);

	if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS)
		glfwSetWindowShouldClose(window, GL_TRUE);

	glUseProgram(shaderProgram);

	// Clear the colorbuffer
	glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	view = glm::lookAt(cameraPos, posLookAt, cameraUp);
	//view = glm::lookAt(glm::vec3(camX, camY, 0.0f), glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f));//        model = glm::mat4(1.0f);
	//view = glm::mat4(1.0f);
	//projection = glm::mat4(1.0f);


	GLint camPos = glGetUniformLocation(shaderProgram, "cameraPos");
	// Pass them to the shaders
	glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(model));
	glUniformMatrix4fv(viewLoc, 1, GL_FALSE, glm::value_ptr(view));
	glUniformMatrix4fv(projLoc, 1, GL_FALSE, glm::value_ptr(projection));
	glUniformMatrix4fv(camPos, 1, GL_FALSE, glm::value_ptr(cameraPos));


	GLuint64 startTime, stopTime;
	GLuint queryID[2];

	// generate two queries
	glGenQueries(2, queryID);

	// issue the first query
	// Records the time only after all previous 
	// commands have been completed
	glQueryCounter(queryID[0], GL_TIMESTAMP);

	//glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
	int nvert = 0;
	for (int j = 1, cont = 1; j < N_OBJECTS; ++j, cont += nvert)
	{
		nvert = vertices[j];
		//printf("id: %d  -- cont: %d -- nvert: %d\n", j, cont, nvert);
		glDrawArrays(GL_TRIANGLE_STRIP, cont, nvert);
		//testError("GOGO 000");
	}
	//printf("STUFF\n");
	// fflush(stdout);

	glfwSwapBuffers(window);
	glQueryCounter(queryID[1], GL_TIMESTAMP);

	// wait until the results are available
	GLint stopTimerAvailable = 0;
	while (!stopTimerAvailable) {
		glGetQueryObjectiv(queryID[1],
			GL_QUERY_RESULT_AVAILABLE, &stopTimerAvailable);
	}

	// get query results
	glGetQueryObjectui64v(queryID[0], GL_QUERY_RESULT, &startTime);
	glGetQueryObjectui64v(queryID[1], GL_QUERY_RESULT, &stopTime);

	calcFPS(window);
	//showFPS();
	//usleep(10);
	glBufferData(GL_ARRAY_BUFFER, sizeof(float) * 3 * N_POINTS, points, GL_STATIC_DRAW);
	float gpuTime = (stopTime - startTime) / 1000000.0;
	printf("Time spent on the GPU: %f ms\n", gpuTime);
	//showFPS();
	//usleep(10);
	//        glBufferData(GL_ARRAY_BUFFER, sizeof(float)*3*N_POINTS, points, GL_STATIC_DRAW);
	return gpuTime;
}

extern "C" __declspec(dllexport) int startth() {
	go();

	while (!glfwWindowShouldClose(window))
	{

		glfwPollEvents();
		do_movement(window);

		if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS)
			glfwSetWindowShouldClose(window, GL_TRUE);

		glUseProgram(shaderProgram);

		// Clear the colorbuffer
		glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		view = glm::lookAt(cameraPos, posLookAt, cameraUp);
		//view = glm::lookAt(glm::vec3(camX, camY, 0.0f), glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f));//        model = glm::mat4(1.0f);
		//view = glm::mat4(1.0f);
		//projection = glm::mat4(1.0f);


		GLint camPos = glGetUniformLocation(shaderProgram, "cameraPos");
		// Pass them to the shaders
		glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(model));
		glUniformMatrix4fv(viewLoc, 1, GL_FALSE, glm::value_ptr(view));
		glUniformMatrix4fv(projLoc, 1, GL_FALSE, glm::value_ptr(projection));
		glUniformMatrix4fv(camPos, 1, GL_FALSE, glm::value_ptr(cameraPos));

		
		GLuint64 startTime, stopTime;
		GLuint queryID[2];

		// generate two queries
		glGenQueries(2, queryID);

		// issue the first query
		// Records the time only after all previous 
		// commands have been completed
		glQueryCounter(queryID[0], GL_TIMESTAMP);

		//glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
		int nvert = 0;
		for (int j = 1, cont = 1; j < N_OBJECTS; ++j, cont += nvert)
		{
			nvert = vertices[j];
			//printf("id: %d  -- cont: %d -- nvert: %d\n", j, cont, nvert);
			glDrawArrays(GL_TRIANGLE_STRIP, cont, nvert);
			//testError("GOGO 000");
		}
		//printf("STUFF\n");
		// fflush(stdout);

		glfwSwapBuffers(window);
		glQueryCounter(queryID[1], GL_TIMESTAMP);

		// wait until the results are available
		GLint stopTimerAvailable = 0;
		while (!stopTimerAvailable) {
			glGetQueryObjectiv(queryID[1],
				GL_QUERY_RESULT_AVAILABLE, &stopTimerAvailable);
		}

		// get query results
		glGetQueryObjectui64v(queryID[0], GL_QUERY_RESULT, &startTime);
		glGetQueryObjectui64v(queryID[1], GL_QUERY_RESULT, &stopTime);

		printf("Time spent on the GPU: %f ms\n", (stopTime - startTime) / 1000000.0);

		calcFPS(window);
		//showFPS();
		//usleep(10);
		glBufferData(GL_ARRAY_BUFFER, sizeof(float) * 3 * N_POINTS, points, GL_STATIC_DRAW);

	}
	printf("Going Out\n");
	N_POINTS = 0;
	N_OBJECTS = 0;
	glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glfwTerminate();

	free(vertices);
	free(points);
	return 0;
}

int CITY_SIZE = 100;
int maina() {
	//printf("A - S - W - D to move R - F or scrol for camera height\n");
	//printf("Insert the_city radius (NUMBER_OF_BUILDINGS = (radius*2)^2):\n");
	//scanf("%d",&CITY_SIZE);
	//go();
	//startth();
	//create_points(50);
	//init2(CITY_SIZE*2*CITY_SIZE*2);
	//box2(0.0f, 0.0f, 0.0f, 0.4, 0.4, 1.0);
	//the_city(CITY_SIZE);
	//printf("NUMBER_OF_BUILDINGS=%d NUMBER_OF_BLOCKS=%d\n", NUMBER_OF_BUILDINGS, N_POINTS);
	init2(CITY_SIZE * 2 * CITY_SIZE * 2);
	//init(10);

	//createPoints(50);
	//box(0.0f, 0.0f, 0.0f, 0.4, 0.4, 1.0, 1.0f, 1.0f, 1.0f, 0.0f, 1.0f, 0.0f, 0.0f);
	the_city(CITY_SIZE);
	printf("NUMBER_OF_BUILDINGS=%d NUMBER_OF_BLOCKS=%d\n", NUMBER_OF_BUILDINGS, N_POINTS);
	go();
	startth();
	//start();
	return 0;
}
