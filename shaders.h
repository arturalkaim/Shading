
// Shader macro
#define GLSL(src) "#version 410 core\n" #src

#include "shaders/shader.vs"

#include "shaders/shader.geom"
#include "shaders/shader.geom1"

#include "shaders/shader.frag"

#include "shaders/shader.tessevl"

#include "shaders/shader.tessctr"


void testError(std::string src) {
	//GLenum err = glGetError();
	//if (err != GL_NO_ERROR)
	//{
	//printf("(%s) Error: %s\n", src.c_str(), gluErrorString(err));
	printf("testError: Nao sei!\n");
	//}
}

// Shader creation helper
GLuint createShader(GLenum type, const GLchar* src) {
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
