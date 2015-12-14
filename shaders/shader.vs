// Vertex shader + vec4(sin(time)/1,cos(time)/1,0,1);
const GLchar* vertexShaderSrc = GLSL(
    in vec4 mat_1;
    in vec4 mat_2;
    in vec4 mat_3;
    in vec4 mat_4;

    in float type;
    in float sides;
	in float scale;
	in float ratio;

    uniform mat4 view;
    uniform mat4 projection;
    uniform mat4 model;

    out mat4 vMat;

    out int vType;
    out int vSides;
	out float vScale;
	out float vRatio;

    void main() {
        gl_Position = vec4(0.0, 0.0, 0.0, 1.0); //projection * view * model *
        vType = int(type);
        vSides = int(sides);
		vScale = scale;
        vMat = mat4(mat_1,mat_2,mat_3,mat_4);
		vRatio = ratio;
    }
);