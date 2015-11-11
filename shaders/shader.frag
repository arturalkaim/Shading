// Fragment shader
const GLchar* fragmentShaderSrc = GLSL(

    in vec4 gColor;
    out vec4 outColor;

    void main() {
        outColor = gColor;
    }
);  
