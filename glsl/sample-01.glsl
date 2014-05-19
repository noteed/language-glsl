bool success;
bool done = false;
int i;
int i, j = 42;
int j = 1;
uint k = 3u;
float x = 1.0;
float a = 1.5, b;
vec2 texcoord1, texcoord2;
vec3 position;
vec4 myRGBA;
ivec2 textureLookup;
bvec3 less;
mat2 mat2D;
mat3 optMatrix;
mat4 view;
mat3x2 m;
struct light
{
  float intensity;
  vec3 position;
} lightVar;
light lightVar3;
float frequencies[3];
uniform vec4 lightPosition[4];
light lights[];
const int numLights = 2;
light lights[numLights];
int a[5];
const float coef = 2.75;
int f ();
float[5] foo();
void foo (float[5]);
void foo (float a[5]);
const vec3 zAxis = vec3 (0.0, 0.0, 1.0);
in vec4 position;
in vec3 normal;
in vec2 texCoord[4];
in float foo[];
centroid out vec2 TexCoord;
invariant centroid out vec4 Color;
invariant flat centroid out vec4 Color;
noperspective out float temperature;
flat out vec3 myColor;
noperspective centroid out vec2 myTexCoord;
out vec4 FragmentColor;
out uint Luminosity;
uniform Transform
{
  mat4 ModelViewMatrix;
  mat4 ModelViewProjectionMatrix;
  uniform mat3 NormalMatrix;
  float Deformation;
};
in Material
{
  smooth in vec4 Color1;
  smooth vec4 Color2;
  vec2 TexCoord;
};
out Vertex
{
  vec4 Position;
  vec2 Texture;
} Coords;
uniform Transform {
  mat4 ModelViewMatrix;
  mat4 ModelViewProjectionMatrix;
  float Deformation;
} transforms[4];
layout (triangles) in;
layout (origin_upper_left) in vec4 gl_FragCoord;
layout (pixel_center_integer) in vec4 gl_FragCoord;
layout (origin_upper_left, pixel_center_integer) in vec4 gl_FragCoord;
layout (triangle_strip, max_vertices = 60) out;
layout (triangle_strip) out;
layout (max_vertices = 60) out;
layout (shared, column_major) uniform;
layout (std140) uniform Transform {
  mat4 M1;
  layout (column_major) mat4 M2;
  mat3 N1;
};
flat out vec4 gl_FrontColor;
lowp float color;
out mediump vec2 P;
lowp ivec2 foo(lowp mat3);
highp mat4 m;
precision highp float;
precision highp int;
precision mediump int;
invariant gl_Position;
out vec3 Color;
invariant Color;
invariant centroid out vec3 Color;
vec4 color = vec4(0.0, 1.0, 0.0, 1.0);
void main ()
{
}
int i = 1 - 5 * 4 + 3;
int i = 1 - (5 * 4) + 3;
int i = (1 - 5) * 4 + 3;
int i = (1 - 5) * (4 + 3);
bool b = 1 < 2;
void main ()
{
  if (intensity < 0.0) /* a comment */
    return;

  if (a & b)
    return;

  if (a | b)
    return;

  if (a && b)
    return;

  if (a || b)
    return;
}

layout (std140) uniform PatternBlock
{ float pattern[100];
  float arr[];
};
