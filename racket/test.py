from ctypes import*
import random
# Load DLL into memory.

shading = cdll.LoadLibrary("C:\\Users\\DEMO\\Documents\\Visual Studio 2015\\Projects\\shading\\x64\\Debug\\shading")


def init(size):
	shading.init(size)

def start():
	shading.start()

def sphere(x,y,z,radius,r,g,b):
	shading.sphere(x,y,z,radius,r,g,b)

def box(x,y,z,sizex, sizey, sizez, r, g, b, angle, vx, vy, vz):
	shading.box(x,y,z,sizex, sizey, sizez, r, g, b, angle, vx, vy, vz)

f0 = c_float(0.0)
f1 = c_float(1.0)
f3 = c_float(3.0)
f10 = c_float(10)

def randomSpheres(n,x):
	for i in range(1,n):
		sphere(c_float(random.uniform(-x, x)),c_float(random.uniform(-x, x)),c_float(random.uniform(-x, x)),c_float(random.uniform(2, 15)),f1,f1,f1)		



init(100)

randomSpheres(150,800)
#sphere(f1, f0, f0, f3, f1, f1, f1)

#box(f1, f3, f0, f3, f3, f3, f0, f1, f1, f0, f1, f1, f1)

start()