
all: island

tests: grid_unittest

island: main.hs grid.hs drawing.hs shadertypes.hs
	ghc -package GLUT-2.3.0.0 -o island main.hs

grid_unittest: grid_unittest.hs grid.hs
	ghc grid_unittest.hs

