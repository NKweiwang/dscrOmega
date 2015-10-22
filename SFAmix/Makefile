CC=g++
CFLAGS=-O2 -march=x86-64 -ffast-math -fopenmp
#CFLAGS=-O2 -march=x86-64 -ffast-math -DHAVE_INLINE -DGSL_RANGE_CHECK_OFF -g -pg -fopenmp
LINKER=-L /usr/local/lib/
INCLUDE=-I /usr/local/include/ -I /usr/local/include/eigen3
#LIBS=-lstdc++ -lgfortran -lgsl -lgslcblas -lpthread -lm -static
LIBS=-lgsl -lgslcblas -lm

OBJECTS=SFAmix.o

main: $(OBJECTS)
	g++ $(CFLAGS) $(OBJECTS) -o SFAmix $(LINKER) $(LIBS)
SFAmix.o: SFAmix.cpp
	$(CC) $(CFLAGS) $(INCLUDE) -c SFAmix.cpp
clean:
	rm *.o
