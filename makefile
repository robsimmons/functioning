#                              -*- makefile -*-



# You shouldn't have to change anything below this point

default : thefreakingame

VERSION_TARGET=10.2
FRAMEWORKS=OSX_build/Frameworks
LIBS=-L/usr/lib
CPPFLAGS = -arch i386 -I/usr/local/include -I${FRAMEWORKS}/SDL.framework/Versions/Current/Headers -I${FRAMEWORKS}/SDL_net.framework/Versions/Current/Headers -I${FRAMEWORKS}/SDL_mixer.framework/Versions/Current/Headers -I${FRAMEWORKS}/SDL_image.framework/Versions/Current/Headers -D_THREAD_SAFE -DOSX

sdlml.o: sdlml/sdlml.c
	gcc -O $(CPPFLAGS) -c sdlml/sdlml.c -o bin/sdlml.o

sdlmix.o: sdlml/sdlmix.c
	gcc -O $(CPPFLAGS) -c sdlml/sdlmix.c -o sdlmix.o

messagebox_fake.o: messagebox_fake.c
	gcc -O $(CPPFLAGS) -c functioning/messagebox_fake.c

thefreakinggame: sdlml.o sdlmix.o game.cm