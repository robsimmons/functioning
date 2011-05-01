OUTPUT_EXE=game.exe

# You shouldn't have to edit anything below this line

default: game

bin: bin
	mkdir bin

bin/sdlml.o: bin sdlml/sdlml.c
	gcc -DLINUX -O `sdl-config --cflags` -c sdlml/sdlml.c -o bin/sdlml.o

.PHONY: game 
game: bin/sdlml.o
	mlton -link-opt "-lSDL_image -ltiff -lpng -ljpeg -lz `sdl-config --libs`" -default-ann 'allowFFI true' -output $(OUTPUT_EXE) game.cm bin/sdlml.o

.PHONY: examples/*
examples/*: bin/sdlml.o
	mlton -link-opt "-lSDL_image -ltiff -lpng -ljpeg -lz `sdl-config --libs`" -default-ann 'allowFFI true' -output $(OUTPUT_EXE) $@/sources.cm bin/sdlml.o

.PHONY: clean
clean:
	rm -f core core.* *~ *.exe *.o bin/*
