echo $PWD

LIBS="-L./SDL2_x64/lib -L./SDL2_ttf_x64/lib"
INCLUDES="-I./SDL2_x64/include/SDL2 -I./SDL2_ttf_x64/include/SDL2"
COMPILE_FLAGS="-w -Wl,-subsystem,windows"
LINKS="-lmingw32 -lSDL2main -lSDL2 -lSDL2_ttf"

BUILD_PARAMS="sdl_wrapper.c $INCLUDES $LIBS $COMPILE_FLAGS $LINKS"

gcc -shared -o sdl_wrapper.dll $BUILD_PARAMS
