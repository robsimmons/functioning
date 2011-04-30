#include <SDL.h>
#include <SDL_mixer.h>

int ml_mix_default_format() {
  return MIX_DEFAULT_FORMAT;
}

// unused
Mix_Chunk *ml_mix_load_clip(const char *path) {
  Mix_Chunk *ret = Mix_LoadWAV(path);
  return ret;
}


void *ml_mix_loadmus(const char * path) {
  void *ret = Mix_LoadMUS(path);
  return ret;
}
