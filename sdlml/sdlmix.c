#include <SDL.h>
#include <SDL_mixer.h>

int ml_mix_default_format() {
  return MIX_DEFAULT_FORMAT;
}

int ml_play_sound(Mix_Chunk *clip) {
  Mix_AllocateChannels(100);
  int ret = Mix_PlayChannelTimed(-1, clip, 0, -1);
  return ret;
}

Mix_Chunk *ml_mix_load_clip(const char *path) {
  Mix_Chunk *ret = Mix_LoadWAV(path);
  return ret;
}

void *ml_mix_loadmus(const char * path) {
  void *ret = Mix_LoadMUS(path);
  return ret;
}
