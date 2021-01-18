#include "SDL.h"
#include "SDL_ttf.h"
#include <stdint.h>

#define DLL_EXPORT __declspec(dllexport)

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

const SDL_AudioFormat AUDIO_FORMAT = AUDIO_S16LSB;

static int g_audio_frequency = 48000;
static u8 g_audio_channels = 2;

// An audio frame is a collection of samples that make up a channel.
const u16 AUDIO_FRAMES = 4096;

static SDL_Window *g_window;
static SDL_Renderer *g_renderer;
static SDL_AudioDeviceID g_audio_device_id;
static SDL_AudioSpec g_obtained_audio_spec;

// Starts SDL. Loads TTF. Creates a window/renderer. 
// Returns a non-zero error code on failure.
DLL_EXPORT int Start(int window_width, int window_height, int audio_frequency, u8 audio_channels) {
#define CHECK(expr) do { int error = expr; if (error) return error; } while(0);
  CHECK(SDL_Init(SDL_INIT_EVERYTHING));
  CHECK(TTF_Init());
  CHECK(SDL_CreateWindowAndRenderer(window_width, window_height, 0, &g_window, &g_renderer));
  SDL_StartTextInput();

  SDL_AudioSpec desired;
  SDL_zero(desired);
  desired.freq = g_audio_frequency = audio_frequency;
  desired.format = AUDIO_FORMAT;
  desired.channels = g_audio_channels = audio_channels;
  desired.samples = AUDIO_FRAMES;
  desired.callback = NULL;
  g_audio_device_id = SDL_OpenAudioDevice(NULL, 0, &desired, &g_obtained_audio_spec, 0);
  if (!g_audio_device_id)
    return -1;
#undef CHECK
  return 0;
}

DLL_EXPORT void Quit() {
  SDL_DestroyRenderer(g_renderer);
  SDL_DestroyWindow(g_window);
  SDL_CloseAudioDevice(g_audio_device_id);
  TTF_Quit();
  SDL_Quit();
}

// Loads a BMP from path.
// Returns NULL on failure
DLL_EXPORT SDL_Texture *LoadBMP(const char* path) {
  SDL_Surface *surface = SDL_LoadBMP(path);
  if (!surface) return NULL;
  SDL_Texture *texture = SDL_CreateTextureFromSurface(g_renderer, surface);
  SDL_FreeSurface(surface);
  return texture;
}

// Loads a BMP from path, using pure black as a transparent pixel.
// Returns NULL on failure
DLL_EXPORT SDL_Texture *LoadBMPWithColorKey(const char* path, u8 r, u8 g, u8 b) {
  SDL_Surface *surface = SDL_LoadBMP(path);
  if (!surface) return NULL;
  // Enable color key.
  SDL_SetColorKey(surface, SDL_TRUE, SDL_MapRGB(surface->format, r, g, b));
  SDL_Texture *texture = SDL_CreateTextureFromSurface(g_renderer, surface);
  SDL_FreeSurface(surface);
  return texture;
}

DLL_EXPORT void FreeTexture(SDL_Texture* texture) {
  SDL_DestroyTexture(texture);
}

DLL_EXPORT int TextureWidth(SDL_Texture *texture) {
  int width;
  SDL_QueryTexture(texture, 0, 0, &width, 0);
  return width;
}
DLL_EXPORT int TextureHeight(SDL_Texture *texture) {
  int height;
  SDL_QueryTexture(texture, 0, 0, 0, &height);
  return height;
}

// Returns NULL on failure
DLL_EXPORT TTF_Font *OpenFont(const char *path, int point_size) {
  return TTF_OpenFont(path, point_size);
}
DLL_EXPORT void CloseFont(TTF_Font* font) {
  TTF_CloseFont(font);
}

// Creates white text on a transparent background.
DLL_EXPORT SDL_Texture* CreateTextTexture(TTF_Font *font, const char* text) {
  SDL_Color color = {.r = 255, .g = 255, .b = 255};
  SDL_Surface *surface = TTF_RenderUTF8_Solid(font, text, color);
  if (!surface) return NULL;
  SDL_Texture *texture = SDL_CreateTextureFromSurface(g_renderer, surface);
  SDL_FreeSurface(surface);
  return texture;
}

DLL_EXPORT void Clear() {
  SDL_RenderClear(g_renderer);
}
DLL_EXPORT void SetDrawColor(u8 r, u8 g, u8 b, u8 a) {
  SDL_SetRenderDrawColor(g_renderer, r, g, b, a);
}
DLL_EXPORT void DrawRect(s32 x, s32 y, s32 w, s32 h) {
  SDL_Rect rect = { .x=x, .y=y, .w=w, .h=h };
  SDL_RenderDrawRect(g_renderer, &rect);
}
DLL_EXPORT void FillRect(s32 x, s32 y, s32 w, s32 h) {
  SDL_Rect rect = { .x=x, .y=y, .w=w, .h=h };
  SDL_RenderFillRect(g_renderer, &rect);
}
DLL_EXPORT void DrawTexture(SDL_Texture* texture, int sx, int sy, int sw, int sh, int dx, int dy, int dw, int dh) {
  SDL_Rect src = { .x=sx, .y=sy, .w=sw, .h=sh };
  SDL_Rect dest = { .x=dx, .y=dy, .w=dw, .h=dh };
  SDL_RenderCopy(g_renderer, texture, &src, &dest);
}
DLL_EXPORT void TextureColorMod(SDL_Texture* texture, u8 r, u8 g, u8 b) {
  SDL_SetTextureColorMod(texture, r, g, b);
}
DLL_EXPORT void Present() {
  SDL_RenderPresent(g_renderer);
}

// Returns the number of bytes currently buffered.
DLL_EXPORT u32 BufferedAudioBytes() {
  return SDL_GetQueuedAudioSize(g_audio_device_id);
}

// Fills the audio buffer with the provided bytes array.
// Returns non-zero on error.
DLL_EXPORT int BufferAudio(u8 *bytes, u32 num_bytes) {
  return SDL_QueueAudio(g_audio_device_id, bytes, num_bytes);
}

// Pauses playback of the audio device.
DLL_EXPORT void PauseAudio() {
  SDL_PauseAudioDevice(g_audio_device_id, 1);
}
// Resumes playback of the audio device.
DLL_EXPORT void PlayAudio() {
  SDL_PauseAudioDevice(g_audio_device_id, 0);
}

DLL_EXPORT void Delay(int milliseconds) {
  SDL_Delay(milliseconds);
}
