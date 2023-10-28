#ifndef INPUT_H
#define INPUT_H
#include <SDL2/SDL.h>
#include <stdbool.h>

typedef struct {
  bool is_down;
  bool changed;
} Button;

enum {
  BUTTON_LEFT,
  BUTTON_RIGHT,
  BUTTON_UP,
  BUTTON_DOWN,
  BUTTON_Z,
  BUTTON_X,
  BUTTON_ESC,
  BUTTON_COUNT,
};

typedef struct {
    Button buttons[BUTTON_COUNT];
} Input;

/** global button tab */
extern Button g_buttons[BUTTON_COUNT];

#define btn_isdown(b) (g_buttons[b].is_down)
#define btn_isheld(b) (g_buttons[b].is_down && !g_buttons[b].changed)
#define btn_pressed(b) (g_buttons[b].is_down && g_buttons[b].changed)
#define btn_released(b) (g_buttons[b].is_down && g_buttons[b].changed)
#define btn_changed(b) (g_buttons[b].changed)

/**
 * maps an SDL keypress to the global buttons input tracker
 * and returns a reference to the button
 */
Button handle_btn_input(SDL_Keycode key, bool is_down);

#endif
