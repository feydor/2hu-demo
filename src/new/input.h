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

#define btn_pressed(b) (b.is_down && b.changed)
#define btn_released(b) (b.is_down && b.changed)
#define btn_is_held(b) (b.is_down && !b.changed)

/**
 * maps an SDL keypress to the global buttons input tracker
 * and returns a reference to the button
 */
Button keypress_to_button(SDL_KeyCode key, bool is_down);

#endif
