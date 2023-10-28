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

/** global button tab */
extern Button g_buttons[BUTTON_COUNT];

#define btn_isdown(b) (g_buttons[b].is_down)
#define btn_isheld(b) (g_buttons[b].is_down && !g_buttons[b].changed)
#define btn_pressed(b) (g_buttons[b].is_down && g_buttons[b].changed)
#define btn_released(b) (g_buttons[b].is_down && g_buttons[b].changed)
#define btn_changed(b) (g_buttons[b].changed)
#define btn_no_movement() (!g_buttons[BUTTON_LEFT].is_down && !g_buttons[BUTTON_RIGHT].is_down \
                            && !g_buttons[BUTTON_UP].is_down && !g_buttons[BUTTON_DOWN].is_down)

/**
 * Buffer key presses into the global buttons tab. Returns a reference to the pressed button
 */
Button handle_btn_input(SDL_Keycode key, bool is_down);

#endif
