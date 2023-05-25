#include "input.h"

Button g_buttons[BUTTON_COUNT] = {0};

static Button handle_btn_input(int btn, bool is_down) {
    /* if down is different this frame */
    g_buttons[btn].changed = is_down != g_buttons[btn].is_down;
    g_buttons[btn].is_down = is_down;
    return g_buttons[btn];
}

Button keypress_to_button(SDL_KeyCode key, bool is_down) {
    switch (key) {
    case SDLK_UP: return handle_btn_input(BUTTON_UP, is_down);
    case SDLK_DOWN: return handle_btn_input(BUTTON_DOWN, is_down);
    case SDLK_LEFT: return handle_btn_input(BUTTON_LEFT, is_down);
    case SDLK_RIGHT: return handle_btn_input(BUTTON_RIGHT, is_down);
    case SDLK_z: return handle_btn_input(BUTTON_Z, is_down);
    case SDLK_x: return handle_btn_input(BUTTON_X, is_down);
    case SDLK_ESCAPE: {
        printf("Exiting game from ESC press...\n");
        exit(0);
    }
    default: return (Button){false, false};
    }
}