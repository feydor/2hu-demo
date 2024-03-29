#include "input.h"

Button g_buttons[BUTTON_COUNT] = {0};
bool g_init = false;

Button handle_btn_input(SDL_Keycode key, bool is_down) {
    if (!g_init) {
        printf("initing g_buttons...\n");
        for (int i=0; i<BUTTON_COUNT; ++i) {
            g_buttons[i] = (Button){.is_down = false, .changed=false};
        }
        g_init = true;
    }

    int btn = -1;
    switch (key) {
    case SDLK_w: btn = BUTTON_UP; break;
    case SDLK_s: btn = BUTTON_DOWN; break;
    case SDLK_d: btn = BUTTON_RIGHT; break;
    case SDLK_a: btn = BUTTON_LEFT; break;
    case SDLK_x: btn = BUTTON_X; break;
    case SDLK_z: btn = BUTTON_Z; break;
    case SDLK_q: {
        printf("Quiting...\n");
        exit(0);
    }
    default: break;
    }

    if (btn == -1) return (Button){0};

    /* if down is different this frame */
    g_buttons[btn].changed = is_down != g_buttons[btn].is_down;
    g_buttons[btn].is_down = is_down;
    return g_buttons[btn];
}
