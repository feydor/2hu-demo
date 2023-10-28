#ifndef INC_2HU_DEMO_SPRITESHEET_MANAGER_H
#define INC_2HU_DEMO_SPRITESHEET_MANAGER_H
#include <SDL2/SDL.h>
#include <stdbool.h>
#include "../util/rect.h"

typedef struct TwohuSpritesheetManager {
    int curr_anim;
    int curr_frame;
    int n_anims;
    /** The dimensions of a single sprite */
    int sprite_w, sprite_h;
    SDL_Rect clip;
    SDL_Surface *image;
    SDL_Texture *texture;

    /** Renders the current frame and safely increments it afterwards */
    void (*render_and_update)(struct TwohuSpritesheetManager*, SDL_Renderer*, FloatRect*);
} TwohuSpritesheetManager;

#define MAX_SPRITESHEETS 10
#define LOG_PREFIX "twohu_ssm"

/**
 * Create a sprite sheet where each animation is a row in the sheet and each animation has ncols frames.
 * Basically each animation has to have the same number of frames.
 * @param path the sheet's image path
 * @param ncols the number of columns in the sheet
 * @param nrows the number of rows in the sheet
 */
TwohuSpritesheetManager twohu_ssm_create(const char *path, uint ncols, uint nrows);

/** Renders the current frame and safely increments it afterwards */
void twohu_ssm_render_and_update(TwohuSpritesheetManager *self, SDL_Renderer *renderer, FloatRect *dest);
bool twohu_ssm_is_current_anim(TwohuSpritesheetManager *self, int anim);
void twohu_ssm_change_current_anim(TwohuSpritesheetManager *self, int anim);

#endif // INC_2HU_DEMO_SPRITESHEET_MANAGER_H
