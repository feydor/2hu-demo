#include "twohu.h"
#include "../entity/entity.h"
#include "../entity/entity_factory.h"
#include "../util/util.h"
#include "../constants.h"

#define W_FIELD 800
#define H_FIELD 1800
#define W_SPRITE 128.0
#define H_SPRITE 128.0

typedef struct Twohu {
    SDL_Renderer *renderer;
    SDL_Window *window;
    TwohuEntity *player;
    SDL_Surface *bg_surface;
    SDL_Texture *bg_texture;

    float dt;
    int w_window, h_window;
    int w_field, h_field;
    int scrolling_offset;
} Twohu;

Twohu *create_twohu(SDL_Renderer *renderer, SDL_Window *window) {
    Twohu *th = malloc(sizeof(*th));
    if (!th) return NULL;

    th->renderer = renderer;
    th->window = window;
    SDL_GetWindowSize(th->window, &th->w_window, &th->h_window);
    th->w_field = W_FIELD;
    th->h_field = H_FIELD;

    th->player = twohu_ef_create(PLAYER,
                                 (FloatRect){.x=0.0, .y=0.0, .w=W_SPRITE, .h=H_SPRITE},
                                 (SDL_Point){.x=W_SPRITE - 20, .y=H_SPRITE - 20});
    th->bg_surface = load_surface_or_exit(DEFAULT_BG_PATH);
    th->bg_texture = load_texture_or_exit(renderer, th->bg_surface);
    th->scrolling_offset = 0;
    return th;
}

void twohu_event(Twohu *th, SDL_Event *e) {
    if (e->type == SDL_QUIT) {
        printf("Exiting...\n");
        exit(0);
    }

    twohu_entity_event(th->player, e);
}

void twohu_update(Twohu *th, float dt) {
    th->dt = dt;
    twohu_ef_update_all(th->player, dt);

    /* Update scrolling offset for field */
    ++th->scrolling_offset;
    if (th->scrolling_offset >= th->h_window) {
        th->scrolling_offset = 0;
    }
}

void twohu_render(Twohu *self) {
    SDL_RenderClear(self->renderer);

    // render scrolling field
    SDL_RenderCopy(self->renderer, self->bg_texture,
                   NULL,
                   &(SDL_Rect){.x=0, .y=self->scrolling_offset, .w=self->w_window, .h=self->h_window});
    SDL_RenderCopy(self->renderer, self->bg_texture,
                   NULL,
                   &(SDL_Rect){.x=0, .y=-self->h_window + self->scrolling_offset, .w=self->w_window, .h=self->h_window});

    // render all the entities
    twohu_ef_render_all(self->renderer);
}