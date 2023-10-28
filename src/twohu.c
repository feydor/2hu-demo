#include "twohu.h"
#include "entity.h"

#define W_FIELD 800
#define H_FIELD 1800
#define W_SPRITE 128.0
#define H_SPRITE 128.0

typedef struct Twohu {
    SDL_Renderer *renderer;
    SDL_Window *window;
    TwohuEntity *player;

    float dt;
    int w_window, h_window;
    int w_field, h_field;
} Twohu;

Twohu *create_twohu(SDL_Renderer *renderer, SDL_Window *window) {
    Twohu *th = malloc(sizeof(*th));
    if (!th) return NULL;

    th->renderer = renderer;
    th->window = window;
    SDL_GetWindowSize(th->window, &th->w_window, &th->h_window);
    th->w_field = W_FIELD;
    th->h_field = H_FIELD;

    th->player = create_twohu_player(
        (FloatRect){.x=0.0, .y=0.0, .w=W_SPRITE, .h=H_SPRITE},
        (SDL_Point){.x=W_SPRITE - 20, .y=H_SPRITE - 20}
    );
    return th;
}

void twohu_event(Twohu *th, SDL_Event *e) {
    if (e->type == SDL_QUIT || e->type == SDL_KEYDOWN && e->key.keysym.sym == SDLK_ESCAPE) {
        printf("Exiting...\n");
        exit(0);
    }

    twohu_entity_event(th->player, e);
}

void twohu_update(Twohu *th, float dt) {
    th->dt = dt;
    twohu_entity_update(th->player, dt);
}

void twohu_render(Twohu *th) {
    SDL_RenderClear(th->renderer);

    // render field
    SDL_SetRenderDrawColor(th->renderer, 0, 0, 0, 255);
    SDL_RenderFillRect(th->renderer, &(SDL_Rect){ .x=0, .y=0, .w=th->w_window, .h=th->h_window });

    // render player
    twohu_entity_render(th->player, th->renderer);
}