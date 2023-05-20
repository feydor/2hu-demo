#include <SDL2/SDL.h>
#include "twohu.h"
#include "constants.h"
#define MAX(X, Y) (((X) > (Y)) ? (X) : (Y))
#define MIN(X, Y) (((X) < (Y)) ? (X) : (Y)) 

int main(void) {
    SDL_Window *window = SDL_CreateWindow("2hu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                                          W_WINDOW, H_WINDOW, SDL_WINDOW_SHOWN);
    SDL_Renderer *renderer =
        SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_ACCELERATED);
    SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
    if (!renderer) {
        exit(fprintf(stderr, "Could not create SDL Renderer\n"));
    }

    Twohu *twohu = create_twohu(renderer, window);
    SDL_Event e;
    float dt = 1000.0f / 60.0f;
    for (;;) {
        uint32_t t0 = SDL_GetTicks();

        while (SDL_PollEvent(&e)) {
            twohu_event(twohu, &e);
        }

        twohu_update(twohu, dt);
        twohu_render(twohu);
        SDL_RenderPresent(renderer);

        uint32_t t1 = SDL_GetTicks();
        SDL_Delay(MAX(10, dt - (t1 - t0)));
    }
    return 0;
}