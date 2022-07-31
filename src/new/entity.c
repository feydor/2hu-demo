#include "entity.h"
#define SPEED_ENTITY 1

typedef struct TwohuEntity {
    FloatRect rect;
    SDL_Point hitbox;
    SDL_Surface *surface;
    float dx, dy;
    float speed;
} TwohuEntity;

TwohuEntity *create_twohu_entity(FloatRect rect, SDL_Point hitbox) {
    TwohuEntity *entity = malloc(sizeof(*entity));
    if (!entity) return NULL;

    entity->rect = rect;
    entity->hitbox = hitbox;
    entity->dx = entity->dy = 0;
    entity->speed = SPEED_ENTITY;
    entity->surface = SDL_CreateRGBSurface(0, rect.w, rect.h, 32, 0xFF000000, 0xFF0000, 0xFF00, 0xFF);
    return entity;
}

void twohu_entity_event(TwohuEntity *entity, SDL_Event *e) {
    if (e->type == SDL_KEYDOWN) {
        SDL_Keycode key = e->key.keysym.sym;
        if (key == SDLK_w) {
            entity->dy = -entity->speed;
        }
        if (key == SDLK_s) {
            entity->dy = entity->speed;
        }
        if (key == SDLK_d) {
            entity->dx = entity->speed;
        }
        if (key == SDLK_a) {
            entity->dx = -entity->speed;
        }
    } else if (e->type == SDL_KEYUP) {
        SDL_Keycode key = e->key.keysym.sym;
        if (key == SDLK_w || key == SDLK_s)
            entity->dy = 0;
        if (key == SDLK_d || key == SDLK_a)
            entity->dx = 0;
    }
}

void twohu_entity_update(TwohuEntity *entity, float dt) {
    entity->rect.x += entity->dx * dt;
    entity->rect.y += entity->dy * dt;
}

void twohu_entity_render(TwohuEntity *ent, SDL_Renderer *renderer) {
    // draw rect aka sprite
    SDL_SetRenderDrawColor(renderer, 0xFF, 0, 0xFF, 255);
    SDL_Rect trunc_rect = { .x=ent->rect.x, .y=ent->rect.y, .w=ent->rect.w, .h=ent->rect.h };
    SDL_RenderDrawRect(renderer, &trunc_rect);

    // draw hitbox on top
    SDL_SetRenderDrawColor(renderer, 0xFF, 0, 0, 255);
    float xoff = (ent->rect.w / 2.0) - (ent->hitbox.x / 2);
    float yoff = (ent->rect.h / 2.0) - (ent->hitbox.y / 2);
    SDL_Rect hb = { .x=ent->rect.x + xoff, .y=ent->rect.y + yoff, .w=ent->hitbox.x, .h=ent->hitbox.y };
    SDL_RenderDrawRect(renderer, &hb);
}