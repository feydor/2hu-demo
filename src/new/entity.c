#include <SDL2/SDL_image.h>
#include <stdbool.h>
#include "entity.h"
#include "constants.h"
#include <SDL2/SDL.h>
#include<SDL2/SDL_image.h>
#define SPEED_ENTITY 1

typedef struct TwohuSpritesheetManager {
    int curr_anim;
    int curr_frame;
    int n_anims;
    /** The dimensions of a single sprite */
    int sprite_w, sprite_h;
    SDL_Rect clip;
    SDL_Surface *image;
} TwohuSpritesheetManager;

typedef struct TwohuEntity {
    FloatRect rect;
    SDL_Point hitbox;
    SDL_Surface *surface;
    TwohuSpritesheetManager sheet_manager;
    int nsheets;
    float dx, dy;
    float speed;
    bool player;
} TwohuEntity;

static void change_current_anim(TwohuSpritesheetManager *sm, int anim);
static void render_curr_animation_frame(TwohuSpritesheetManager *sm, FloatRect *dst, SDL_Renderer *r);

static SDL_Rect floatrect_to_sdlrect(FloatRect *r) {
    return (SDL_Rect){ .x = r->x, .y = r->y, .w = r->w, .h = r->h };
}

static TwohuSpritesheetManager create_twohu_spritesheet_manager(int n) {
    IMG_Init(IMG_INIT_PNG);

    SDL_Surface *image = IMG_Load(PLAYER_SPRITESHEET);
    if (!image) {
        exit(fprintf(stderr, "Failed to load the spritesheet: '%s'!\n", PLAYER_SPRITESHEET));
    }

    int sprite_w = image->w / PLAYER_SPRITESHEET_NCOLS;
    int sprite_h = image->h / PLAYER_SPRITESHEET_NROWS;
    TwohuSpritesheetManager sm = { .n_anims=n, .sprite_w=sprite_w, .sprite_h=sprite_h,
        .image=image, .curr_anim=0, .curr_frame=0, .clip=(SDL_Rect){0} };
    change_current_anim(&sm, 0);
    return sm;
}

static void change_current_anim(TwohuSpritesheetManager *sm, int anim) {
    if (anim >= sm->n_anims) {
        exit(fprintf(stderr, "Out of bounds: curr anim: %d is greater than %d!\n", anim, sm->n_anims));
    }

    sm->curr_anim = anim;
    sm->curr_frame = 0;
    sm->clip = (SDL_Rect){
        .x= sm->sprite_w * sm->curr_frame,
        .y = sm->sprite_h * sm->curr_anim,
        .w = sm->sprite_w,
        .h = sm->sprite_h
    };
}

static void render_curr_animation_frame(TwohuSpritesheetManager *sm, FloatRect *dst, SDL_Renderer *r) {
    SDL_Texture *txt = SDL_CreateTextureFromSurface(r, sm->image);
    if (!txt) {
        exit(fprintf(stderr, "Failed to create texture!\n"));
    }
    SDL_Rect rect = floatrect_to_sdlrect(dst);
    SDL_RenderCopy(r, txt, &sm->clip, &rect);
    SDL_DestroyTexture(txt);

    sm->curr_frame++;
    if (sm->curr_frame >= sm->image->w / sm->sprite_w) {
        sm->curr_frame = 0; 
    }
    printf("frame: %d\n", sm->curr_frame);

    sm->clip = (SDL_Rect){
        .x= sm->sprite_w *sm->curr_frame,
        .y = sm->sprite_h * sm->curr_anim,
        .w = sm->sprite_w,
        .h = sm->sprite_h
    };
}

TwohuEntity *create_twohu_entity(FloatRect rect, SDL_Point hitbox, bool player) {
    TwohuEntity *entity = malloc(sizeof(*entity));
    if (!entity) return NULL;

    entity->rect = rect;
    entity->hitbox = hitbox;
    entity->dx = entity->dy = 0;
    entity->speed = SPEED_ENTITY;
    entity->player = player;

    entity->surface = SDL_CreateRGBSurface(0, rect.w, rect.h, 32, 0xFF000000, 0xFF0000, 0xFF00, 0xFF);

    if (player) {
        entity->sheet_manager = create_twohu_spritesheet_manager(PLAYER_N_ANIMS);
    }
    
    return entity;
}

TwohuEntity *create_twohu_player(FloatRect rect, SDL_Point hitbox) {
    return create_twohu_entity(rect, hitbox, true);
}

TwohuEntity *create_twohu_enemy(FloatRect rect, SDL_Point hitbox) {
    return create_twohu_entity(rect, hitbox, false);
}

inline float twohu_W(TwohuEntity *entity) { return entity->rect.w; }
inline float twohu_H(TwohuEntity *entity) { return entity->rect.h; }

void twohu_entity_event(TwohuEntity *entity, SDL_Event *e) {
    if (e->type == SDL_KEYDOWN) {
        SDL_Keycode key = e->key.keysym.sym;
        if (key == SDLK_w) {
            entity->dy = -entity->speed;
            change_current_anim(&entity->sheet_manager, PLAYER_ANIM_IDLE);
        }
        if (key == SDLK_s) {
            entity->dy = entity->speed;
            change_current_anim(&entity->sheet_manager, PLAYER_ANIM_IDLE);
        }
        if (key == SDLK_d) {
            entity->dx = entity->speed;
            change_current_anim(&entity->sheet_manager, PLAYER_ANIM_LEFT);
        }
        if (key == SDLK_a) {
            entity->dx = -entity->speed;
            change_current_anim(&entity->sheet_manager, PLAYER_ANIM_RIGHT);
        }
    } else if (e->type == SDL_KEYUP) {
        SDL_Keycode key = e->key.keysym.sym;
        change_current_anim(&entity->sheet_manager, PLAYER_ANIM_IDLE);
        if (key == SDLK_w || key == SDLK_s)
            entity->dy = 0;
        if (key == SDLK_d || key == SDLK_a)
            entity->dx = 0;
    }
}

void twohu_entity_update(TwohuEntity *entity, float dt) {
    float dx = entity->dx * dt;
    float dy = entity->dy * dt;

    float x_next = entity->rect.x + dx;
    float y_next = entity->rect.y + dy;

    // bounds checking
    if (entity->player) {
        if (x_next < 0) {
            x_next = 1;
        } else if (x_next > W_WINDOW - twohu_W(entity)) {
            x_next = W_WINDOW - twohu_W(entity) - 1;
        }

        if (y_next < 0) {
            y_next = 1;
        } else if (y_next > H_WINDOW - twohu_H(entity)) {
            y_next = H_WINDOW - twohu_H(entity) - 1;
        }
    }

    entity->rect.x = x_next;
    entity->rect.y = y_next;
}

void twohu_entity_render(TwohuEntity *e, SDL_Renderer *renderer) {
    if (e->player) {
        // play animation
        render_curr_animation_frame(&e->sheet_manager, &e->rect, renderer);
    } else {
        // draw rect aka sprite
        SDL_SetRenderDrawColor(renderer, 0xFF, 0, 0xFF, 255);
        SDL_Rect trunc_rect = { .x=e->rect.x, .y=e->rect.y, .w=e->rect.w, .h=e->rect.h };
        SDL_RenderDrawRect(renderer, &trunc_rect);

        // draw hitbox on top
        SDL_SetRenderDrawColor(renderer, 0xFF, 0, 0, 255);
        float xoff = (e->rect.w / 2.0) - (e->hitbox.x / 2);
        float yoff = (e->rect.h / 2.0) - (e->hitbox.y / 2);
        SDL_Rect hb = { .x=e->rect.x + xoff, .y=e->rect.y + yoff, .w=e->hitbox.x, .h=e->hitbox.y };
        SDL_RenderDrawRect(renderer, &hb);
    }    
}