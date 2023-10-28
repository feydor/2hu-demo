#include "entity.h"
#include "../constants.h"
#include "../input/input.h"
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>

static void change_current_anim(TwohuSpritesheetManager *sm, int anim);
static void render_curr_animation_frame(TwohuSpritesheetManager *sm, FloatRect *dst, SDL_Renderer *r);
static void set_frame(TwohuSpritesheetManager *sm, int nframe);
static void increment_frame(TwohuSpritesheetManager *sm);
static int nframes(TwohuSpritesheetManager *sm);

static TwohuSpritesheetManager create_twohu_spritesheet_manager(int n) {
    IMG_Init(IMG_INIT_PNG);

    SDL_Surface *image = IMG_Load(PLAYER_SPRITESHEET);
    if (!image) {
        exit(fprintf(stderr, "Failed to load the spritesheet: '%s'!\n", PLAYER_SPRITESHEET));
    }

    int sprite_w = image->w / PLAYER_SPRITESHEET_NCOLS;
    int sprite_h = image->h / PLAYER_SPRITESHEET_NROWS;
    TwohuSpritesheetManager sm = {
        .n_anims=n, .sprite_w=sprite_w, .sprite_h=sprite_h,
        .image=image, .curr_anim=0, .curr_frame=0, .clip=(SDL_Rect){0}
    };
    change_current_anim(&sm, 0);
    return sm;
}

static void change_current_anim(TwohuSpritesheetManager *sm, int anim) {
    if (anim >= sm->n_anims) {
        exit(fprintf(stderr, "Out of bounds: curr anim: %d is greater than %d!\n", anim, sm->n_anims));
    }

    sm->curr_anim = anim;
    set_frame(sm, 0);
}

static bool is_current_anim(TwohuSpritesheetManager *sm, int anim) {
    return sm->curr_anim == anim;
}

static void render_curr_animation_frame(TwohuSpritesheetManager *sm, FloatRect *dst, SDL_Renderer *r) {
    if (!sm->texture) {
        sm->texture = SDL_CreateTextureFromSurface(r, sm->image);
        if (!sm->texture) {
            exit(fprintf(stderr, "Failed to create texture!\n"));
        }
    }

    SDL_Rect rect = floatrect_to_sdlrect(dst);
    SDL_RenderCopy(r, sm->texture, &sm->clip, &rect);
}

/** Set the current frame to nframe */
static void set_frame(TwohuSpritesheetManager *sm, int nframe) {
    sm->curr_frame = nframe;
    if (sm->curr_frame >= nframes(sm)) {
        sm->curr_frame = 0; 
    }

    sm->clip = (SDL_Rect){
        .x= sm->sprite_w * sm->curr_frame,
        .y = sm->sprite_h * sm->curr_anim,
        .w = sm->sprite_w,
        .h = sm->sprite_h - 2 // in order to remove extra pixels at the sprite's bottom
    };
}

/** Increment the current frame */
static void increment_frame(TwohuSpritesheetManager *sm) {
    set_frame(sm, sm->curr_frame+1);
}

TwohuEntity create_twohu_entity(FloatRect rect, SDL_Point hitbox, bool player) {
//    TwohuEntity *entity = malloc(sizeof(*entity));
//    if (!entity) return NULL;
    TwohuEntity entity = {0};

    entity.rect = rect;
    entity.hitbox = hitbox;
    entity.dx = 0;
    entity.dy = 0;
    entity.speed = ENTITY_SPEED;
    entity.player = player;
    entity.bullet_manager = twohu_bulletmanager_create();
    entity.alive = player;

    entity.surface = SDL_CreateRGBSurface(0, rect.w, rect.h, 32, 0xFF000000, 0xFF0000, 0xFF00, 0xFF);

    if (player) {
        entity.sheet_manager = create_twohu_spritesheet_manager(PLAYER_N_ANIMS);
    }
    
    return entity;
}

TwohuEntity create_twohu_player(FloatRect rect, SDL_Point hitbox) {
    return create_twohu_entity(rect, hitbox, true);
}

TwohuEntity create_twohu_enemy(FloatRect rect, SDL_Point hitbox) {
    return create_twohu_entity(rect, hitbox, false);
}

inline float twohu_W(TwohuEntity *entity) { return entity->rect.w; }
inline float twohu_H(TwohuEntity *entity) { return entity->rect.h; }
inline SDL_Point twohu_entity_center(TwohuEntity *e) {
    return (SDL_Point){
        .x=e->rect.x + (e->rect.w/2),
        .y=e->rect.y + (e->rect.h/2)
    };
}

/** The number of frames in an animation */
static inline int nframes(TwohuSpritesheetManager *sm) {
    return sm->image->w / sm->sprite_w;
}

SDL_Rect floatrect_to_sdlrect(FloatRect *r) {
    return (SDL_Rect){ .x = r->x, .y = r->y, .w = r->w, .h = r->h };
}

void floatrect_print(FloatRect *r) {
    printf("FloatRect{.x=%f, .y=%f, .w=%f, .h=%f}\n", r->x, r->y, r->w, r->h);
}

void twohu_entity_event(TwohuEntity *self, SDL_Event *event) {
    SDL_Keycode key = event->key.keysym.sym;
    switch (event->type) {
    case SDL_KEYDOWN:
        handle_btn_input(key, true);
        break;
    case SDL_KEYUP:
        handle_btn_input(key, false);
        break;
    }
}

void twohu_entity_update(TwohuEntity *entity, float dt) {
    if (entity->player) {
        // TODO: make this work for enemies too
        twohu_bulletmanager_update(&entity->bullet_manager, dt);
    }

    float x_next = entity->rect.x;
    float y_next = entity->rect.y;

    if (entity->player) {
        // handle input
        if (btn_isdown(BUTTON_UP)) {
            entity->dy = entity->speed;
            y_next -= entity->dy;
            if (btn_pressed(BUTTON_UP) &&
                !is_current_anim(&entity->sheet_manager, PLAYER_ANIM_IDLE)) {
                change_current_anim(&entity->sheet_manager, PLAYER_ANIM_IDLE);
            }
        }
        if (btn_isdown(BUTTON_DOWN)) {
            entity->dy = entity->speed;
            y_next += entity->dy;
            if (btn_pressed(BUTTON_DOWN) &&
                !is_current_anim(&entity->sheet_manager, PLAYER_ANIM_IDLE)) {
                change_current_anim(&entity->sheet_manager, PLAYER_ANIM_IDLE);
            }
        }
        if (btn_isdown(BUTTON_LEFT)) {
            entity->dx = entity->speed;
            x_next -= entity->dy;
            if (btn_pressed(BUTTON_LEFT) &&
                !is_current_anim(&entity->sheet_manager, PLAYER_ANIM_RIGHT)) {
                change_current_anim(&entity->sheet_manager, PLAYER_ANIM_RIGHT);
            }
        }
        if (btn_isdown(BUTTON_RIGHT)) {
            entity->dx = entity->speed;
            x_next += entity->dy;
            if (btn_pressed(BUTTON_RIGHT) &&
                !is_current_anim(&entity->sheet_manager, PLAYER_ANIM_LEFT)) {
                change_current_anim(&entity->sheet_manager, PLAYER_ANIM_LEFT);
            }
        }
        if (btn_isdown(BUTTON_X)) {
            twohu_bullet_spawn(&entity->bullet_manager, twohu_entity_center(entity), 0,
                               -PLAYER_BULLET_SPEED);
        }

        if (btn_no_movement()) {
            if (!is_current_anim(&entity->sheet_manager, PLAYER_ANIM_IDLE)) {
                change_current_anim(&entity->sheet_manager, PLAYER_ANIM_IDLE);
            }
        }
    } else {
        // enemy
        x_next = entity->rect.x + entity->dx * dt;
        y_next = entity->rect.y + entity->dy * dt;
    }


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
    } else {
        // enemy
        if (y_next < 0 || y_next > H_WINDOW - entity->rect.w ||
            x_next < 0 || x_next > W_WINDOW - entity->rect.w) {
            entity->alive = false;
            return;
        }
    }

    entity->rect.x = x_next;
    entity->rect.y = y_next;
}

void twohu_entity_render(TwohuEntity *self, SDL_Renderer *renderer) {
    twohu_bulletmanager_render(&self->bullet_manager, renderer);

    if (self->player) {
        // play animation
        render_curr_animation_frame(&self->sheet_manager, &self->rect, renderer);
        increment_frame(&self->sheet_manager);
    } else {
        // draw rect aka sprite
        SDL_SetRenderDrawColor(renderer, 0xFF, 0, 0xFF, 255);
        SDL_Rect trunc_rect = { .x=self->rect.x, .y=self->rect.y, .w=self->rect.w, .h=self->rect.h };
        SDL_RenderDrawRect(renderer, &trunc_rect);

        // draw hitbox on top
        SDL_SetRenderDrawColor(renderer, 0xFF, 0, 0, 255);
        float xoff = (self->rect.w / 2.0) - (self->hitbox.x / 2);
        float yoff = (self->rect.h / 2.0) - (self->hitbox.y / 2);
        SDL_Rect hb = { .x=self->rect.x + xoff, .y=self->rect.y + yoff, .w=self->hitbox.x, .h=self->hitbox.y };
        SDL_RenderDrawRect(renderer, &hb);
    }
}
