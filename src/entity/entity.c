#include "entity.h"
#include "../constants.h"
#include "../input/input.h"
#include <SDL2/SDL.h>

TwohuEntity create_twohu_entity(FloatRect rect, SDL_Point hitbox, bool player) {
    TwohuEntity entity = {0};

    entity.rect = rect;
    entity.hitbox = hitbox;
    entity.dx = 0;
    entity.dy = 0;
    entity.speed = DEFAULT_ENTITY_SPEED;
    entity.player = player;
    entity.bullet_manager = twohu_bulletmanager_create();
    entity.alive = player;

    entity.surface = SDL_CreateRGBSurface(0, rect.w, rect.h, 32, 0xFF000000, 0xFF0000, 0xFF00, 0xFF);

    // TODO: Create this for enemies too
    if (player) {
        entity.sheet_manager = twohu_ssm_create(PLAYER_SPRITESHEET, PLAYER_SPRITESHEET_NCOLS, PLAYER_SPRITESHEET_NROWS);
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
                !twohu_ssm_is_current_anim(&entity->sheet_manager, PLAYER_ANIM_IDLE)) {
                twohu_ssm_change_current_anim(&entity->sheet_manager, PLAYER_ANIM_IDLE);
            }
        }
        if (btn_isdown(BUTTON_DOWN)) {
            entity->dy = entity->speed;
            y_next += entity->dy;
            if (btn_pressed(BUTTON_DOWN) &&
                !twohu_ssm_is_current_anim(&entity->sheet_manager, PLAYER_ANIM_IDLE)) {
                twohu_ssm_change_current_anim(&entity->sheet_manager, PLAYER_ANIM_IDLE);
            }
        }
        if (btn_isdown(BUTTON_LEFT)) {
            entity->dx = entity->speed;
            x_next -= entity->dy;
            if (btn_pressed(BUTTON_LEFT) &&
                !twohu_ssm_is_current_anim(&entity->sheet_manager, PLAYER_ANIM_RIGHT)) {
                twohu_ssm_change_current_anim(&entity->sheet_manager, PLAYER_ANIM_RIGHT);
            }
        }
        if (btn_isdown(BUTTON_RIGHT)) {
            entity->dx = entity->speed;
            x_next += entity->dy;
            if (btn_pressed(BUTTON_RIGHT) &&
                !twohu_ssm_is_current_anim(&entity->sheet_manager, PLAYER_ANIM_LEFT)) {
                twohu_ssm_change_current_anim(&entity->sheet_manager, PLAYER_ANIM_LEFT);
            }
        }
        if (btn_isdown(BUTTON_X)) {
            twohu_bullet_spawn(&entity->bullet_manager, twohu_entity_center(entity), 0,
                               -PLAYER_BULLET_SPEED);
        }

        if (btn_no_movement()) {
            if (!twohu_ssm_is_current_anim(&entity->sheet_manager, PLAYER_ANIM_IDLE)) {
                twohu_ssm_change_current_anim(&entity->sheet_manager, PLAYER_ANIM_IDLE);
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
        self->sheet_manager.render_and_update(&self->sheet_manager, renderer, &self->rect);
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
