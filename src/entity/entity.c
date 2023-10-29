#include "entity.h"
#include "../constants.h"
#include "../input/input.h"
#include "../bullet/bullet.h"
#include <SDL2/SDL.h>

inline float twohu_W(TwohuEntity *entity) { return entity->rect.w; }
inline float twohu_H(TwohuEntity *entity) { return entity->rect.h; }
inline SDL_Point twohu_entity_center(TwohuEntity *e) {
    return (SDL_Point){
        .x=e->rect.x + (e->rect.w/2),
        .y=e->rect.y + (e->rect.h/2)
    };
}

static bool is_colliding(TwohuEntity *self, TwohuEntity *other);

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

void twohu_entity_update(TwohuEntity *self, float dt) {
    if (self->type == PLAYER) {
        // TODO: make this work for enemies too
        twohu_bulletmanager_update(dt);
    }

    float x_next = self->rect.x;
    float y_next = self->rect.y;

    if (self->type == PLAYER) {
        // handle input
        if (btn_isdown(BUTTON_UP)) {
            self->dy = self->speed;
            y_next -= self->dy;
            if (btn_pressed(BUTTON_UP) &&
                !twohu_ssm_is_current_anim(&self->sheet_manager, PLAYER_ANIM_IDLE)) {
                twohu_ssm_change_current_anim(&self->sheet_manager, PLAYER_ANIM_IDLE);
            }
        }
        if (btn_isdown(BUTTON_DOWN)) {
            self->dy = self->speed;
            y_next += self->dy;
            if (btn_pressed(BUTTON_DOWN) &&
                !twohu_ssm_is_current_anim(&self->sheet_manager, PLAYER_ANIM_IDLE)) {
                twohu_ssm_change_current_anim(&self->sheet_manager, PLAYER_ANIM_IDLE);
            }
        }
        if (btn_isdown(BUTTON_LEFT)) {
            self->dx = self->speed;
            x_next -= self->dy;
            if (btn_pressed(BUTTON_LEFT) &&
                !twohu_ssm_is_current_anim(&self->sheet_manager, PLAYER_ANIM_RIGHT)) {
                twohu_ssm_change_current_anim(&self->sheet_manager, PLAYER_ANIM_RIGHT);
            }
        }
        if (btn_isdown(BUTTON_RIGHT)) {
            self->dx = self->speed;
            x_next += self->dy;
            if (btn_pressed(BUTTON_RIGHT) &&
                !twohu_ssm_is_current_anim(&self->sheet_manager, PLAYER_ANIM_LEFT)) {
                twohu_ssm_change_current_anim(&self->sheet_manager, PLAYER_ANIM_LEFT);
            }
        }
        if (btn_isdown(BUTTON_X)) {
            twohu_bullet_spawn(twohu_entity_center(self), 0,-PLAYER_BULLET_SPEED);
        }

        if (btn_no_movement()) {
            if (!twohu_ssm_is_current_anim(&self->sheet_manager, PLAYER_ANIM_IDLE)) {
                twohu_ssm_change_current_anim(&self->sheet_manager, PLAYER_ANIM_IDLE);
            }
        }
    } else {
        // enemy
        x_next = self->rect.x + self->dx * dt;
        y_next = self->rect.y + self->dy * dt;
    }

    // bounds checking
    if (self->type == PLAYER) {
        if (x_next < 0) {
            x_next = 1;
        } else if (x_next > W_WINDOW - twohu_W(self)) {
            x_next = W_WINDOW - twohu_W(self) - 1;
        }

        if (y_next < 0) {
            y_next = 1;
        } else if (y_next > H_WINDOW - twohu_H(self)) {
            y_next = H_WINDOW - twohu_H(self) - 1;
        }
    } else {
        // enemy
        if (y_next < 0 || y_next > H_WINDOW - self->rect.w ||
            x_next < 0 || x_next > W_WINDOW - self->rect.w) {
            self->alive = false;
            return;
        }
    }

    // collision detection with bullet manager
    if (self->type != PLAYER) {
        bool is_colliding = twohu_bulletmanager_is_colliding(self);
        if (is_colliding) {
            // TODO: change animation
//            twohu_ssm_change_current_anim(&self->sheet_manager, 1, callback);
            self->alive = false;
            return;
        }
    }

    self->rect.x = x_next;
    self->rect.y = y_next;
}

void twohu_entity_render(TwohuEntity *self, SDL_Renderer *renderer) {
    if (self->type == PLAYER) {
        // TODO: make this work for enemies too
        twohu_bulletmanager_render(renderer);
    }

    // play animation
    self->sheet_manager.render_and_update(&self->sheet_manager, renderer, &self->rect);
}
