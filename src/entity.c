#include "../include/common.h"
#include "../include/entity.h"
#include "../include/game.h"

/* comparison functions */
int compare_entities(const void *a, const void *b) {
  // use entity->born parameter for comparison
  Entity *e1 = (Entity *) a;
  Entity *e2 = (Entity *) b;
  return (e1->born > e2->born) - (e1->born < e2->born);
}

void print_entity(Entity *entity) {
    printf("(%i, %i)\n", entity->pos.x, entity->pos.y);
}

int collision(Entity *e1, Entity *e2) {
  if (e1->pos.x + e1->pos.w < e2->pos.x) {
    return 0;
  } else if (e1->pos.x > e2->pos.x + e2->pos.w) {
    return 0;
  } else if (e1->pos.y + e1->pos.h < e2->pos.y) {
    return 0;
  } else if (e1->pos.y > e2->pos.y + e2->pos.h) {
    return 0;
  }
  return 1;
}

void spawn_enemies() {
  if (--game.enemy_spawn_timer <= 0) {
    Entity *e = malloc(sizeof(Entity));
    memset(e, 0, sizeof(Entity));
    e->hp = 1;

    int upper = LEVEL_W - 2 * ENEMY_W;
    int lower = 2 * ENEMY_W;
    e->pos.x = rand_from_range(upper, lower);
    e->pos.y = 0;
    e->pos.w = ENEMY_W;
    e->pos.h = ENEMY_H;
    e->last_update = SDL_GetTicks();
    e->born = SDL_GetTicks();
    e->fire_delay_max = 3 * 60;
    e->fire_delay_min = 1 * 60;
    e->fire_time = rand_from_range(e->fire_delay_max, e->fire_delay_min);
    //SDL_QueryTexture(e->enemy_idle[0], NULL, NULL, &e->pos.w, &e->pos.h);

    e->dy = rand_from_range(3, 2);
    e->dx = 0;

    sarray_pushback(&game.enemies, e);
    //e->reload = (rand() % 60 * 2); // fps = 60
    game.enemy_spawn_timer = rand_from_range(60, 40);
  }
}

void spawn_bullet() {
  Mix_PlayChannel( PLAYER_SHOT_CHANNEL, game.shotsfx, 0 ); // play sfx
  int y_variation = 0;
  for (int i = 0; i < 1; i++) {
    Entity *b = malloc(sizeof(Entity));
    memset(b, 0, sizeof(Entity));
    b->pos.x = player.pos.x + (player.pos.w/2) - (BULLET_W/2);
    b->pos.y = player.pos.y + (player.pos.h/2) - (BULLET_H/2) + y_variation;
    b->pos.w = BULLET_W;
    b->pos.h = BULLET_H;
    b->dx = 0;
    b->dy = -1*BULLET_SPD;
    b->hp = 1;
    b->is_enemy_bullet = false;
    b->last_update = SDL_GetTicks();
    b->born = SDL_GetTicks();
    b->texture = bullet_texture;

    sarray_pushback(&game.bullets, b);

    y_variation += 20;
  }
  player.reload = 16; // reload rate
}

void gravitate_items_towards_player(void *arr, void *item, void *idx) {
  UNUSED(arr);
  UNUSED(idx);
  Entity *itm = (Entity *) item;

  calculate_slope(player.pos.x + (player.pos.w / 2),
        player.pos.y + (player.pos.h / 2), itm->pos.x, itm->pos.y,
        &itm->dx, &itm->dy);
    itm->dx *= 15;
    itm->dy *= 15;
}

/* eliminate all enemies on screen */
void fire_bomb() {
  Mix_PlayChannel(PLAYER_BOMB_CHANNEL, game.player_bombsfx, 0);

  player.bomb_cooldown = PLAYER_BOMB_COOLDOWN;
  player.bomb_count -= 1;

  // TODO: gravitate all current items towards the player
  void (*callback) (void *, void *, void *);
  callback = gravitate_items_towards_player;
  sarray_foreach(&game.items, callback);

  callback = delete_all_entities_from_arr;
  sarray_foreach(&game.enemies, callback);
  sarray_foreach(&game.bullets, callback);
}

void spawn_item(Entity *enm, ItemType type) {
  Entity *itm = malloc(sizeof(Entity));
  memset(itm, 0, sizeof(Entity));
  itm->pos.x = enm->pos.x + (enm->pos.w / 2);
  itm->pos.y = enm->pos.y + (enm->pos.h / 2);
  itm->hp = 1;
  itm->is_enemy_bullet = false;
  itm->last_update = SDL_GetTicks();
  itm->born = SDL_GetTicks();
  itm->type = type;

  switch (type) {
    case POWERUP:
      itm->texture = game.powerup;
      itm->pos.w = POWERUP_W;
      itm->pos.h = POWERUP_H;
      break;
    case SCORE:
      itm->texture = game.score;
      itm->pos.w = SCORE_W;
      itm->pos.h = SCORE_H;
      break;
    default:
      itm->texture = game.score;
      itm->pos.w = SCORE_W;
      itm->pos.h = SCORE_H;
      break;
  }

  itm->dy = 3;
  itm->dx = 0;
  
  sarray_pushback(&game.items, itm);
}

void spawn_enemy_bullet(Entity *e) {
  int rand_num_bullets = rand_from_range(1, 1);
  int y_variation = 0;
  for (int i = 0; i < rand_num_bullets; i++) {
    Entity *b = malloc(sizeof(Entity));
    memset(b, 0, sizeof(Entity));
    b->pos.x = e->pos.x + (e->pos.w / 2);
    b->pos.y = e->pos.y + (e->pos.h / 2) + y_variation + ENEMY_BULLET_H + 5;
    b->pos.w = ENEMY_BULLET_W;
    b->pos.h = ENEMY_BULLET_H;
    b->hp = 1;
    b->is_enemy_bullet = true;
    b->last_update = SDL_GetTicks();
    b->born = SDL_GetTicks();
    b->texture = enemy_bullet_texture;

    calculate_slope(player.pos.x + (player.pos.w / 2),
        player.pos.y + (player.pos.h / 2), e->pos.x, e->pos.y,
        &b->dx, &b->dy);
    b->dx *= ENEMY_BULLET_SPD;
    b->dy *= ENEMY_BULLET_SPD;

    //e->reload = (rand() % 15 * 4); // fps = 60
    e->fire_time = rand_from_range(e->fire_delay_max, e->fire_delay_min);
    sarray_pushback(&game.bullets, b);
    y_variation += 2*ENEMY_BULLET_H;
  }
}

void spawn_enemy_bullet_flower(Entity *e) {
    int x_gap = ENEMY_BULLET_W + ENEMY_BULLET_W / 2;
    int y_gap = x_gap;
    for (int i = 0; i < 33; i++) {
        Entity *curr_b = malloc(sizeof(Entity));
        memset(curr_b, 0, sizeof(Entity));
        curr_b->pos.x = e->pos.x;
        curr_b->pos.y = e->pos.y;
        switch(i) {
            case 1:
            case 2:
            case 3:
            case 4:
            case 5:
            case 6:
            case 7:
            case 8:
                curr_b->pos.x += (i * x_gap);
                curr_b->pos.y += 0;
                break;
            case 9:
            case 10:
            case 11:
            case 12:
            case 13:
            case 14:
            case 15:
            case 16:
                curr_b->pos.x += (-(i - 8) * x_gap);
                curr_b->pos.y += 0;
                break;
            case 17:
            case 18:
            case 19:
            case 20:
            case 21:
            case 22:
            case 23:
            case 24:
                curr_b->pos.x += 0;
                curr_b->pos.y += ((i - 16) * y_gap);
                break;
            case 25:
            case 26:
            case 27:
            case 28:
            case 29:
            case 30:
            case 31:
            case 32:
                curr_b->pos.x += 0;
                curr_b->pos.y += (-(i - 24) * y_gap);
                break;

        }
        
         curr_b->pos.w = ENEMY_BULLET_W;
         curr_b->pos.h = ENEMY_BULLET_H;
         curr_b->hp = 1;
         curr_b->is_enemy_bullet = true;
         curr_b->last_update = SDL_GetTicks();
         curr_b->born = SDL_GetTicks();
         curr_b->texture = enemy_bullet_texture;
        
        calculate_slope(player.pos.x + (player.pos.w / 2),
        player.pos.y + (player.pos.h / 2), e->pos.x, e->pos.y,
        &curr_b->dx, &curr_b->dy);
        curr_b->dx *= ENEMY_BULLET_SPD;
        curr_b->dy *= ENEMY_BULLET_SPD;
        
        sarray_pushback(&game.bullets, curr_b);
    }
}

void delete_all_entities_from_arr(void *arr, void *elem, void* idx) {
  UNUSED(idx);
  SafeArray *entities = (SafeArray *) arr;
  Entity *entity = (Entity *) elem;
  sarray_delete(entities, entity);
} 
