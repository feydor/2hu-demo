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

void print_entity(Entity *ent) {
    printf("(%i, %i)\n", ent->pos.x, ent->pos.y);
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

// returns values in x, y
void center_of_entity(Entity *ent, int offset_x, int offset_y, int *x, int *y) {
  *x = ent->pos.x + ent->pos.w/2 - offset_x;
  *y = ent->pos.y + ent->pos.w/2 - offset_y;
}

void setup_player(Entity *plr) {
  plr->pos.w = PLAYER_W; 
  plr->pos.h = PLAYER_H; 
  plr->dir = NORTH;
  plr->hp = PLAYER_INIT_LIVES;
  plr->bomb_count = PLAYER_INIT_BOMBS;
  plr->shot_count = PLAYER_INIT_SHOTS;
  plr->type = ENT_PLAYER;
}

void setup_enemy(Entity *enm) {
  enm->pos.w = ENEMY_W;
  enm->pos.h = ENEMY_H;
  enm->hp = 1;
  enm->fire_time = rand_from_range(3 * 60, 1 * 60);
  enm->dy = rand_from_range(3, 2);
  enm->dx = 0;
  enm->type = ENT_ENEMY;
}

void setup_item(Entity *itm, EntityType type) {
  itm->pos.w = ITEM_W;
  itm->pos.h = ITEM_H;
  itm->hp = 1;
  itm->type = type;
  itm->dy = 3;
  itm->dx = 0;
  itm->texture = type == ENT_POWERUP
                 ? game.powerup
                 : type == ENT_SCORE
                 ? game.score
                 : game.score;
}

void play_spawnsfx(EntityType type) {
  switch (type) {
    case ENT_PLAYER_BULLET:
      Mix_PlayChannel(PLAYER_SHOT_CHANNEL, game.shotsfx, 0);
      break;
    default:
      // do nothing
      break;
  }
}

// generic spawn for any entity
// returns a pointer to the new entity
Entity* spawn_entity(EntityType type, int x, int y) {
  Entity* entity = malloc( sizeof(Entity) );
  memset( entity, 0, sizeof(Entity) );

  // set common attributes
  entity->pos = (SDL_Rect){x, y, ENT_DEFAULT_W, ENT_DEFAULT_H};
  entity->dx = entity->dy = 0;
  entity->last_update = entity->born = SDL_GetTicks();
  entity->alpha = OPAQUE;
  entity->hit_cooldown = entity->bomb_cooldown = 0;
  entity->hp = 1;
  entity->type = type;
  entity->play_spawnsfx = play_spawnsfx;

  switch (type) {
    case ENT_PLAYER:
      setup_player(entity);
      break;
    case ENT_ENEMY:
      setup_enemy(entity);
      break;
    case ENT_POWERUP:
      setup_item(entity, ENT_POWERUP);
      break;
    case ENT_SCORE:
      setup_item(entity, ENT_SCORE);
      break;
    case ENT_PLAYER_BULLET:
      entity->pos.w = PLAYER_BULLET_W;
      entity->pos.h = PLAYER_BULLET_H;
      entity->texture = game.player_bullet_txt;
      entity->dy = -1*PLAYER_BULLET_SPD;
      break;
    case ENT_ENEMY_BULLET:
      entity->pos.w = ENEMY_BULLET_W;
      entity->pos.h = ENEMY_BULLET_H;
      entity->texture = game.enemy_bullet_txt;
      break;
    default:
      printf("type for entity is missing.");
      break;
  }

  return entity;
}

// continously spawn random enemies
// from max_t frames to min_t frames
void spawn_enemies_rand(int max_t, int min_t) {
  if (--game.enemy_spawn_timer <= 0) {
    int upper_x = LEVEL_W - 2 * ENEMY_W;
    int lower_x = 2 * ENEMY_W;
    Entity *enm;
    enm = spawn_entity( ENT_ENEMY, rand_from_range(upper_x, lower_x), 0 );

    sarray_pushback(&game.enemies, enm);
    game.enemy_spawn_timer = rand_from_range(max_t, min_t);
  }
}

// Generic bullet spawning for all entities
void spawn_bullet(EntityType blt_type, Entity *src, Entity *target) {
  int x = 0, y = 0;
  if (blt_type == ENT_PLAYER_BULLET) {
    x = src->pos.x + (src->pos.w/2) - (PLAYER_BULLET_W/2);
    y = src->pos.y + (src->pos.h/2) - (PLAYER_BULLET_H/2);
  } else {
    x = src->pos.x + (src->pos.w/2) - (ENEMY_BULLET_W/2);
    y = src->pos.y + (src->pos.h/2) - (ENEMY_BULLET_H/2);
  }
  Entity *blt1;
  blt1 = spawn_entity(blt_type, x, y);

  // play sound, if applicable
  blt1->play_spawnsfx(blt_type);

  // 4 or 5 shot states, filtered into by entity.shot_count
  // 0 < shot_count / sp < 10 === SHOT_LVL_1
  // .....
  // 190 < shot_count / sp < 290 === SHOT_LVL_X
  Entity *blt2 = NULL;
  Entity *blt3 = NULL;
  Entity *blt4 = NULL;
  if (src->shot_count <= 5) {
     // single shot, do nothing 
  }

  if (src->shot_count > 5 && src->shot_count <= 10) {
    blt1->pos.x = x - 50;
    blt2 = spawn_entity(blt_type, x + 50, y); 
  } 

  if (src->shot_count > 10 && src->shot_count <= 15) {
    blt1->pos.y = y + 50;
    // blt2->pos.x = x + 50;
    blt2 = spawn_entity(blt_type, x + 50, y); 
    blt3 = spawn_entity(blt_type, x - 50, y); 
  } 

  if (src->shot_count > 15) {
    blt1->pos.y = y + 50;
    // blt2->pos.x = x + 50;
    // blt3->pos.x = x - 50;
    blt2 = spawn_entity(blt_type, x + 50, y); 
    blt3 = spawn_entity(blt_type, x - 50, y); 
    blt4 = spawn_entity(blt_type, x, y - 50); 
  }
  // depending on SHOT_LVL, create bullets and giver trajectories(dy and dx)
  if (target) {
    calculate_slope(target->pos.x + (target->pos.w / 2),
        target->pos.y + (target->pos.h / 2), src->pos.x, src->pos.y,
        &blt1->dx, &blt1->dy);
    blt1->dx *= ENEMY_BULLET_SPD;
    blt1->dy *= ENEMY_BULLET_SPD;
  }

  // add bullets to game.bullets
  sarray_pushback(&game.bullets, blt1);

  if (blt2) sarray_pushback(&game.bullets, blt2);
  if (blt3) sarray_pushback(&game.bullets, blt3);
  if (blt4) sarray_pushback(&game.bullets, blt4);
  // set reload to DEFAULT_RELOAD
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

  // gravitate all current items towards the player
  void (*callback) (void *, void *, void *);
  callback = gravitate_items_towards_player;
  sarray_foreach(&game.items, callback);

  callback = delete_all_entities_from_arr;
  sarray_foreach(&game.enemies, callback);
  sarray_foreach(&game.bullets, callback);
}

void spawn_item(Entity *enm, EntityType type) {
  int x = enm->pos.x + (enm->pos.w / 2);
  int y = enm->pos.y + (enm->pos.h / 2);
  Entity *itm = spawn_entity(type, x, y);

  sarray_pushback(&game.items, itm);
}

void delete_all_entities_from_arr(void *arr, void *elem, void* idx) {
  UNUSED(idx);
  SafeArray *entities = (SafeArray *) arr;
  Entity *entity = (Entity *) elem;
  sarray_delete(entities, entity);
} 

