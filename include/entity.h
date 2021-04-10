#ifndef ENTITY_H
#define ENTITY_H

#define PLAYER_HIT_COOLDOWN 120 // in frames
#define PLAYER_BOMB_COOLDOWN 120 // in frames
#define PLAYER_BOMBS 3
#define OPAQUE 255
#define SEMI_TRANSPARENT 255 / 2

/* forward declarations */

/* data structures */
typedef struct {
  SDL_Rect pos;
  float dx;
  float dy;
  int hp;
  int bomb_count;
  int dir;
  int reload;
  int hit_cooldown;  // temporary invincibility when hit
  int bomb_cooldown;
  int is_enemy_bullet;
  Uint32 born;
  Uint32 last_update;
  SDL_Texture *texture;
  SDL_Texture *idle[IDLE_FRAMES];
  int alpha;

  void (*ai_function)(int); // holds the current ai function
  Uint32 fire_delay_min;
  Uint32 fire_delay_max;
  Uint32 fire_time;
  //SDL_Texture *enemy_idle[ENEMY_IDLE_FRAMES];
} Entity;

/* function prototypes */
int compare_entities(const void *, const void *);
void print_entity(Entity *);

#endif
