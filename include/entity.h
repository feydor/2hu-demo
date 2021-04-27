#ifndef ENTITY_H
#define ENTITY_H

#define PLAYER_HIT_COOLDOWN 120 // in frames
#define PLAYER_BOMB_COOLDOWN 120 // in frames
#define PLAYER_INIT_BOMBS 3
#define PLAYER_INIT_LIVES 6
#define PLAYER_INIT_SHOTS 16
#define OPAQUE            255
#define SEMI_TRANSPARENT  255 / 2

#define ENT_DEFAULT_W 32 // is changed in respective setup_* function
#define ENT_DEFAULT_H 32
#define PLAYER_W      32
#define PLAYER_H      50
#define ENEMY_W       32
#define ENEMY_H       32
#define ITEM_W        16
#define ITEM_H        16
#define PLAYER_BULLET_W 28
#define PLAYER_BULLET_H 32
#define PLAYER_BULLET_SPD 12
#define ENEMY_BULLET_W 24
#define ENEMY_BULLET_H 24
#define ENEMY_BULLET_SPD 3

/* data structures */
enum direction {NORTH, WEST, EAST, SOUTH};

typedef enum {
  ENT_PLAYER,
  ENT_ENEMY,
  ENT_PLAYER_BULLET,
  ENT_ENEMY_BULLET,
  ENT_POWERUP,
  ENT_SCORE
} EntityType;

typedef struct {
  SDL_Rect pos;
  float dx, dy;
  EntityType type;
  int hp;
  int bomb_count, shot_count;
  enum direction dir;
  int hit_cooldown;  // temporary invincibility when > 0
  int bomb_cooldown; // disables bombs when > 0
  Uint32 born, last_update;
  SDL_Texture *texture;
  SDL_Texture *idle[IDLE_FRAMES];
  int alpha;
  void (*play_spawnsfx)(EntityType);
  void (*motion_eq)(float, int, int, int *, int *);
  void (*ai_function)(int); // holds the current ai function
  Uint32 fire_time;
  int death_anim_counter;
} Entity;

/* function prototypes */
int compare_entities(const void *, const void *);
void print_entity(Entity *);
int collision(Entity *, Entity *);
void center_of_entity(Entity *, int, int, int *, int *);
void spawn_enemies_rand(int, int);
void spawn_bullet(EntityType, Entity *, Entity *);
void fire_bomb();
void spawn_item(Entity *, EntityType);
void setup_player(Entity *);
void setup_enemy(Entity *);
void setup_item(Entity *, EntityType);
Entity* spawn_entity(EntityType, int, int);
void spawn_enemy_bullet_flower(Entity *);
void delete_all_entities_from_arr(void *arr, void *elem, void* idx);

#endif
