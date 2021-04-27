#ifndef ENTITY_H
#define ENTITY_H

#define PLAYER_HIT_COOLDOWN 120 // in frames
#define PLAYER_BOMB_COOLDOWN 120 // in frames
#define PLAYER_INIT_BOMBS 3
#define PLAYER_INIT_LIVES 6
#define PLAYER_INIT_SHOTS 6
#define OPAQUE            255
#define SEMI_TRANSPARENT  255 / 2

#define ENT_DEFAULT_W 32 // is changed in respective setup_* function
#define ENT_DEFAULT_H 32
#define PLAYER_W      32
#define PLAYER_H      50

/* data structures */
enum direction {NORTH, WEST, EAST, SOUTH};

typedef enum {
  ENT_PLAYER,
  ENT_ENEMY,
  ENT_BULLET,
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
  int reload;
  int hit_cooldown;  // temporary invincibility when > 0
  int bomb_cooldown; // disables bombs when > 0
  bool is_enemy_bullet;
  Uint32 born, last_update;
  SDL_Texture *texture;
  SDL_Texture *idle[IDLE_FRAMES];
  int alpha;

  void (*ai_function)(int); // holds the current ai function
  Uint32 fire_delay_min, fire_delay_max, fire_time;
  int death_anim_counter;
} Entity;

/* function prototypes */
int compare_entities(const void *, const void *);
void print_entity(Entity *);
int collision(Entity *, Entity *);
void spawn_enemies();
void spawn_bullet();
void fire_bomb();
void spawn_item(Entity *, EntityType);
void setup_generic_entity(Entity *, SDL_Rect *);
void setup_player(Entity *);
void setup_enemy(Entity *);
void setup_item(Entity *);
void setup_bullet(Entity *);
Entity* spawn_entity(EntityType, int, int);
void spawn_enemy_bullet(Entity *);
void spawn_enemy_bullet_flower(Entity *);
void delete_all_entities_from_arr(void *arr, void *elem, void* idx);

#endif
