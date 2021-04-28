#ifndef GAME_H
#define GAME_H

/* definitions */
#define WINDOW_W 1280			/* window width */
#define WINDOW_H 1000			/* window height */
#define LEVEL_W 800				/* Level width */
#define LEVEL_H 1800			/* Level height */
#define SPRITE_SCALE 1.5	      /* 2x sprite magnification */
#define PLYR_SPD 6              /* units per frame */
#define SCROLLING_SPEED 1		    /* pixels per frame */
#define ENEMY_IDLE_FRAMES 5
#define ENEMY_DEATH_FRAMES 8
#define STARTPX (LEVEL_W/2)           /* starting position */
#define STARTPY (LEVEL_H/2)           /* starting position */
#define MAX_BULLETS 10          /* max # of bullets */
#define GRAVITY 750.0f;

#define POWERUP_DROPRATE 20
#define SCORE_DROPRATE   80 
#define SCORE_DROP_AMOUNT 1000
#define SCORE_PER_KILL 500
#define SCORE_PER_HIT  55

#define STAGE_MUSIC_CHANNEL  1
#define PLAYER_SHOT_CHANNEL  2
#define ENEMY_HIT_CHANNEL    3
#define PLAYER_HIT_CHANNEL   4
#define PLAYER_BOMB_CHANNEL  5
#define PLAYER_DEATH_CHANNEL 6

enum playerstates {PL_NORMAL, PL_FIRE, PL_HURT, PL_DYING, PL_DEAD};

/* Begin input */
typedef struct {
  int is_down;
  int changed;
} Button;

enum {
  BUTTON_LEFT,
  BUTTON_RIGHT,
  BUTTON_UP,
  BUTTON_DOWN,

  BUTTON_Z,
  BUTTON_X,
  BUTTON_ESC,

  BUTTON_COUNT,
};

typedef struct {
  Button buttons[BUTTON_COUNT];  
} Input;

#define pressed(b) (game.input.buttons[b].is_down && game.input.buttons[b].changed)
#define released(b) (!game.input.buttons[b].is_down && game.input.buttons[b].changed)
#define is_down(b) (game.input.buttons[b].is_down)
#define is_held(b) (game.input.buttons[b].is_down && !game.input.buttons[b].changed)
/* End input*/

/* data structures */
typedef struct {
  bool score;
  bool hiscore;
  bool powerup;
  bool lives;
  bool bombs; 
} Flags;

typedef struct {
  SDL_Renderer *renderer;
  SDL_Window   *window;
  SDL_Surface  *surface;
  SDL_Texture  *background;
  SDL_Texture  *foreground;
  SDL_Texture  *UI;
  SDL_Texture  *powerup;
  SDL_Texture  *score;
  SDL_Color    white;
  SDL_Color    yellow;
  TTF_Font     *font; 
  Mix_Music	   *bgm;
  Mix_Chunk    *shotsfx;
  Mix_Chunk	   *enemy_hitsfx;
  Mix_Chunk	   *player_hitsfx;
  Mix_Chunk	   *player_deathsfx;
  Mix_Chunk	   *player_bombsfx;
  SDL_Texture  *enemy_death[ENEMY_DEATH_FRAMES];
  SDL_Texture  *player_bullet_txt;
  SDL_Texture  *enemy_bullet_txt;
  SafeArray    bullets;
  SafeArray    enemies;
  SafeArray    items;
  // SafeArray    transients; /* effects, explosions, misc animations with limited TTL */
  Input        input;
  Flags        flags;
  int          fire;
  int          frame;
  int          scrolling_offset;
  int          enemy_spawn_timer;
  int          player_hiscore;
  int          player_score;
  float        dT;
  enum GameStates {READY, ALIVE, GAMEOVER} state;
} Game;

typedef struct {
  int x;
  int y;
  int w;
  int h;
} Camera;

/* external globals */
extern Game game;
extern SDL_Event event;
extern SDL_Texture *enemy_idle[ENEMY_IDLE_FRAMES];
extern Entity player;
extern Camera camera;

/* function prototypes */
void setup();
void key_press(int down);
void spawn_bullet();
void fire_bomb();
void spawn_enemies();
void update();
void update_pipe(int i);
int collision (Entity *e1, Entity *e2);
void calculate_slope(int x1, int y1, int x2, int y2, float *dx, float *dy);
int move_player(int velx, int vely, int fake_it, int weave);
void draw();
void text(char *fstr, int value, int height);
void print_vectors();
bool set_flag(bool);
void set_all_flags();
void clear_flags();
void cleanup();

#endif
