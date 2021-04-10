#ifndef GAME_H
#define GAME_H

/* definitions */
#define WINDOW_W 1280			/* window width */
#define WINDOW_H 1000			/* window height */
#define LEVEL_W 800				/* Level width */
#define LEVEL_H 1800			/* Level height */
#define SPRITE_H 50				/* sprite height */
#define SPRITE_W 32				/* sprite width */
#define BULLET_H 32             /* bullet height*/
#define BULLET_W 28             /* bullet width */
#define ENEMY_BULLET_H 24       /* bullet height*/
#define ENEMY_BULLET_W 24       /* bullet width */
#define ENEMY_H 32              /* enemy height*/
#define ENEMY_W 32              /* enemy width */
#define SPRITE_SCALE 1.5	      /* 2x sprite magnification */
#define PLYR_SPD 6              /* units per frame */
#define BULLET_SPD 12
#define SCROLLING_SPEED 1		    /* pixels per frame */
#define ENEMY_BULLET_SPD 3
#define ENEMY_IDLE_FRAMES 5
#define STARTPX (LEVEL_W/2)           /* starting position */
#define STARTPY (LEVEL_H/2)           /* starting position */
#define MAX_BULLETS 10          /* max # of bullets */
#define PLAYER_LIVES 7

enum playerstates {PL_NORMAL, PL_FIRE, PL_HURT, PL_DYING, PL_DEAD};
enum dir {NORTH, WEST, EAST, SOUTH};

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
  SDL_Renderer *renderer;
  SDL_Window   *window;
  SDL_Surface  *surface;
  SDL_Texture  *background;
  SDL_Texture  *foreground;
  SDL_Texture  *UI;
  SDL_Color    white;
  SDL_Color    yellow;
  TTF_Font     *font; 
  Mix_Music	   *bgm;
  Mix_Chunk    *shotsfx;
  Mix_Chunk	   *enemy_hitsfx;
  Mix_Chunk	   *player_hitsfx;
  Mix_Chunk	   *player_deathsfx;
  SafeArray    bullets;
  SafeArray    enemies;
  Input        input;
  int          fire;
  int          frame;
  int          scrolling_offset;
  enum GameStates {READY, ALIVE, GAMEOVER} state;
} Game;

typedef struct {
  int x;
  int y;
  int w;
  int h;
} Camera;

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
void cleanup();

#endif
