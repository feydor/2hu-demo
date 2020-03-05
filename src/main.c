/* 2hu */
#include <stdlib.h> /* for malloc, realloc, free, rand, srand, AND strtol */
#include <stdio.h> /* for printf */
#include <time.h> /* for time */
#define SDL_DISABLE_IMMINTRIN_H
#include <SDL.h>
#include <SDL_image.h>

#define W 720					/* window width */
#define H 900					/* window height */			
#define SPRITE_H 50				/* sprite height */
#define SPRITE_W 32				/* sprite width */
#define SPRITE_SCALE 2			/* 2x sprite magnification */
#define PLYR_SPD 6              /* units per frame */
#define IDLE_FRAMES 9			/* number of idle frames */
#define STARTPX (W/2)           /* starting position */
#define STARTPY (H/2)           /* starting position */

enum gamestates {READY, ALIVE, GAMEOVER} gamestate = READY; // change to READY
enum dir {NORTH, WEST, EAST, SOUTH};

int idle_time = 30;
float frame = 0;

SDL_Event event;
SDL_Renderer *renderer;
SDL_Surface *surface;
SDL_Texture *background;
SDL_Texture *idle[IDLE_FRAMES];
SDL_Texture *right[8];
SDL_Texture *left[8];
SDL_Surface* sprite_sheet;
SDL_Texture* sprite_texture;
SDL_Rect sprite_src_rect;
SDL_Rect sprite_dst_rect;

void setup();
void new_game();
void key_press(int down);
void update();
void update_pipe(int i);
int move_player(int velx, int vely, int fake_it, int weave);
void draw();
void text(char *fstr, int value, int height);

struct point { int x, y; };

struct player {
        SDL_Rect pos;
        SDL_Rect hitbox;
        struct point vel;
        int dir;
        int ylast; // moved in y direction last?
        int state;
        int delay;
        int frame;
        int alive;
        int hp;
        int stun;
} player;

/* entry point & game loop */
int main(int argc, char* argv[]) {
	setup();
    new_game();
	
	for (;;) {
		while (SDL_PollEvent(&event)) {
			switch (event.type) {
				case SDL_QUIT: exit(0);
				case SDL_KEYDOWN: key_press(1); break;
				case SDL_KEYUP: key_press(0); break;
			}
		}

		update();
		draw();
        SDL_Delay(1000 / 60);
        idle_time++;
	}
	return 0;	
}

/* window and rendering setup*/
void setup()
{
        srand(time(NULL));

        SDL_Init(SDL_INIT_VIDEO);
        SDL_Window *win = SDL_CreateWindow("2hu",
                SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, W, H, SDL_WINDOW_SHOWN);
        renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_PRESENTVSYNC);
        if (!renderer) exit(fprintf(stderr, "Could not create SDL renderer\n"));

        sprite_sheet = IMG_Load("res/reimu.png");
        //SDL_SetColorKey(surface, 1, 0xffff00);
        //pillar = SDL_CreateTextureFromSurface(renderer, surf);
        surface = SDL_LoadBMP("res/bg.bmp");
        background = SDL_CreateTextureFromSurface(renderer, surface);

		for (int i = 0; i < 9; i++) {
			char file[80];
			sprintf(file, "res/marisa-idle-%d.bmp", i+1);
			surface = SDL_LoadBMP(file);
			SDL_SetColorKey(surface, 1, 0xffff00);
			idle[i] = SDL_CreateTextureFromSurface(renderer, surface);
		}
        //TTF_Init();
        //font = TTF_OpenFont("res/terminus.ttf", 42);
}

void new_game() 
{
    memset(&player, 0, sizeof player); // set all fields of player struct to 0
    player.alive = 1;
    player.pos.x = STARTPX;
    player.pos.y = STARTPY;
    player.pos.w = SPRITE_W;
    player.pos.h = SPRITE_H;
    player.dir = NORTH;
    player.hp = 3*4;

    gamestate = ALIVE;
}

/* handle keypresses */
void key_press(int down) 
{
	if(event.key.repeat) return;

	int amt = down ? PLYR_SPD : -PLYR_SPD;

	if (down) {
		if(event.key.keysym.sym == SDLK_UP || event.key.keysym.sym == SDLK_DOWN) {
			player.ylast = 1;
		} else {
			player.ylast = 0;
		}
	}
   
	switch (event.key.keysym.sym) {
		case SDLK_UP:
            player.vel.y -= amt;
            if(down) player.dir = NORTH;
            break;
        case SDLK_DOWN:
            player.vel.y += amt;
            if(down) player.dir = SOUTH;
            break;
        case SDLK_LEFT:
            player.vel.x -= amt;
            if(down) player.dir = WEST;
            break;
        case SDLK_RIGHT:
            player.vel.x += amt;
            if(down) player.dir = EAST;
            break;
		case SDLK_ESCAPE:
            exit(0);
	}
}

/*TODO: add collision detection with walls*/
int move_player(int velx, int vely, int fake_it, int weave) 
{
    SDL_Rect newpos = player.pos;
    player.pos.x += velx; 
    player.pos.y += vely;

    //newpos.x += velx;
    //newpos.y += vely;
}

void update() 
{
	struct player *p = &player; // use a pointer to player

    if (!p->vel.x ^ !p->vel.y) { // moving only one direction
        move_player(p->vel.x, p->vel.y, 0, 1);
    }
    else if ((p->ylast || !p->vel.x) && p->vel.y) { 
        //only move 1 direction, but try the most recently pressed first
        int fake_it = move_player(0, p->vel.y, 0, 0);
        move_player(p->vel.x, 0, fake_it, 0);
    } else {
        int fake_it = move_player(p->vel.x, 0, 0, 0);
        move_player(0, p->vel.y, fake_it, 0);
    }

	if(gamestate == ALIVE) {
		p->frame += 1.0f;
	}
}

void draw() 
{
	/* background */
	SDL_Rect dest = {0, 0, W, H};
    SDL_RenderCopy(renderer, background, NULL, &dest);

	/* objects & players*/
	//draw player
    SDL_RenderCopy(renderer, idle[(int)player.frame % IDLE_FRAMES], NULL,
        &(SDL_Rect){player.pos.x, player.pos.y, SPRITE_SCALE*SPRITE_W, SPRITE_SCALE*SPRITE_H});

	SDL_RenderPresent(renderer);
}
