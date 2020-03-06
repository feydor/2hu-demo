/* 2hu */
#include <stdlib.h> /* for malloc, realloc, free, rand, srand, AND strtol */
#include <stdio.h> /* for printf */
#include <time.h> /* for time */
#define SDL_DISABLE_IMMINTRIN_H
#include <SDL.h>
#include <SDL_image.h>

#include "vector.c"

#define W 720					/* window width */
#define H 900					/* window height */			
#define SPRITE_H 50				/* sprite height */
#define SPRITE_W 32				/* sprite width */
#define BULLET_H 15             /* bullet height*/
#define BULLET_W 15             /* bullet width */
#define SPRITE_SCALE 2			/* 2x sprite magnification */
#define PLYR_SPD 6              /* units per frame */
#define IDLE_FRAMES 9			/* number of idle frames */
#define STARTPX (W/2)           /* starting position */
#define STARTPY (H/2)           /* starting position */
#define MAX_BULLETS 10          /* max # of bullets */

enum gamestates {READY, ALIVE, GAMEOVER} gamestate = READY;
enum playerstates {PL_NORMAL, PL_FIRE, PL_HURT, PL_DYING, PL_DEAD};
enum dir {NORTH, WEST, EAST, SOUTH};

int idle_time = 30;

SDL_Event event;

SDL_Texture *bullet_texture;
SDL_Texture *right[9];
SDL_Texture *left[9];

void setup();
void key_press(int down);
void spawn_bullet();
void update();
void update_pipe(int i);
int move_player(int velx, int vely, int fake_it, int weave);
void draw();
void text(char *fstr, int value, int height);

struct point { int x, y; };

typedef struct {
    SDL_Renderer *renderer;
    SDL_Window *window;
    SDL_Surface *surface;
    SDL_Texture *background;
    int fire;
    int frame;
} Game;

typedef struct {
    SDL_Rect pos;
    int dx;
    int dy;
    int hp;
    int dir;
    int ylast;
    int reload;
    SDL_Texture *texture;
    SDL_Texture *idle[IDLE_FRAMES];
} Entity;

Game game;
Entity bullet;
Entity player;
vector bullets;

/*struct player {
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
} player;*/

int num_active_bullets = 0;

/* entry point & game loop */
int main(int argc, char* argv[]) {
	setup();
	
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

        memset(&game, 0, sizeof(Game));
	    memset(&player, 0, sizeof(Entity));
        vector_init(&bullets);
        player.pos.x = STARTPX;
        player.pos.y = STARTPY;
        player.pos.w = SPRITE_W;
        player.pos.h = SPRITE_H;
        player.dir = NORTH;
        player.hp = 1;

        gamestate = ALIVE;

        SDL_Init(SDL_INIT_VIDEO);
        game.window = SDL_CreateWindow("2hu",
                SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, W, H, SDL_WINDOW_SHOWN);
        game.renderer = SDL_CreateRenderer(game.window, -1, SDL_RENDERER_PRESENTVSYNC);
        if (!game.renderer) exit(fprintf(stderr, "Could not create SDL renderer\n"));

        //sprite_sheet = IMG_Load("res/reimu.png");
        //SDL_SetColorKey(surface, 1, 0xffff00);
        //pillar = SDL_CreateTextureFromSurface(renderer, surf);
        game.surface = SDL_LoadBMP("res/bg.bmp");
        game.background = SDL_CreateTextureFromSurface(game.renderer, game.surface);
        game.surface = SDL_LoadBMP("res/shot-1.bmp");
        bullet_texture = SDL_CreateTextureFromSurface(game.renderer, game.surface);

        /* loads player sprites*/
		for (int i = 0; i < 9; i++) {
			char file[80];
			sprintf(file, "res/reimu-idle-%d.bmp", i+1);
			game.surface = SDL_LoadBMP(file);
			SDL_SetColorKey(game.surface, 1, 0xffff00);
			player.idle[i] = SDL_CreateTextureFromSurface(game.renderer, game.surface);
		}
        //TTF_Init();
        //font = TTF_OpenFont("res/terminus.ttf", 42);
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
            player.dy -= amt;
            if(down) player.dir = NORTH;
            break;
        case SDLK_DOWN:
            player.dy += amt;
            if(down) player.dir = SOUTH;
            break;
        case SDLK_LEFT:
            player.dx -= amt;
            if(down) player.dir = WEST;
            break;
        case SDLK_RIGHT:
            player.dx += amt;
            if(down) player.dir = EAST;
            break;
        case SDLK_z: 
            if (down && player.reload == 0) {
                printf("spawn bullet\n");
                spawn_bullet();
            }
            break;
		case SDLK_ESCAPE:
            exit(0);
	}
}

void spawn_bullet() 
{
    Entity *b;
    b = malloc(sizeof(Entity));
    memset(b, 0, sizeof(Entity));
    b->pos.x = player.pos.x + SPRITE_W/2;
    b->pos.y = player.pos.y;
    b->dx = 0;
    b->dy = -4*PLYR_SPD;
    b->hp = 1;
    b->texture = bullet_texture;
    SDL_QueryTexture(b->texture, NULL, NULL, &b->pos.w, &b->pos.h);

    vector_pushback(&bullets, b);

    player.reload = 8;
}

/*TODO: add collision detection with walls*/
int move_player(int velx, int vely, int fake_it, int weave) 
{
    SDL_Rect newpos = player.pos;

    /* edge detection */
    if (newpos.x <= 4) {
        player.pos.x += 1;
        return;
    } else if (newpos.x >= W - 2*SPRITE_W) {
        player.pos.x -= 1;
        return;
    } else if (newpos.y <= 4) {
        player.pos.y += 1;
        return;
    } else if (newpos.y >= H - 2*SPRITE_H) {
        player.pos.y -= 1;
        return;
    }

    player.pos.x += velx; 
    player.pos.y += vely;
    //newpos.x += velx;
    //newpos.y += vely;
}

void update() 
{
	Entity *p = &player; // use a pointer to player

    /*if(player.state == PL_DEAD) {
        new_game();
        return;
    }*/

    if (p->reload > 0) {
        p->reload--;
    }

    if (!p->dx ^ !p->dy) { // moving only one direction
        move_player(p->dx, p->dy, 0, 1);
    }
    else if ((p->ylast || !p->dx) && p->dy) { 
        //only move 1 direction, but try the most recently pressed first
        int fake_it = move_player(0, p->dy, 0, 0);
        move_player(p->dx, 0, fake_it, 0);
    } else {
        int fake_it = move_player(p->dx, 0, 0, 0);
        move_player(0, p->dy, fake_it, 0);
    }

	if (gamestate == ALIVE) {
		game.frame += 1.0f;
	}

    /* update bullets */
    for (int i = 0; i < vector_size(&bullets); i++) {
        Entity *b = vector_get(&bullets, i);
        b->pos.x += b->dx;
        b->pos.y += b->dy;
        if (b->pos.y < 0) {
            vector_delete(&bullets, i);
        }
    }

    // bullet
    /*for (int i = 0; i < num_active_bullets; ++i) {
        if (bullet[i].alive) {
            bullet[i].pos.y += bullet[i].vel.y;
            bullet[i].pos.x += bullet[i].vel.x; 
        } 
    }*/
}

void draw() 
{
	/* background */
	SDL_Rect dest = {0, 0, W, H};
    SDL_SetRenderDrawBlendMode(game.renderer, SDL_BLENDMODE_BLEND);
    SDL_RenderCopy(game.renderer, game.background, NULL, &dest);

	/* objects & players*/
	//draw player
    SDL_RenderCopy(game.renderer, player.idle[(int)game.frame % IDLE_FRAMES], NULL,
        &(SDL_Rect){player.pos.x, player.pos.y, SPRITE_SCALE*SPRITE_W, SPRITE_SCALE*SPRITE_H});

    /* draw bullets */
    for (int i = 0; i < vector_size(&bullets); i++) {
        Entity *b = vector_get(&bullets, i);
        SDL_Rect dest;
        dest.x = b->pos.x;
        dest.y = b->pos.y;
        SDL_QueryTexture(b->texture, NULL, NULL, &dest.w, &dest.h);
        SDL_RenderCopy(game.renderer, b->texture, NULL,
            &(SDL_Rect){b->pos.x, b->pos.y, SPRITE_SCALE*BULLET_W, SPRITE_SCALE*BULLET_H});
    }

    // draw bullets
    /*for (int i = 0; i < num_active_bullets; i++) {
        if (!bullet[i].alive) {
            continue;
        }

        dest = bullet[i].pos;
        dest.w *= SPRITE_SCALE;
        dest.h *= SPRITE_SCALE;
        SDL_RenderCopy(renderer, bullet_texture, NULL, &dest);
            //&(SDL_Rect){bullet[i].pos.x, bullet[i].pos.y, SPRITE_SCALE*BULLET_W, SPRITE_SCALE*BULLET_H});
    }*/

	SDL_RenderPresent(game.renderer);
}
