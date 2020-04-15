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
#define BULLET_H 32             /* bullet height*/
#define BULLET_W 28             /* bullet width */
#define ENEMY_BULLET_H 24             /* bullet height*/
#define ENEMY_BULLET_W 24             /* bullet width */
#define ENEMY_H 32              /* enemy height*/
#define ENEMY_W 32              /* enemy width */
#define SPRITE_SCALE 1.5	    /* 2x sprite magnification */
#define PLYR_SPD 6              /* units per frame */
#define BULLET_SPD 12
#define ENEMY_BULLET_SPD 3
#define IDLE_FRAMES 9			/* number of idle frames */
#define ENEMY_IDLE_FRAMES 5
#define STARTPX (W/2)           /* starting position */
#define STARTPY (H/2)           /* starting position */
#define MAX_BULLETS 10          /* max # of bullets */
#define MAX(X, Y) (((X) > (Y)) ? (X) : (Y))
#define MIN(X, Y) (((X) < (Y)) ? (X) : (Y)) 

enum gamestates {READY, ALIVE, GAMEOVER} gamestate = READY;
enum playerstates {PL_NORMAL, PL_FIRE, PL_HURT, PL_DYING, PL_DEAD};
enum dir {NORTH, WEST, EAST, SOUTH};

SDL_Event event;
SDL_Texture *bullet_texture;
SDL_Texture *enemy_bullet_texture;
SDL_Texture *enemy_idle[ENEMY_IDLE_FRAMES];
SDL_Texture *right[9];
SDL_Texture *left[9];

/* Input */
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
    BUTTON_ESC,
    
    BUTTON_COUNT,
};

typedef struct {
    Button buttons[BUTTON_COUNT];  
} Input;

Input input = {0};

#define pressed(b) (input.buttons[b].is_down && input.buttons[b].changed)
#define released(b) (!input.buttons[b].is_down && input.buttons[b].changed)
#define is_down(b) (input.buttons[b].is_down)
#define is_held(b) (input.buttons[b].is_down && !input.buttons[b].changed)
/* End input*/

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
    float dx;
    float dy;
    int hp;
    int dir;
    int reload;
    int is_enemy_bullet;
    Uint32 born;
    Uint32 last_update;
    SDL_Texture *texture;
    SDL_Texture *idle[IDLE_FRAMES];
    //SDL_Texture *enemy_idle[ENEMY_IDLE_FRAMES];
} Entity;

void setup();
void key_press(int down);
void spawn_bullet();
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

Game game;
Entity bullet;
Entity player;
vector bullets;
vector enemies;
int enemy_spawn_timer = 0;

/* entry point & game loop */
int main(int argc, char* argv[]) {
	setup();
    
	for (;;) {
        for (int i = 0; i < BUTTON_COUNT; i++) input.buttons[i].changed = 0; /*change all button.changed to 0*/

		while (SDL_PollEvent(&event)) {
			switch (event.type) {
				case SDL_QUIT: cleanup(); exit(0);
				case SDL_KEYDOWN: key_press(1); break;
				case SDL_KEYUP: key_press(0); break;
			}
		}

        spawn_enemies();
		update();
		draw();
        SDL_Delay(1000 / 60);
        //SDL_Delay(rand() % 25);

        // End frame timing
		/*Uint32 endTicks = SDL_GetTicks();
		Uint64 endPerf = SDL_GetPerformanceCounter();
		Uint64 framePerf = endPerf - startPerf;
		float frameTime = (endTicks - startTicks) / 1000.0f;
		totalFrameTicks += endTicks - startTicks;
        printf("Current FPS: %f\n", 1.0f / frameTime);
        printf("Average FPS: %f\n", 1000.0f / ((float)totalFrameTicks / totalFrames));
        printf("Current Perf: %f\n", framePerf);*/
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
        vector_init(&enemies);

        SDL_Init(SDL_INIT_VIDEO);

        game.window = SDL_CreateWindow("2hu",
                SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, W, H, SDL_WINDOW_SHOWN);
        SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "linear");
        game.renderer = SDL_CreateRenderer(game.window, -1, SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_ACCELERATED);
        if (!game.renderer) exit(fprintf(stderr, "Could not create SDL renderer\n"));

        //sprite_sheet = IMG_Load("res/reimu.png");
        //SDL_SetColorKey(surface, 1, 0xffff00);
        //pillar = SDL_CreateTextureFromSurface(renderer, surf);
        game.surface = SDL_LoadBMP("res/bg.bmp");
        game.background = SDL_CreateTextureFromSurface(game.renderer, game.surface);
        game.surface = SDL_LoadBMP("res/shot1.bmp");
        bullet_texture = SDL_CreateTextureFromSurface(game.renderer, game.surface);
        game.surface = SDL_LoadBMP("res/eshot1.bmp");
        enemy_bullet_texture = SDL_CreateTextureFromSurface(game.renderer, game.surface);

        /* loads player sprites*/
		for (int i = 0; i < IDLE_FRAMES; i++) {
			char file[80];
			sprintf(file, "res/reimu-idle-%d.bmp", i+1);
			game.surface = SDL_LoadBMP(file);
			SDL_SetColorKey(game.surface, 1, 0xffff00);
			player.idle[i] = SDL_CreateTextureFromSurface(game.renderer, game.surface);
		}

        /* load enemy sprites */
        for (int i = 0; i < ENEMY_IDLE_FRAMES; i++) {
			char file[80];
			sprintf(file, "res/e%d.bmp", i+1);
			game.surface = SDL_LoadBMP(file);
			SDL_SetColorKey(game.surface, 1, 0xffff00);
			enemy_idle[i] = SDL_CreateTextureFromSurface(game.renderer, game.surface);
		}
        
        /* init player */
        player.pos.x = STARTPX;
        player.pos.y = STARTPY;
        player.pos.w = SPRITE_W;
        player.pos.h = SPRITE_H;
        player.dir = NORTH;
        player.hp = 1;
        player.born = SDL_GetTicks();
        player.last_update = SDL_GetTicks();

        gamestate = ALIVE;

        //TTF_Init();
        //font = TTF_OpenFont("res/terminus.ttf", 42);
}

/* handle keypresses */
void key_press(int down) 
{
    //if(event.key.repeat) return;

	//int amt = down ? PLYR_SPD : -PLYR_SPD;
   
	switch (event.key.keysym.sym) {
		case SDLK_UP:
            input.buttons[BUTTON_UP].changed = down != input.buttons[BUTTON_UP].is_down; /* if down is different this frame */
            input.buttons[BUTTON_UP].is_down = down;
            break;
        case SDLK_DOWN:
            input.buttons[BUTTON_DOWN].changed = down != input.buttons[BUTTON_DOWN].is_down; /* if down is different this frame */
            input.buttons[BUTTON_DOWN].is_down = down;
            break;
        case SDLK_LEFT:
            input.buttons[BUTTON_LEFT].changed = down != input.buttons[BUTTON_LEFT].is_down; /* if down is different this frame */
            input.buttons[BUTTON_LEFT].is_down = down;
            break;
        case SDLK_RIGHT:
            input.buttons[BUTTON_RIGHT].changed = down != input.buttons[BUTTON_RIGHT].is_down; /* if down is different this frame */
            input.buttons[BUTTON_RIGHT].is_down = down;
            break;
        case SDLK_z:
            //if (player.reload == 0) {
                input.buttons[BUTTON_Z].changed = down != input.buttons[BUTTON_Z].is_down; /* if down is different this frame */
                input.buttons[BUTTON_Z].is_down = down;
            //} 
            
            /*if (down && player.reload == 0) {
                //printf("spawn bullet\n");
                spawn_bullet();
            }*/
            break;
		case SDLK_ESCAPE:
            cleanup();
            print_vectors();
            exit(0);
	}
}

void spawn_bullet() 
{
    int y_variation = 0;
    for (int i = 0; i < 5; i++) {
        Entity *b;
        b = malloc(sizeof(Entity));
        memset(b, 0, sizeof(Entity));
        b->pos.x = player.pos.x + (player.pos.w/2) - (BULLET_W/2);
        b->pos.y = player.pos.y + (player.pos.h/2) - (BULLET_H/2) + y_variation;
        b->pos.w = BULLET_W;
        b->pos.h = BULLET_H;
        b->dx = 0;
        b->dy = -1*BULLET_SPD;
        b->hp = 1;
        b->is_enemy_bullet = 0;
        b->last_update = SDL_GetTicks();
        b->born = SDL_GetTicks();
        b->texture = bullet_texture;
        //SDL_QueryTexture(b->texture, NULL, NULL, &b->pos.w, &b->pos.h);

        vector_pushback(&bullets, b);

        y_variation += 20;
    }
    player.reload = 16; // reload rate
}

void spawn_enemy_bullet(Entity *e) {
    int upper = 1;
    int lower = 1;
    int rand_num_bullets = (rand() % (upper - lower + 1)) + lower;
    int y_variation = 0;
    for (int i = 0; i < rand_num_bullets; i++) {
        Entity *b;
        b = malloc(sizeof(Entity));
        memset(b, 0, sizeof(Entity));
        b->pos.x = e->pos.x + (e->pos.w / 2);
        b->pos.y = e->pos.y + (e->pos.h / 2) + y_variation + ENEMY_BULLET_H + 5;
        b->pos.w = ENEMY_BULLET_W;
        b->pos.h = ENEMY_BULLET_H;
        b->hp = 1;
        b->is_enemy_bullet = 1;
        b->last_update = SDL_GetTicks();
        b->born = SDL_GetTicks();
        b->texture = enemy_bullet_texture;

        calculate_slope(player.pos.x + (player.pos.w / 2), player.pos.y + (player.pos.h / 2), e->pos.x, e->pos.y,
                &b->dx, &b->dy);
        b->dx *= ENEMY_BULLET_SPD;
        b->dy *= ENEMY_BULLET_SPD;

        e->reload = (rand() % 15 * 2); // fps = 60
        vector_pushback(&bullets, b);
        y_variation += 2*ENEMY_BULLET_H;
    }
}

void spawn_enemies() {
    if (--enemy_spawn_timer <= 0) {
        Entity *e = malloc(sizeof(Entity));
        memset(e, 0, sizeof(Entity));
        e->hp = 1;
        
        int upper = W - 2 * ENEMY_W;
        int lower = 2 * ENEMY_W;
        e->pos.x = (rand() % (upper - lower + 1)) + lower;
        e->pos.y = 0;
        e->pos.w = ENEMY_W;
        e->pos.h = ENEMY_H;
        e->last_update = SDL_GetTicks();
        e->born = SDL_GetTicks();
        //SDL_QueryTexture(e->enemy_idle[0], NULL, NULL, &e->pos.w, &e->pos.h);
        
        e->dy = (2 + (rand() % 3));
        e->dx = 0;
        
        vector_pushback(&enemies, e);
        e->reload = (rand() % 60 * 2); // fps = 60
        enemy_spawn_timer = 40 + (rand() % 60);
    }
}

int move_player(int velx, int vely, int fake_it, int weave) 
{
    SDL_Rect newpos = player.pos;
    /* edge detection */
    /*if (newpos.x <= 4) {
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
    }*/
}

int collision(Entity *e1, Entity *e2) 
{
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

void calculate_slope(int x1, int y1, int x2, int y2, float *dx, float *dy)
{
	int steps = MAX(abs(x1 - x2), abs(y1 - y2));

	if (steps == 0)
	{
		*dx = *dy = 0;
		return;
	}

	*dx = (x1 - x2);
	*dx /= steps;

	*dy = (y1 - y2);
	*dy /= steps;
}

int bullet_hit(Entity *b) 
{
    /* iterate through enemies and check for collisions with Entity b*/    
    for (int i = 0; i < vector_size(&enemies); i++) {
        Entity *e = vector_get(&enemies, i);
        if (collision(b, e)) {
            b->hp = 0;
            e->hp = 0;
            //vector_delete(&enemies, i);
            return 1;
        } 
        return 0;
    }
}

void update() 
{
    /* update player */
	Entity *p = &player; // use a pointer to player
    p->last_update = SDL_GetTicks;

    if (p->reload > 0) {
        p->reload--;
    }

    /* handle player input */
    if (is_down(BUTTON_UP)) {
        p->dy = PLYR_SPD;
        p->pos.y -= p->dy;
        p->dir = NORTH;
    } 
    if (is_down(BUTTON_DOWN)) {
        p->dy = PLYR_SPD;
        p->pos.y += p->dy;
        p->dir = SOUTH;
    } 
    if (is_down(BUTTON_LEFT)) {
        p->dx = PLYR_SPD;
        p->pos.x -= p->dx;
        p->dir = SOUTH;
    } 
    if (is_down(BUTTON_RIGHT)) {
        p->dx = PLYR_SPD;
        p->pos.x += p->dx;
        p->dir = SOUTH;
    }
    if (is_held(BUTTON_Z)) {
        spawn_bullet();
    }

    //move_player(p->dx, p->dy, 0, 1);

	if (gamestate == ALIVE) {
		game.frame += 1.0f;
	}

    /* update enemies */
    for (int i = 0; i < vector_size(&enemies); i++) {
        Entity *e = vector_get(&enemies, i);
        e->pos.x += e->dx;
        e->pos.y += e->dy;
        e->last_update = SDL_GetTicks();
        e->reload--;
        if ((e->pos.y > H - ENEMY_H) || e->hp == 0) {
            vector_delete(&enemies, i);
        } else if (e->hp != 0 && e->reload <= 0) {
            spawn_enemy_bullet(e);
        }
    }

    /* update bullets */
    for (int i = 0; i < vector_size(&bullets); i++) {
        Entity *b = vector_get(&bullets, i);
        if (!b->is_enemy_bullet && bullet_hit(b)) {
            b->hp = 0;
        }
        b->pos.x += b->dx;
        b->pos.y += b->dy;
        b->last_update = SDL_GetTicks();
        if ((b->pos.y < 0) || (b->pos.x < 0) || (b->pos.x > W) || (b->hp == 0)) { 
            vector_delete(&bullets, i);
        }
    }
}


void print_vectors() {
    printf("DEBUG: Remaining array elements:\nEnemies:\n");
    for (int i = 0; i < vector_size(&enemies); i++) {
        Entity *e = vector_get(&enemies, i);
        printf("(%i, %i)\n", e->pos.x, e->pos.y);
    }    
    printf("Bullets:\n");
    for (int i = 0; i < vector_size(&bullets); i++) {
        Entity *b = vector_get(&bullets, i);
        printf("(%i, %i)\n", b->pos.x, b->pos.y);
    }    
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
        &(SDL_Rect){player.pos.x, player.pos.y, SPRITE_W, SPRITE_H});

    /* draw enemies */
    for (int i = 0; i < vector_size(&enemies); i++) {
        Entity *e = vector_get(&enemies, i);
        SDL_Rect dest;
        dest = e->pos;
        //SDL_QueryTexture(e->texture, NULL, NULL, &dest.w, &dest.h);
        int index = rand() % ENEMY_IDLE_FRAMES;
        SDL_RenderCopy(game.renderer, enemy_idle[index], NULL,
            &(SDL_Rect){e->pos.x, e->pos.y, ENEMY_W, ENEMY_H});
    }

    /* draw bullets */
    for (int i = 0; i < vector_size(&bullets); i++) {
        Entity *b = vector_get(&bullets, i);
        SDL_Rect dest;
        dest = b->pos;
        //SDL_QueryTexture(b->texture, NULL, NULL, &dest.w, &dest.h);
        SDL_RenderCopy(game.renderer, b->texture, NULL,
            &(SDL_Rect){b->pos.x, b->pos.y, b->pos.w, b->pos.h});
    }

	SDL_RenderPresent(game.renderer);
}

void cleanup() 
{
    SDL_DestroyRenderer(game.renderer);
	SDL_DestroyWindow(game.window);
	SDL_Quit();
}