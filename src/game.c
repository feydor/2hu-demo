#include "../include/common.h"
#include "../include/game.h"
#include "../include/entity.h"
#include "../include/safe_array.h"

// globals
SDL_Event event;
SDL_Texture *bullet_texture;
SDL_Texture *enemy_bullet_texture;
SDL_Texture *enemy_idle[ENEMY_IDLE_FRAMES];
SDL_Texture *right[9];
SDL_Texture *left[9];
Game game;
Entity player;
Camera camera;
int enemy_spawn_timer = 0;

/* entry point & game loop */
int main(int argc, char* argv[]) {
  UNUSED(argc);
  UNUSED(argv);
  
  setup();

  for (;;) {
    for (int i = 0; i < BUTTON_COUNT; i++) game.input.buttons[i].changed = 0; /*change all button.changed to 0*/

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

/* window and rendering setup */
void setup() {
  srand(time(NULL));

  /* Wipe system structs and init entity arrays*/
  memset(&game, 0, sizeof(Game));
  memset(&player, 0, sizeof(Entity));
  memset(&camera, 0, sizeof(Camera));
  camera.w = WINDOW_W;
  camera.h = WINDOW_H;
  sarray_init(&game.bullets, compare_entities);
  sarray_init(&game.enemies, compare_entities);
  game.scrolling_offset = 0;
  memset(&game.input, 0, sizeof(Input));

  SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO);

  game.window = SDL_CreateWindow("2hu", SDL_WINDOWPOS_UNDEFINED, \
      SDL_WINDOWPOS_UNDEFINED, WINDOW_W, WINDOW_H, SDL_WINDOW_SHOWN);
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "linear");
  game.renderer = SDL_CreateRenderer(game.window, -1, SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_ACCELERATED);
  if (!game.renderer) exit(fprintf(stderr, "Could not create SDL renderer\n"));

  /* Initialize SDL_mixer */
  if( Mix_OpenAudio( 22050, MIX_DEFAULT_FORMAT, 2, 2048 ) < 0 ) 
    exit(fprintf(stderr, "SDL_mixer could not initialize.\n"));

  /* Load wav files */
  game.shotsfx = Mix_LoadWAV("../res/plst00.wav");
  if( game.shotsfx == NULL ) exit(fprintf(stderr, "Failed to load shot sound effect.\n"));
  game.enemy_hitsfx = Mix_LoadWAV("../res/tan01.wav");
  Mix_Volume(2, MIX_MAX_VOLUME/2 + MIX_MAX_VOLUME/4); // channel 2 is hitsfx
  game.bgm = Mix_LoadMUS("../res/s1.wav");

  /* Load sprite files */
  //sprite_sheet = IMG_Load("res/reimu.png");
  //SDL_SetColorKey(surface, 1, 0xffff00);
  //pillar = SDL_CreateTextureFromSurface(renderer, surf);
  game.surface = SDL_LoadBMP("../res/bbg.bmp");
  game.background = SDL_CreateTextureFromSurface(game.renderer, game.surface);
  game.surface = SDL_LoadBMP("../res/fg.bmp");
  game.foreground = SDL_CreateTextureFromSurface(game.renderer, game.surface);
  game.surface = SDL_LoadBMP("../res/shot1.bmp");
  bullet_texture = SDL_CreateTextureFromSurface(game.renderer, game.surface);
  game.surface = SDL_LoadBMP("../res/eshot1.bmp");
  enemy_bullet_texture = SDL_CreateTextureFromSurface(game.renderer, game.surface);

  /* Load player sprites */
  for (int i = 0; i < IDLE_FRAMES; i++) {
    char file[80];
    sprintf(file, "../res/reimu-idle-%d.bmp", i+1);
    game.surface = SDL_LoadBMP(file);
    SDL_SetColorKey(game.surface, 1, 0xffff00);
    player.idle[i] = SDL_CreateTextureFromSurface(game.renderer, game.surface);
  }

  /* Load enemy sprites */
  for (int i = 0; i < ENEMY_IDLE_FRAMES; i++) {
    char file[80];
    sprintf(file, "../res/e%d.bmp", i+1);
    game.surface = SDL_LoadBMP(file);
    SDL_SetColorKey(game.surface, 1, 0xffff00);
    enemy_idle[i] = SDL_CreateTextureFromSurface(game.renderer, game.surface);
  }

  /* Init player */
  player.pos.x = STARTPX;
  player.pos.y = STARTPY;
  player.pos.w = SPRITE_W;
  player.pos.h = SPRITE_H;
  player.dir = NORTH;
  player.hp = 1;
  player.born = SDL_GetTicks();
  player.last_update = SDL_GetTicks();

  game.state = ALIVE;

  Mix_PlayMusic( game.bgm, -1 );
  //TTF_Init();
  //font = TTF_OpenFont("res/terminus.ttf", 42);
}

/* handle keypresses */
void key_press(int down) {
  switch (event.key.keysym.sym) {
    case SDLK_UP:
      game.input.buttons[BUTTON_UP].changed = down != game.input.buttons[BUTTON_UP].is_down; /* if down is different this frame */
      game.input.buttons[BUTTON_UP].is_down = down;
      break;
    case SDLK_DOWN:
      game.input.buttons[BUTTON_DOWN].changed = down != game.input.buttons[BUTTON_DOWN].is_down; /* if down is different this frame */
      game.input.buttons[BUTTON_DOWN].is_down = down;
      break;
    case SDLK_LEFT:
      game.input.buttons[BUTTON_LEFT].changed = down != game.input.buttons[BUTTON_LEFT].is_down; /* if down is different this frame */
      game.input.buttons[BUTTON_LEFT].is_down = down;
      break;
    case SDLK_RIGHT:
      game.input.buttons[BUTTON_RIGHT].changed = down != game.input.buttons[BUTTON_RIGHT].is_down; /* if down is different this frame */
      game.input.buttons[BUTTON_RIGHT].is_down = down;
      break;
    case SDLK_z:
      //if (player.reload == 0) {
      game.input.buttons[BUTTON_Z].changed = down != game.input.buttons[BUTTON_Z].is_down; /* if down is different this frame */
      game.input.buttons[BUTTON_Z].is_down = down;
      //} 

      break;
    case SDLK_ESCAPE:
      print_vectors();
      cleanup();
      exit(0);
  }
}

void spawn_bullet() {
  Mix_PlayChannel( -1, game.shotsfx, 0 ); // play sfx
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
    b->is_enemy_bullet = 0;
    b->last_update = SDL_GetTicks();
    b->born = SDL_GetTicks();
    b->texture = bullet_texture;
    //SDL_QueryTexture(b->texture, NULL, NULL, &b->pos.w, &b->pos.h);

    sarray_pushback(&game.bullets, b);

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
    Entity *b = malloc(sizeof(Entity));
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

    calculate_slope(player.pos.x + (player.pos.w / 2),
        player.pos.y + (player.pos.h / 2), e->pos.x, e->pos.y,
        &b->dx, &b->dy);
    b->dx *= ENEMY_BULLET_SPD;
    b->dy *= ENEMY_BULLET_SPD;

    //e->reload = (rand() % 15 * 4); // fps = 60
    e->fire_time = (rand() % (e->fire_delay_max - e->fire_delay_min) + e->fire_delay_min);
    sarray_pushback(&game.bullets, b);
    y_variation += 2*ENEMY_BULLET_H;
  }
}

void spawn_enemies() {
  if (--enemy_spawn_timer <= 0) {
    Entity *e = malloc(sizeof(Entity));
    memset(e, 0, sizeof(Entity));
    e->hp = 1;

    int upper = LEVEL_W - 2 * ENEMY_W;
    int lower = 2 * ENEMY_W;
    e->pos.x = (rand() % (upper - lower + 1)) + lower;
    e->pos.y = 0;
    e->pos.w = ENEMY_W;
    e->pos.h = ENEMY_H;
    e->last_update = SDL_GetTicks();
    e->born = SDL_GetTicks();
    e->fire_delay_max = 3 * 60;
    e->fire_delay_min = 1 * 60;
    e->fire_time = (rand() % (e->fire_delay_max - e->fire_delay_min) + e->fire_delay_min);
    //SDL_QueryTexture(e->enemy_idle[0], NULL, NULL, &e->pos.w, &e->pos.h);

    e->dy = (2 + (rand() % 3));
    e->dx = 0;

    sarray_pushback(&game.enemies, e);
    //e->reload = (rand() % 60 * 2); // fps = 60
    enemy_spawn_timer = 40 + (rand() % 60);
  }
}


/* Calculates 2D cubic Catmull-Rom spline. */
SDL_Point *spline(SDL_Point *p0, SDL_Point *p1, SDL_Point *p2, SDL_Point *p3, double t) {
  SDL_Point newp = {
    0.5 * ((2 * p1->x) + 
        t * (( -p0->x + p2->x) + 
          t * ((2 * p0->x -5 * p1->x +4 * p2->x -p3->x) + 
            t * ( -p0->x +3 * p1->x -3 * p2->x +p3->x)))),
    0.5 * ((2 * p1->y) + 
        t * (( -p0->y + p2->y) + 
          t * ((2 * p0->y -5 * p1->y +4 * p2->y -p3->y) + 
            t * (  -p0->y +3 * p1->y -3 * p2->y +p3->y))))
  };

  SDL_Point *p = &newp;
  return p;
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

void calculate_slope(int x1, int y1, int x2, int y2, float *dx, float *dy) {
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

bool foreach_enemy_collision(void *arr, void *enemy, void *bullet, int enemy_index) {
  SafeArray *enemies = (SafeArray *) arr;
  Entity *e = (Entity *) enemy;
  Entity *b = (Entity *) bullet;
  UNUSED(enemy_index);
  
  if (collision(b, e)) {
      Mix_PlayChannel( 2, game.enemy_hitsfx, 0 );
      b->hp = 0;
      e->hp = 0;
      sarray_delete(enemies, enemy);
      return true;
  }
    return false;
}

// update loop for bullets
void update_foreach_bullet(void *arr, void *bullet, void* idx) {
  Entity *curr_bullet = (Entity *) bullet;
  UNUSED(idx);
  SafeArray *bullets = (SafeArray *) arr;
  
  // delete player bullets that hit
  if (!curr_bullet->is_enemy_bullet) {
      /* iterate through enemies and check for collisions with Entity b*/
      bool (*callback)(void *, void *, void *, int); 
      callback = foreach_enemy_collision;

      _add_queued(&game.enemies);
      _remove_queued(&game.enemies);
      for (int i = 0; i < game.enemies.size; i++) {
        if ( queue_has_item(game.enemies.remove_queue, game.enemies.items[i]) )
          continue;
        bool collided = callback( (void *) &game.enemies, game.enemies.items[i], (void *) curr_bullet, i);
        if (collided) {
            return;      
        }
      }
      _remove_queued(&game.enemies);
  }
    
  // update bullet positions
  curr_bullet->pos.x += curr_bullet->dx;
  curr_bullet->pos.y += curr_bullet->dy;
  curr_bullet->last_update = SDL_GetTicks();
    
  // check for out of bounds
  if ((curr_bullet->pos.y < 0) || (curr_bullet->pos.x < 0) || (curr_bullet->pos.x > LEVEL_W) || (curr_bullet->hp == 0)) { 
    sarray_delete(bullets, curr_bullet);
  }
}

void update_foreach_enemy(void *arr, void *enemy, void* idx) {
  Entity *e = (Entity *) enemy;
  UNUSED(idx);
  SafeArray *enemies = (SafeArray *) arr;
  
  // delete dead enemies
  if (e->hp == 0) {
      sarray_delete(enemies, e);
      return;
  }
  
  // update enemy positions
  e->pos.x += e->dx;
  e->pos.y += e->dy;
  e->last_update = SDL_GetTicks();
  //e->reload--;
  e->fire_time -= 1;
  
  // check for out of bounds
  if ((e->pos.y > LEVEL_H - ENEMY_H)) {
      sarray_delete(enemies, e);
  } else if (e->fire_time <= 0) {
      spawn_enemy_bullet(e);
  } 
}

void update() {
  /* update player */
  Entity *p = &player; // use a pointer to player
  p->last_update = SDL_GetTicks();

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

  /* Center the camera on the player */
  camera.x = ( p->pos.x + SPRITE_W / 2 ) - WINDOW_W / 2;
  camera.y = ( p->pos.y + SPRITE_H / 2 ) - WINDOW_H / 2;

  /* Keep the camera in bounds */
  if ( camera.x < 0 ) { 
    camera.x = 0;
  }
  if ( camera.y < 0 ) {
    camera.y = 0;
  }
  if ( camera.x > LEVEL_W - camera.w ) {
    camera.x = LEVEL_W - camera.w;
  }
  if( camera.y > LEVEL_H - camera.h ) {
    camera.y = LEVEL_H - camera.h;
  }

  // fire rates : 3 seems good for powerup
  if (is_held(BUTTON_Z) && game.scrolling_offset % 7 == 0) {
    spawn_bullet();
  }

  if (game.state == ALIVE) {
    game.frame += 1.0f;
  }

  /* Update scrolling offset */
  game.scrolling_offset += SCROLLING_SPEED;
  if (game.scrolling_offset > LEVEL_H) {
    game.scrolling_offset = 0;
  }

  /* update bullets */
  void (*callback) (void *, void *, void *);
  callback = update_foreach_bullet;
  sarray_foreach(&game.bullets, callback);
  
  /* update enemies */
  callback = update_foreach_enemy;
  sarray_foreach(&game.enemies, callback);
}


void print_vectors() {
  printf("DEBUG: Remaining array elements:\nEnemies:\n");
  for (int i = 0; i < sarray_size(&game.enemies); i++) {
    Entity *e = sarray_get(&game.enemies, i);
    print_entity(e);
  }    
  printf("Bullets:\n");
  for (int i = 0; i < sarray_size(&game.bullets); i++) {
    Entity *b = sarray_get(&game.bullets, i);
    print_entity(b);
  }    
}

void draw() 
{
  /* Render screen */
  SDL_SetRenderDrawColor( game.renderer, 0xFF, 0xFF, 0xFF, 0xFF );
  SDL_RenderClear( game.renderer );

  /* Render background */
  // rect transformation is bgRect.x - camera.x
  SDL_SetRenderDrawBlendMode(game.renderer, SDL_BLENDMODE_BLEND);

  // render two baclgrounds, on top of each other
  SDL_RenderCopy(game.renderer, game.background, NULL,
      &(SDL_Rect){0 - camera.x, game.scrolling_offset - camera.y, LEVEL_W, LEVEL_H});
  SDL_RenderCopy(game.renderer, game.background, NULL,
      &(SDL_Rect){0 - camera.x, (game.scrolling_offset - LEVEL_H) - camera.y, LEVEL_W, LEVEL_H});

  /* foreground */
  /* IMPORTANT: Change the first two values of dest(x, y, w, h) for
   * a special surprise. */
  //~ SDL_SetRenderDrawBlendMode(game.renderer, SDL_BLENDMODE_BLEND);
  //~ SDL_RenderCopy(game.renderer, game.foreground, NULL, 
  //~ &(SDL_Rect){0 - camera.x, 0 - camera.y, LEVEL_W, LEVEL_H});

  /* objects & players*/
  //draw player
  SDL_RenderCopy(game.renderer, player.idle[(int)game.frame % IDLE_FRAMES], NULL,
      &(SDL_Rect){player.pos.x - camera.x, player.pos.y - camera.y, SPRITE_W, SPRITE_H});

  /* draw enemies */
  for (int i = 0; i < sarray_size(&game.enemies); i++) {
    Entity *e = sarray_get(&game.enemies, i);
    //SDL_Rect dest;
    //dest = e->pos;
    //SDL_QueryTexture(e->texture, NULL, NULL, &dest.w, &dest.h);
    int index = rand() % ENEMY_IDLE_FRAMES;
    SDL_RenderCopy(game.renderer, enemy_idle[index], NULL,
        &(SDL_Rect){e->pos.x - camera.x, e->pos.y - camera.y, ENEMY_W, ENEMY_H});
  }

  /* draw bullets */
  for (int i = 0; i < sarray_size(&game.bullets); i++) {
    Entity *b = sarray_get(&game.bullets, i);
    //SDL_Rect dest;
    //dest = b->pos;
    //SDL_QueryTexture(b->texture, NULL, NULL, &dest.w, &dest.h);
    SDL_RenderCopyEx(game.renderer, b->texture, NULL,
        &(SDL_Rect){b->pos.x - camera.x, b->pos.y - camera.y, b->pos.w, b->pos.h},
        (double)(game.frame % 360), NULL, SDL_FLIP_NONE);
  }

  SDL_RenderPresent(game.renderer);
}

void cleanup() {
  sarray_free(&game.enemies);
  sarray_free(&game.bullets);
  Mix_FreeChunk(game.shotsfx);
  Mix_FreeChunk(game.enemy_hitsfx);
  Mix_FreeMusic(game.bgm);
  SDL_DestroyRenderer(game.renderer);
  SDL_DestroyWindow(game.window);
  SDL_Quit();
}