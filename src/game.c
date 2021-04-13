#include "../include/common.h"
#include "../include/game.h"
#include "../include/entity.h"
#include "../include/safe_array.h"


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
  camera.w = LEVEL_W;
  camera.h = LEVEL_H;
  game.scrolling_offset = 0;
  memset(&game.input, 0, sizeof(Input));
  
  sarray_init(&game.bullets, compare_entities);
  sarray_init(&game.enemies, compare_entities);
  sarray_init(&game.items, compare_entities);

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
  Mix_Volume(PLAYER_SHOT_CHANNEL, MIX_MAX_VOLUME/4);
  if( game.shotsfx == NULL ) exit(fprintf(stderr, "Failed to load shot sound effect.\n"));
  game.enemy_hitsfx = Mix_LoadWAV("../res/tan01.wav");
  Mix_Volume(ENEMY_HIT_CHANNEL, MIX_MAX_VOLUME/5);
  game.player_hitsfx = Mix_LoadWAV("../res/damage.wav");
  Mix_Volume(PLAYER_HIT_CHANNEL, MIX_MAX_VOLUME/2 + MIX_MAX_VOLUME/4);
  game.player_deathsfx = Mix_LoadWAV("../res/death.wav");
  Mix_Volume(PLAYER_DEATH_CHANNEL, MIX_MAX_VOLUME/2);
  game.bgm = Mix_LoadMUS("../res/s1.wav");
  Mix_Volume(STAGE_MUSIC_CHANNEL, MIX_MAX_VOLUME/3);
  game.player_bombsfx = Mix_LoadWAV("../res/bomb.wav");
  Mix_Volume(PLAYER_BOMB_CHANNEL, MIX_MAX_VOLUME/2);

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
  game.surface = SDL_LoadBMP("../res/powerup.bmp");
  game.powerup = SDL_CreateTextureFromSurface(game.renderer, game.surface);
  game.surface = SDL_LoadBMP("../res/score.bmp");
  game.score = SDL_CreateTextureFromSurface(game.renderer, game.surface);

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
  player.hp = PLAYER_LIVES;
  player.bomb_count = PLAYER_BOMBS;
  player.born = SDL_GetTicks();
  player.last_update = SDL_GetTicks();
  player.alpha = OPAQUE;

  game.state = ALIVE;

  Mix_PlayMusic( game.bgm, -1 );

  // init font
  if(TTF_Init() == -1) {
    printf("TTF_Init: %s\n", TTF_GetError());
    exit(2);
  }

  // default dpi is 72, so pt == px
  game.font=TTF_OpenFont("../res/MagicDecimal.ttf", 128);
  if(!game.font) {
    printf("TTF_OpenFont: %s\n", TTF_GetError());
  }

  // init UI text
  game.white = (SDL_Color) {255, 255, 255, 255};
  game.yellow = (SDL_Color) {255, 255, 0, 255};
  game.surface = TTF_RenderUTF8_Solid(game.font, u8"生活x", game.white);
  // game.UI = SDL_CreateTextureFromSurface(game.renderer, game.surface);
  if(!game.surface) {
    //handle error here, perhaps print TTF_GetError at least
    printf("TTF_RenderText_Solid: %s\n", TTF_GetError());
  }

  game.UI = SDL_CreateTextureFromSurface(game.renderer, game.surface);

}

/* handle keypresses */
void key_press(int down) {
  switch (event.key.keysym.sym) {
    case SDLK_UP:
      game.input.buttons[BUTTON_UP].changed = down != game.input.buttons[BUTTON_UP].is_down; /* if down is different this frame */
      game.input.buttons[BUTTON_UP].is_down = down;
      break;
    case SDLK_DOWN:
      game.input.buttons[BUTTON_DOWN].changed = down != game.input.buttons[BUTTON_DOWN].is_down;
      game.input.buttons[BUTTON_DOWN].is_down = down;
      break;
    case SDLK_LEFT:
      game.input.buttons[BUTTON_LEFT].changed = down != game.input.buttons[BUTTON_LEFT].is_down;
      game.input.buttons[BUTTON_LEFT].is_down = down;
      break;
    case SDLK_RIGHT:
      game.input.buttons[BUTTON_RIGHT].changed = down != game.input.buttons[BUTTON_RIGHT].is_down;
      game.input.buttons[BUTTON_RIGHT].is_down = down;
      break;
    case SDLK_z:
      game.input.buttons[BUTTON_Z].changed = down != game.input.buttons[BUTTON_Z].is_down;
      game.input.buttons[BUTTON_Z].is_down = down;
      break;
    case SDLK_x:
      game.input.buttons[BUTTON_X].changed = down != game.input.buttons[BUTTON_X].is_down;
      game.input.buttons[BUTTON_X].is_down = down;
      break;
    case SDLK_ESCAPE:
      print_vectors();
      cleanup();
      exit(0);
  }
}

bool check_for_enemy_collision(void *arr, void *enemy, void *bullet, int enemy_index) {
  SafeArray *enemies = (SafeArray *) arr;
  Entity *e = (Entity *) enemy;
  Entity *b = (Entity *) bullet;
  UNUSED(enemy_index);
  
  if (collision(b, e)) {
      Mix_PlayChannel( ENEMY_HIT_CHANNEL, game.enemy_hitsfx, 0 );
      
      int roll_out_of_100 = (rand() % 101);

      if (roll_out_of_100 < POWERUP_DROPRATE) {
        spawn_item(e, POWERUP);
      } else if (roll_out_of_100 < SCORE_DROPRATE) {
        spawn_item(e, SCORE);
      }

      // TODO: Move into increase_player_score function
      game.player_score += SCORE_PER_KILL;
      if (game.player_hiscore < game.player_score)
        game.player_hiscore = game.player_score;

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
  
  // delete enemies && player bullets that hit
  bool (*callback)(void *, void *, void *, int); 
  if (!curr_bullet->is_enemy_bullet) {
      /* iterate through enemies and check for collisions with Entity b*/
      callback = check_for_enemy_collision;

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
  } else {
    // check enemy bullet for contact with player
    if ( collision(curr_bullet, &player) ) {
      if (player.hit_cooldown < 1) {
        Mix_PlayChannel( PLAYER_HIT_CHANNEL, game.player_hitsfx, 0 );
        player.hp -= 1;
        player.hit_cooldown = PLAYER_HIT_COOLDOWN; 
      }
      curr_bullet->hp = 0;
      sarray_delete(bullets, curr_bullet);
    }
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
  e->fire_time -= 1;
  
  // check for out of bounds
  if ((e->pos.y > LEVEL_H - ENEMY_H)) {
      sarray_delete(enemies, e);
  // possibly fire bullets
  } else if (e->fire_time <= 0) {
      spawn_enemy_bullet(e);
      // spawn_enemy_bullet_flower(e);
  } 
}

void update_foreach_item(void *arr, void *item, void* idx) {
  SafeArray *items = (SafeArray *) arr;
  Entity *itm = (Entity *) item;
  UNUSED(idx);

  // delete dead items
  if (itm->hp == 0) {
    sarray_delete(items, itm);
    return;
  }

  // update positions
  itm->pos.x += itm->dx;
  itm->pos.y += itm->dy;
  itm->last_update = SDL_GetTicks();

  // check for out of bounds
  if (itm->pos.y > LEVEL_H - itm->pos.h) {
    sarray_delete(items, itm);
  }

  //check for collision with player
  if (collision(&player, itm)) {
    itm->hp = 0;
    sarray_delete(items, itm);

    switch(itm->type) {
      case POWERUP:
        game.player_shot += 1;
        break;
      case SCORE:
        game.player_score += SCORE_DROP_AMOUNT;

        if (game.player_hiscore < game.player_score) {
          game.player_hiscore = game.player_score;
        }
        break;
    }
  }
}

void update() {
  /* update player */
  Entity *p = &player; // use a pointer to player
  p->last_update = SDL_GetTicks();

  if (p->hp < 0) {
    // death sound play
    Mix_PlayChannel( PLAYER_DEATH_CHANNEL, game.player_deathsfx, 0 );
    p->hp = PLAYER_LIVES;
    game.state = GAMEOVER;
  }

  if (p->reload > 0) {
    p->reload--;
  }

  if (p->hit_cooldown > 0) {
    p->hit_cooldown--;
    player.alpha = SEMI_TRANSPARENT;
  } else {
    player.alpha = OPAQUE;
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
  camera.x = ( p->pos.x + SPRITE_W / 2 ) - LEVEL_W / 2;
  camera.y = ( p->pos.y + SPRITE_H / 2 ) - LEVEL_H / 2;

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

  // bomb fire rate
  if (p->bomb_count > 0 && p->bomb_cooldown <= 0 && is_held(BUTTON_X)) {
    fire_bomb();
  }

  if (p->bomb_cooldown > 0) {
    p->bomb_cooldown--;
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

  /* update items */
  callback = update_foreach_item;
  sarray_foreach(&game.items, callback);
  
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
  printf("Items:\n");
  for (int i = 0; i < sarray_size(&game.items); i++) {
    Entity *itm = sarray_get(&game.items, i);
    print_entity(itm);
  }    
}

void draw() {
  /* Render screen */

  // flash the screen if bomb_cooldown
  if (player.bomb_cooldown > 0 && game.frame % IDLE_FRAMES == 2) {
    SDL_SetRenderDrawColor( game.renderer, 0xFF, 0xFF, 0xFF, 0x7F );
  } else {
    SDL_SetRenderDrawColor( game.renderer, 0x00, 0x00, 0x00, 0x00 );
  }
  SDL_RenderClear( game.renderer );

  /* Render background */
  // rect transformation is bgRect.x - camera.x
  SDL_SetRenderDrawBlendMode(game.renderer, SDL_BLENDMODE_BLEND);

  // render two backgrounds, on top of each other
  /*
  SDL_RenderCopy(game.renderer, game.background, NULL,
        &(SDL_Rect){0 - camera.x, game.scrolling_offset - camera.y, LEVEL_W, LEVEL_H});
  SDL_RenderCopy(game.renderer, game.background, NULL,
      &(SDL_Rect){0 - camera.x, (game.scrolling_offset - LEVEL_H) - camera.y, LEVEL_W, LEVEL_H});
      */

  /* foreground */
  /* IMPORTANT: Change the first two values of dest(x, y, w, h) for
   * a special surprise. */
  // SDL_SetRenderDrawBlendMode(game.renderer, SDL_BLENDMODE_BLEND);
  // SDL_RenderCopy(game.renderer, game.foreground, NULL, 
  // &(SDL_Rect){0 - camera.x, 0 - camera.y, LEVEL_W, LEVEL_H});


  // render UI
  int xpos = 100;
  int xgap = 50;
  char *scoreUi = "  HiScore  ";
  char scoreStr[255];
  
  sprintf(scoreStr, "%d", game.player_hiscore);
  
  game.surface = TTF_RenderUTF8_Solid(game.font, scoreUi, game.yellow);
  game.UI = SDL_CreateTextureFromSurface(game.renderer, game.surface);
  SDL_RenderCopy(game.renderer, game.UI, NULL,
      &(SDL_Rect){LEVEL_W, xpos, 125, 25});
  xpos += xgap;
  game.surface = TTF_RenderUTF8_Solid(game.font, scoreStr, game.white);
  game.UI = SDL_CreateTextureFromSurface(game.renderer, game.surface);
  SDL_RenderCopy(game.renderer, game.UI, NULL,
      &(SDL_Rect){LEVEL_W, xpos, 125, 25});

  scoreUi = "  Score  ";
  sprintf(scoreStr, "%d", game.player_score);

  xpos+= xgap;

  game.surface = TTF_RenderUTF8_Solid(game.font, scoreUi, game.yellow);
  game.UI = SDL_CreateTextureFromSurface(game.renderer, game.surface);
  SDL_RenderCopy(game.renderer, game.UI, NULL,
      &(SDL_Rect){LEVEL_W, xpos, 125, 25});
  xpos += xgap;
  game.surface = TTF_RenderUTF8_Solid(game.font, scoreStr, game.white);
  game.UI = SDL_CreateTextureFromSurface(game.renderer, game.surface);
  SDL_RenderCopy(game.renderer, game.UI, NULL,
      &(SDL_Rect){LEVEL_W, xpos, 125, 25});

  xpos += xgap;

  // render player lives and bombs
  char livesStr[255];
  char *playerUi = "Player ";
  strcpy(livesStr, "");

  if (player.hp > 4 && player.hp < 10) {
    char *x = "x";
    strcpy(livesStr, x);
  
    char buff[25];
    sprintf(&buff, "%d", player.hp);
    strcpy(livesStr, buff);
  } else {
    switch (player.hp) {
      case 4:
        strcpy(livesStr, "\x3D\x3D\x3D\x3D");
        break;
      case 3:
        strcpy(livesStr, "\x3D\x3D\x3D");
        break;
      case 2:
        strcpy(livesStr, "\x3D\x3D");
        break;
      case 1:
        strcpy(livesStr, "\x3D");
        break;
      case 0:
        break;
    }
  }

  game.surface = TTF_RenderUTF8_Solid(game.font, playerUi, game.yellow);
  game.UI = SDL_CreateTextureFromSurface(game.renderer, game.surface);
  SDL_RenderCopy(game.renderer, game.UI, NULL,
      &(SDL_Rect){LEVEL_W, xpos, 125, 25});

  game.surface = TTF_RenderUTF8_Solid(game.font, livesStr, game.white);
  game.UI = SDL_CreateTextureFromSurface(game.renderer, game.surface);
  SDL_RenderCopy(game.renderer, game.UI, NULL,
      &(SDL_Rect){LEVEL_W + 120, xpos, 100, 25});

  xpos += xgap;

  char bombStr[255];
  strcpy(bombStr, "");
  char *bombUi = "Bomb ";

  if (player.bomb_count > 4 && player.bomb_count < 10) {
    char *x = "x";
    strcpy(livesStr, x);
  
    char buff[25];
    sprintf(&buff, "%d", player.bomb_count);
    strcpy(livesStr, buff);
  } else {
    switch (player.bomb_count) {
      case 4:
        strcpy(bombStr, "\x2F\x2F\x2F\x2F");
        break;
      case 3:
        strcpy(bombStr, "\x2F\x2F\x2F");
        break;
      case 2:
        strcpy(bombStr, "\x2F\x2F");
        break;
      case 1:
        strcpy(bombStr, "\x2F");
        break;
      case 0:
        break;
    }
  }
  
  game.surface = TTF_RenderUTF8_Solid(game.font, bombUi, game.yellow);
  game.UI = SDL_CreateTextureFromSurface(game.renderer, game.surface);
  SDL_RenderCopy(game.renderer, game.UI, NULL,
      &(SDL_Rect){LEVEL_W, xpos, 125, 25});
  
  game.surface = TTF_RenderUTF8_Solid(game.font, bombStr, game.white);
  game.UI = SDL_CreateTextureFromSurface(game.renderer, game.surface);
  SDL_RenderCopy(game.renderer, game.UI, NULL,
      &(SDL_Rect){LEVEL_W + 120, xpos, 100, 25});

  /* objects & players*/
  //draw player
  SDL_SetTextureBlendMode(player.idle[(int) game.frame % IDLE_FRAMES],
      SDL_BLENDMODE_BLEND);
  SDL_SetTextureAlphaMod(player.idle[(int) game.frame % IDLE_FRAMES], player.alpha);

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
    SDL_RenderCopyEx(game.renderer, b->texture, NULL,
        &(SDL_Rect){b->pos.x - camera.x, b->pos.y - camera.y, b->pos.w, b->pos.h},
        (double)(game.frame % 360), NULL, SDL_FLIP_NONE);
  }

  /* draw items */
  for (int i = 0; i < sarray_size(&game.items); i++) {
    Entity *itm = sarray_get(&game.items, i);
    SDL_RenderCopy(game.renderer, itm->texture, NULL,
        &(SDL_Rect){itm->pos.x - camera.x, itm->pos.y - camera.y, itm->pos.w, itm->pos.h});
  }

  // SDL_FreeSurface(livesUISurface);
  // SDL_DestroyTexture(livesUI);

  SDL_RenderPresent(game.renderer);
}

void cleanup() {
  sarray_free(&game.enemies);
  sarray_free(&game.bullets);
  sarray_free(&game.items);
  Mix_FreeChunk(game.shotsfx);
  Mix_FreeChunk(game.enemy_hitsfx);
  Mix_FreeMusic(game.bgm);
  SDL_DestroyRenderer(game.renderer);
  SDL_DestroyWindow(game.window);
  SDL_Quit();
}
