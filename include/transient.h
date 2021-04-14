#ifndef TRANSIENT_H
#define TRANSIENT_H

typedef enum {
  ENEMY_DEATH
} TransientType;

typedef struct {
  SDL_Rect*     pos;
  float         dx;
  float         dy;
  int           ttl;
  Uint32        born;
  Uint32        last_update;
  SDL_Texture*  frames;
  TransientType type;
} Transient;

/* function declarations */
Transient* init_transient(TransientType type, SDL_Texture* frames, int ttl);
void update_transient(Transient* t);

#endif
