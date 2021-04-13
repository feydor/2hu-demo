#include "../include/common.h"
#include "../include/entity.h"
#include "../include/game.h"

/* global declarations */
Game game;
SDL_Event event;
SDL_Texture *bullet_texture = NULL;
SDL_Texture *enemy_bullet_texture = NULL;
SDL_Texture *enemy_idle[ENEMY_IDLE_FRAMES];
SDL_Texture *right[9];
SDL_Texture *left[9];
TTF_Font *g_font = NULL;
Entity player;
Camera camera;

/* utility functions */
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
