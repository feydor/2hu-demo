#ifndef TRANSIENT_H
#define TRANSIENT_H

#include <SDL2/SDL.h>

#define ENM_DEATH_FRAMES 8

/* data structures */
typedef enum {
	TRANS_ENM_DEATH,
} TransientType;

typedef struct {
	SDL_Rect pos;
	float dx, dy;
	TransientType type;
	int ttl;
	Uint32 born, last_update;
	SDL_Texture *texture;
	SDL_Texture *enm_death[ENM_DEATH_FRAMES];
	int alpha;
	float (*motion_eq)(float, int, int);
	Uint32 fire_time;
} Transient;

/* function prototypes */

#endif
