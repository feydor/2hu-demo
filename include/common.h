#ifndef COMMON_H_
#define COMMON_H_

/* This is a header included by every single file.
 It should include definitions of most basic types and utilities
 (like asserts and simple templates. It usually means most common
 header files for C standard library like <stdio.h> etc. and
 <windows.h> on Windows.
 Resist temptation to include too much in this file - it’ll lead
 to insanity.
*/
#include <stdlib.h> /* for malloc, realloc, free, rand, srand, AND strtol */
#include <stdio.h> 	/* for printf */
#include <time.h> 	/* for time */
#define SDL_DISABLE_IMMINTRIN_H
#include <SDL.h>
#include <SDL_mixer.h>
#include <SDL_image.h>

/* definitions */
#define IDLE_FRAMES 9			      /* number of idle frames */
#define MAX(X, Y) (((X) > (Y)) ? (X) : (Y))
#define MIN(X, Y) (((X) < (Y)) ? (X) : (Y)) 
#define UNUSED(X) (void) X;

/* forward declarations */
#include "entity.h"

/* function prototVypes */
int compare_entities(const void *, const void *);

#endif