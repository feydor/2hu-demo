/* safe_array.h */
#ifndef SAFE_ARRAY_H
#define SAFE_ARRAY_H

#include <stdlib.h> /* for malloc, realloc, free */
#include <stdbool.h> /* for bool, true, false */
#include <stdio.h> /* for printf */

#define SARRAY_INIT_CAPACITY 10

#define SARRAY_INIT(sa) SafeArray sa; sarray_init(&sa)
#define SARRAY_PUSHBACK(sa, item) sarray_pushback(&sa, (void *) item)
#define SARRAY_FOREACH(sa, cb) void (*callback)(void *, void *); callback = cb; sarray_foreach(&sa, callback);

/* data structure definitions */
typedef struct Queue {
  void **items;
  int size;
  int capacity;
} Queue;

typedef struct SafeArray {
  void  **items;
  Queue *add_queue;
  Queue *remove_queue;
  int   (*compare)(const void *, const void *); // comparison function
  int   size;
  int   capacity;
} SafeArray;

// generic function pointer
typedef void (*funcptr)(void);

/* function prototypes */
void sarray_init(SafeArray *, int (*compare)(const void *, const void *));
int sarray_size(SafeArray *);
bool sarray_isempty(SafeArray *);
void *sarray_get(SafeArray *, const int);
void *sarray_get_queued(SafeArray *, const int);
void sarray_pushback(SafeArray *, void *);
void sarray_delete(SafeArray *, void *);
void sarray_delete_index(SafeArray *, int);
void sarray_foreach(SafeArray *, void (*callback)(void *, void *, void *)); 
void _add_queued(SafeArray *);
void _remove_queued(SafeArray *);
void sarray_free(SafeArray *);

bool queue_has_item(Queue *, void *);
void print_item(void *);

/* common compare functions 
 * if a < b, result is -1,
 * if a == b, result is 0,
 * if a > b, result is 1
 */
int compare_int(const void *, const void *);

#endif
