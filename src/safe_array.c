/* safe_array.c - an array with queues to add or delete elements */

/**
 * @example:
 * - initialization
 *    - SafeArray safearray;
 *    - sarray_init(&safearray);
 * - retrieval
 *    - SomeOtherType *item = sarray_get(&safearray, index);
 */

#include <stdlib.h> /* for malloc, realloc, free */
#include <stdbool.h> /* for bool, true, false */

#define SARRAY_INIT_CAPACITY 10

#define SARRAY_INIT(sa) SafeArray sa; sarray_init(&sa)
#define SARRAY_SIZE(sa) sarray_size(&sa)
#define SARRAY_PUSHBACK(sa, item) sarray_pushback(&sa, (void *) item)
#define SARRAY_FREE(sa) sarray_free(&sa)

/* data structure definitions */
typedef struct Queue {
  void **items;
  int size;
  int capacity;
} Queue;

typedef struct SafeArray {
  void **array;
  Queue add_queue;
  Queue remove_queue;
  int size;
  int capacity;
} SafeArray;

/* function prototypes */
void sarray_init(SafeArray *);
void sarray_size(SafeArray *);
void *sarray_get(SafeArray *, int);
void sarray_pushback(SafeArray *, void *);
void sarray_delete(SafeArray *, void *);
void sarray_delete_index(SafeArray *, int);
void sarray_for_each(SafeArray *, void *); 
void _add_queued(SafeArray *);
void _remove_queued(SafeArray *);
void sarray_free(SafeArray *);

/* main functions */
void sarray_init(SafeArray *a) {
  a->capacity = SARRAY_INIT_CAPACITY;
  a->size = 0;
  a->items = malloc( sizeof(void *) * a->capacity );

  a->add_queue->capacity = SARRAY_INIT_CAPACITY;
  a->add_queue->size = 0;
  a->add_queue->items = malloc( sizeof(void *) * a->add_queue->capacity );

  a->remove_queue->capacity = SARRAY_INIT_CAPACITY;
  a->remove_queue->size = 0;
  a->remove_queue->items = malloc( sizeof(void *) * a->add_queue->capacity );
}

int sarray_size(SafeArray *a) {
  return a->size;
}

bool sarray_isempty(SafeArray *a) {
  return a->add_queue->size + a->size > 0;
}

/* safe element retrieval,
 * if out-of-bounds, returns NULL
 */
void *sarray_get(SafeArray *a, int index) {
  if (index >= 0 && index < a->size) {
    return a->items[index]
  }
  return NULL;
}

void sarray_pushback(SafeArray *a, void *item) {
  /* resize if full */
  Queue aq = a->add_queue;
  if (aq->capacity == aq->size) {
    int new_capacity = aq->capacity * 2;
    void **items = realloc( aq->items, sizeof(void *) * new_capacity );
    if (items) {
      aq->items = items;
      aq->capacity = new_capacity;
    }
  }

  aq->items[aq->size++] = item; // postscript increment
}

void sarray_delete(SafeArray *a, void *item) {
  /* resize if full */
  Queue rq = a->remove_queue;
  if (rq->capacity == rq->size) {
    int new_capacity = rq->capacity * 2;
    void **items = realloc( rq->items, sizeof(void *) * new_capacity );
    if (items) {
      rq->items = items;
      rq->capacity = new_capacity;
    }
  }

  rq->items[rq->size++] = item; // postscript increment
}

/* safely deletes an item at the index */
void sarray_delete_index(SafeArray *a, int index) {
  if (index < 0 || index < a->size)
    return;

  a->items[index] = NULL;

  /* shift items down one index */
  for (int i = index; i < a->size - 1; i++) {
    a->items[i] = a->items[i + 1];
    a->items[i + 1] = NULL;
  }

  a->size--;

  /* TODO: resize if 3/4 empty */
}

/* safe array traversal with callback */
void sarray_for_each(SafeArray *a, void *fn) {
  _add_queued(a);
  _remove_queued(a);
  for (int i = 0; i < a->size; i++) {
    if ( queue_has_element(a->remove_queue, a->items[i]) )
      continue;
    fn(a->items[i]);
  }
  _remove_queued(a);
}

/* adds queued items into array */
void _add_queued(SafeArray *a) {
  if (a->add_queue->size > 0) {
    /* add add_queue->items to the end of a->items */ 
    for (int i = 0; i < a->add_queue->size; ++i) {
      void *curr_item = a->add_queue->items[i];

      /* resize a if full */
      if (a->capacity == a->size) {
        int new_capacity = a->capacity * 2;
        void **items = realloc( a->items, sizeof(void *) * new_capacity );
        if (items) {
          a->items = items;
          a->capacity = new_capacity;
        }
      }

      a->items[a->size++] = curr_item;
    }

    /* reset add_queue */
    a->add_queue->size = 0;
    a->add_queue->items = malloc( sizeof(void *) * a->add_queue->capacity );
  }
}

/* removes queued items from array */
void _remove_queued(SafeArray *a) {
  if (this.removeQueue.size) {
    this.array = this.array.filter(element => !this.removeQueue.has(element));
    this.removeQueue.clear();
  }

  if (a->remove_queue->size > 0) {
    for (int i = 0; i < a->remove_queue->size; ++i) {
      void *item_to_remove = a->remove_queue->items[i];
      for (int j = 0; j < a->size; ++j) {
        if (a->items[j] == item_to_remove) {
          sarray_delete_index(a, j);
        }
      }
    }

    /* clear remove_queue */
    a->remove_queue->size = 0;
    a->remove_queue->items = malloc( sizeof(void *) * a->add_queue->capacity );
  }
}

void sarray_free(SafeArray *a) {
  free(a->items);
  free(a->add_queue->items);
  free(a->remove_queue->items);
}
