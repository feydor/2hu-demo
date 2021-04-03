/* safe_array.c - an array with queues to add or delete elements */

/**
 * @example:
 * - initialization
 *    - SafeArray safearray;
 *    - sarray_init(&safearray);
 * - retrieval
 *    - SomeOtherType *item = sarray_get(&safearray, index);
 */
#include "safe_array.h"

/* main functions */
int main() {
  printf("SafeArray initialization...\n");
  SafeArray sa;
  sarray_init(&sa);
  
  int n1 = 777;
  int n2 = 101010;
  void *p_n1 = &n1;
  void *p_n2 = &n2;
  sarray_pushback(&sa, p_n1);
  sarray_pushback(&sa, p_n2);

  void (*print_callback)(void *);
  print_callback = print_item;
  
  /* testing foreach callback */
  sarray_foreach(&sa, print_callback);

  return 0;
}

void print_item(void *item) {
  printf( "Item: %i\n", *((int *) item) );
}

void sarray_init(SafeArray *a) {
  a->capacity = SARRAY_INIT_CAPACITY;
  a->size = 0;
  a->items = malloc( sizeof(void *) * a->capacity );
  a->add_queue = malloc( sizeof(Queue *) );
  a->remove_queue = malloc( sizeof(Queue *) );

  a->add_queue->capacity = SARRAY_INIT_CAPACITY;
  a->add_queue->size = 0;
  a->add_queue->items = malloc( sizeof(void *) * a->add_queue->capacity );

  a->remove_queue->capacity = SARRAY_INIT_CAPACITY;
  a->remove_queue->size = 0;
  a->remove_queue->items = malloc( sizeof(void *) * a->remove_queue->capacity );
}

int sarray_size(const SafeArray *a) {
  return a->size;
}

bool sarray_isempty(const SafeArray *a) {
  return a->add_queue->size + a->size > 0;
}

/* safe element retrieval,
 * if out-of-bounds, returns NULL
 */
void *sarray_get(const SafeArray *a, const int index) {
  if (index >= 0 && index < a->size) {
    return a->items[index];
  }
  return NULL;
}

void sarray_pushback(SafeArray *a, void *item) {
  /* resize if full */
  Queue *aq = a->add_queue;
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
  Queue *rq = a->remove_queue;
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
void sarray_foreach(SafeArray *a, void (*callback)(void *)) {
  _add_queued(a);
  _remove_queued(a);
  for (int i = 0; i < a->size; i++) {
    if ( queue_has_item(a->remove_queue, a->items[i]) )
      continue;
    callback(a->items[i]);
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

bool queue_has_item(Queue *q, void *item) {
  for (int i = 0; i < q->size; i++) {
    if (q->items[i] == item) {
      return true;
    }
  }
  return false;
}

void sarray_free(SafeArray *a) {
  free(a->items);
  free(a->add_queue->items);
  free(a->remove_queue->items);
  free(a->add_queue);
  free(a->remove_queue);
}
