#include "../include/safe_array.h"
#include <unistd.h> /* for EXIT_FAILURE, EXIT_SUCCESS */
#include <assert.h> /* for assert */
#include <string.h> /* for strcmp */

#define UNUSED(x) (void)(x)

/* function prototypes */
int init_works(SafeArray *);
int isempty_works(SafeArray *);
int pushback_works(SafeArray *);
int foreach_works(SafeArray *);
int add_queue_works(SafeArray *);
int remove_queue_works(SafeArray *);
int add_remove_queues_work(SafeArray *);
  
/* test structure */
typedef struct {
  int n1;
  float f1;
  char *str;
  bool isalive;
} Data;

int counter = 0;

/* entry point for test suite */
int main(int argc, char *argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }

  SafeArray sa;
  sarray_init(&sa, compare_int);
  SafeArray *a = &sa;

  char *opt = argv[1];

  if (strcmp(opt, "test_init") == 0) {
    return init_works(a); 
  } else if (strcmp(opt, "test_isempty") == 0) {
    return isempty_works(a);
  } else if (strcmp(opt, "test_pushback") == 0) {
    return pushback_works(a);
  } else if (strcmp(opt, "test_foreach") == 0) {
    return foreach_works(a);
  } else if (strcmp(opt, "test_queues") == 0) {
    return add_remove_queues_work(a);
  } else if (strcmp(opt, "test_addqueue") == 0) {
    return add_queue_works(a);
  } else if (strcmp(opt, "test_removequeue") == 0) {
    return remove_queue_works(a);
  } else {
    return EXIT_FAILURE;
  }
}

int init_works(SafeArray *sa) {
  assert(sa->capacity == SARRAY_INIT_CAPACITY);
  assert(sa->size == 0);
  assert(sa->add_queue->capacity == SARRAY_INIT_CAPACITY);
  assert(sa->add_queue->size == 0);
  assert(sa->remove_queue->capacity == SARRAY_INIT_CAPACITY);
  assert(sa->remove_queue->size == 0);
  return EXIT_SUCCESS;
}

int isempty_works(SafeArray *sa) {
  assert(sarray_isempty(sa) == true);
  return EXIT_SUCCESS;
}

int pushback_works(SafeArray *sa) {
  char x1 = 'Z';
  char x2 = 'E';
  char x3 = 'O';
  sarray_pushback(sa, (void *) &x1);
  sarray_pushback(sa, (void *) &x2);
  sarray_pushback(sa, (void *) &x3);

  assert(*((char *) sarray_get_queued(sa, 0)) == 'Z');
  assert(*((char *) sarray_get_queued(sa, 1)) == 'E');
  assert(*((char *) sarray_get_queued(sa, 2)) == 'O');

  return EXIT_SUCCESS;
}

/* increment each element */
void increment(void *arr, void *item, void *opt) {
  UNUSED(arr);
  UNUSED(opt);
  *((int *) item) += 1;
}

int foreach_works(SafeArray *sa) {
  int n1 = 1;
  int n2 = 2;
  int n3 = 3;
  sarray_pushback(sa, (void *) &n1);
  sarray_pushback(sa, (void *) &n2);
  sarray_pushback(sa, (void *) &n3);
  
  void (*test_callback)(void *, void *, void *);
  test_callback = increment;

  sarray_foreach(sa, test_callback);

  n1 = *((int *) sarray_get(sa, 0));
  assert(n1 == 2);

  n2 = *((int *) sarray_get(sa, 1));
  assert(n2 == 3);

  n3 = *((int *) sarray_get(sa, 2));
  assert(n3 == 4);

  return EXIT_SUCCESS;
}

int add_queue_works(SafeArray *sa) {
  int n1 = 77;
  int n2 = 88;
  sarray_pushback(sa, (void *) &n1);
  sarray_pushback(sa, (void *) &n2);

  // check for presence in add_queue
  assert(*((int *) sarray_get_queued(sa, 0)) == n1);
  assert(*((int *) sarray_get_queued(sa, 1)) == n2);

  _add_queued(sa);

  // check for presence in safe array
  assert(*((int *) sarray_get(sa, 0)) == n1);
  assert(*((int *) sarray_get(sa, 1)) == n2);
  
  // check for NON-presence in add_queue
  assert(sarray_get_queued(sa, 0) == NULL);
  assert(sarray_get_queued(sa, 1) == NULL);

  return EXIT_SUCCESS;
}

int remove_queue_works(SafeArray *sa) {
  int n1 = 77;
  int n2 = 88;
  sarray_pushback(sa, (void *) &n1);
  sarray_pushback(sa, (void *) &n2);

  _add_queued(sa);
  
  // check for presence in safe array
  assert(*((int *) sarray_get(sa, 0)) == n1);
  assert(*((int *) sarray_get(sa, 1)) == n2);

  sarray_delete(sa, (void *) &n1);
  sarray_delete(sa, (void *) &n2);

  // check for presence in remove_queue
  assert(*((int *) sa->remove_queue->items[0]) == n1);
  assert(*((int *) sa->remove_queue->items[1]) == n2);

  assert(sa->remove_queue->size == 2);
  _remove_queued(sa);
  assert(sa->remove_queue->size == 0);
  
  // check for NON-presence in safe array
  assert(sa->size == 0);
  assert(sarray_get(sa, 0) == NULL);
  assert(sarray_get(sa, 1) == NULL);

  return EXIT_SUCCESS;
}

/* remove dead data */
void validate_data(void *arr, void *item, void *opt) {
  Data *data = (Data *) item;
  UNUSED(opt);
  if (!data->isalive) {
    sarray_delete((SafeArray *) arr, item);
  }
  // do other data processing
}

void count_data(void *arr, void *item, void *opt) {
  UNUSED(arr);
  UNUSED(item);
  UNUSED(opt);
  counter += 1;  
}

int compare_data(const void *a, const void *b) {
  Data *d1 = (Data *) a;
  Data *d2 = (Data *) b;
  return compare_int( (void *) &d1->n1, (void *) &d2->n1 );
}

int add_remove_queues_work(SafeArray *sa) {
  // change compare function for custom Data struct
  sa->compare = compare_data;

  // mock data struct
  Data d1;
  d1.n1 = 100;
  d1.f1 = 1.20;
  d1.str = "this is some data.";
  d1.isalive = true;
  Data d2;
  d2.n1 = 200;
  d2.f1 = 2.12;
  d2.str = "d2";
  d2.isalive = false;

  if (!d1.n1) exit(2);

  sarray_pushback(sa, (void *) &d1);
  sarray_pushback(sa, (void *) &d2);

  void (*callback)(void *, void *, void *);
  callback = validate_data;
  sarray_foreach(sa, callback);

  /* d2 should have been removed from the array and from the remove_queue */
  assert(((Data *) sarray_get(sa, 0))->n1 == d1.n1);
  assert(sarray_get(sa, 1) == NULL);

  // there should only be 1 item in the safe array
  callback = count_data;
  sarray_foreach(sa, callback);
  assert(counter == 1);

  return EXIT_SUCCESS;
}

