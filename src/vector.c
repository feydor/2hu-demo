/* vector.c - a generic, dynamic array */
#include <stdlib.h> /* for malloc, realloc, free */

#define VECTOR_INIT_CAPACITY 10

#define VECTOR_INIT(vec) vector vec; vector_init(&vec)
#define VECTOR_SIZE(vec) vector_size(&vec)
#define VECTOR_PUSHBACK(vec, item) vector_pushback(&vec, (void *) item)
#define VECTOR_FREE(vec) vector_free(&vec)

typedef struct vector {
    void **items;
    int capacity;
    int size;
} vector;

void vector_init(vector *);
int vector_size(vector *);
void vector_pushback(vector *, void *);
void vector_set(vector *, int, void *);
void vector_delete(vector *, int);
void *vector_get(vector *, int);
void vector_free(vector *);

void vector_init(vector *v) 
{
    v->capacity = VECTOR_INIT_CAPACITY;
    v->size = 0;
    v->items = malloc(sizeof(void *) * v->capacity);
}

int vector_size(vector *v) 
{
    return v->size;
}

void vector_pushback(vector *v, void *item)
{ 
    /* resize if full */
    if (v->capacity == v->size) {
        int new_capacity = v->capacity * 2;
        void **items = realloc(v->items, sizeof(void *) * new_capacity);
        if (items) {
            v->items = items;
            v->capacity = new_capacity;
        }
    }

    v->items[v->size++] = item; // post-script, increments size last
}

void vector_set(vector *v, int index, void *item)
{
    if (index >= 0 && index < v->size)
        v->items[index] = item;
}

void vector_delete(vector *v, int index) 
{
    if (index < 0 || index >= v->size)
        return;

    v->items[index] = NULL;

    for (int i = index; i < v->size - 1; i++) {
        v->items[i] = v->items[i + 1];
        v->items[i + 1] = NULL;
    }

    v->size--;

    /* resize if 3/4 empty */
    if (v->size > 0 && v->size == v->capacity / 4) {
        int new_capacity = v->capacity / 2;
        void **items = realloc(v->items, sizeof(void *) * new_capacity);
        if (items) {
            v->items = items;
            v->capacity = new_capacity;
        }
    }
}

void *vector_get(vector *v, int index)
{
    if (index >= 0 && index < v->size)
        return v->items[index];
    return NULL;
}

void vector_free(vector *v) 
{
    free(v->items);
}