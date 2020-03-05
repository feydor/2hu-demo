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
    if (v->capacity == v->size) {
        int capacity = v->capacity * 2;
        void **items = realloc(v->items, sizeof(void *) * capacity);
        if (items) {
            v->items = items;
            v->capacity = capacity;
        }
    }

    v->items[v->size++] = item; // post-script, increments size last
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