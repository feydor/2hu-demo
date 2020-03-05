#ifndef VECTOR_H
#define VECTOR_H

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
void vector_free(vector *);

#endif