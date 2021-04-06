#include "../include/common.h"
#include "../include/entity.h"

/* comparison functions */
int compare_entities(const void *a, const void *b) {
  // use entity->born parameter for comparison
  Entity *e1 = (Entity *) a;
  Entity *e2 = (Entity *) b;
  return (e1->born > e2->born) - (e1->born < e2->born);
}

void print_entity(Entity *entity) {
    printf("(%i, %i)\n", entity->pos.x, entity->pos.y);
}