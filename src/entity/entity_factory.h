#ifndef INC_2HU_DEMO_ENTITY_FACTORY_H
#define INC_2HU_DEMO_ENTITY_FACTORY_H
#include "entity-type.h"
#include "entity.h"
#include <SDL2/SDL.h>

#define FACTORY_CAPACITY 666

TwohuEntity *twohu_ef_create(TwohuEntityType type, FloatRect dest, SDL_Point hitbox);

/** Update all the entities in the factory */
void twohu_ef_update_all(TwohuEntity *player, float dt);

void twohu_ef_render_all(SDL_Renderer *renderer);

#endif // INC_2HU_DEMO_ENTITY_FACTORY_H
