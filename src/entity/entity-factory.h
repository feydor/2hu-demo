#ifndef INC_2HU_DEMO_ENTITY_FACTORY_H
#define INC_2HU_DEMO_ENTITY_FACTORY_H
#include "entity.h"

#define FACTORY_CAPACITY 6666

enum TwohuEntityType {
    PLAYER,
    DEFAULT_ENEMY,
    ENTITY_TYPE_COUNT
};

TwohuEntity *twohu_ef_create(TwohuEntityType type, TwohuSpritesheetManager *sm, FloatRect dest, SDL_Point hitbox);

#endif // INC_2HU_DEMO_ENTITY_FACTORY_H
