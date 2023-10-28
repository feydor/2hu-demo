#include "entity-factory.h"
#include "../util/rect.h"
#include "../constants.h"

static TwohuEntity G_ENTITY_FACTORY[FACTORY_CAPACITY] = {0};
static int factory_top = 0;

TwohuEntity *twohu_entity_factory_new(FloatRect rect, SDL_Point hitbox, bool player) {
    if (factory_top >= FACTORY_CAPACITY) {
        // TODO: remalloc the pool
        exit(fprintf(stderr, "Entity Factory reached FACTORY_CAPACITY=%d\n", FACTORY_CAPACITY));
    }

    TwohuEntity ent = {
        .player=player,
        .hitbox=hitbox,
        .dx=0,
        .dy=0,
        .speed=DEFAULT_ENTITY_SPEED,
        .bullet_manager=twohu_bulletmanager_create(),
        .alive=true,
        .surface=SDL_CreateRGBSurface(0, rect.w, rect.h, 32, 0xFF000000, 0xFF0000, 0xFF00, 0xFF),
        .sheet_manager=create_twohu_spritesheet_manager() // TODO:
    };
}
