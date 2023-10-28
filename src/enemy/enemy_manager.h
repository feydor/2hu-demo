#ifndef INC_2HU_DEMO_ENEMY_MANAGER_H
#define INC_2HU_DEMO_ENEMY_MANAGER_H
#include "../entity/entity.h"
#include <SDL2/SDL.h>

typedef struct TwohuEnemyManager {
    int count;
    float ticks;
} TwohuEnemyManager;

#define MAX_ENEMIES 333
#define DEFAULT_ENEMY_PNG "./res/enemy-default.png"
#define DEFAULT_ENEMY_SPRITE_W 60
#define DEFAULT_ENEMY_SPRITE_H 52
#define DEFAULT_ENEMY_SPEED 0.5

TwohuEnemyManager create_twohu_enemymanager();
void twohu_enemymanager_update(TwohuEnemyManager *self, float dt);
void twohu_enemymanager_render(TwohuEnemyManager *self, SDL_Renderer *renderer);

#endif // INC_2HU_DEMO_ENEMY_MANAGER_H
