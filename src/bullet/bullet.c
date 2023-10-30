#include "bullet.h"
#include "../constants.h"
#include "../util/util.h"

/** The global circular buffer */
TwohuBullet g_bullets[MAX_BULLETS] = {0};
int g_head = 0;
int g_tail = 0;
int g_capacity = MAX_BULLETS;

/** global bullet surface */
SDL_Surface *g_bullet_surface;
SDL_Texture *g_bullet_texture;

typedef enum {
    CONTINUE, // normal return, continue loop
    TRUE, // returned true early, stop loop
    FALSE // returned false early, stop loop
} FOREACH_RET_CODE;

static FOREACH_RET_CODE update_bullet(int index, void *dt_cast);
static FOREACH_RET_CODE render_bullet(int index, void *renderer_cast);
static FOREACH_RET_CODE is_colliding(int bullet_index, void *entity_cast);
static FOREACH_RET_CODE circular_buffer_foreach(FOREACH_RET_CODE (*foreach_fn)(int index, void *_param1),
                                                void *param1);
static inline int dist(int t, int h, int capacity);
static inline void increment_circular(int *x, int capacity);

TwohuBullet *twohu_bullet_spawn(SDL_Point loc, float dx, float dy) {
    if (!g_bullet_surface) {
        g_bullet_surface = load_surface_or_exit(BULLET_PNG);
    }

    TwohuBullet bullet = {
        .rect=(FloatRect){loc.x - (BULLET_W/2), loc.y - (BULLET_H/2), BULLET_W, BULLET_H},
        .hitbox=(SDL_Point){BULLET_W, BULLET_H},
        .surface=g_bullet_surface,
        .dx=dx,
        .dy=dy,
        .speed=BULLET_INIT_SPEED,
        .alive=true
    };

    g_bullets[g_tail] = bullet;
    TwohuBullet *ref = &g_bullets[g_tail];

    if (dist(g_tail, g_head, g_capacity) == g_capacity-1) {
        // circular buffer is full
        // move each ptr forward
        increment_circular(&g_head, g_capacity);
        increment_circular(&g_tail, g_capacity);
    } else {
        // move only tail
        increment_circular(&g_tail, g_capacity);
    }

    return ref;
}

bool twohu_bulletmanager_is_colliding(TwohuEntity *entity) {
    FOREACH_RET_CODE r = circular_buffer_foreach(is_colliding, (void *)entity);
    return r == TRUE;
}

void twohu_bulletmanager_update(float *dt) {
    circular_buffer_foreach(update_bullet, (void *)dt);
}

void twohu_bulletmanager_render(SDL_Renderer *renderer) {
    circular_buffer_foreach(render_bullet, (void *)renderer);
}

static FOREACH_RET_CODE render_bullet(int index, void *renderer_cast) {
    TwohuBullet *bullet = &g_bullets[index];
    if (!bullet->alive) return CONTINUE;

    SDL_Renderer *renderer = (SDL_Renderer *)renderer_cast;

    if (!g_bullet_texture) {
        g_bullet_texture = load_texture_or_exit(renderer, bullet->surface);
    }

    SDL_Rect render_rect = {.x=0, .y=0, .w=bullet->rect.w, .h=bullet->rect.h};
    SDL_Rect dest = floatrect_to_sdlrect(&bullet->rect);
    SDL_RenderCopy(renderer, g_bullet_texture, &render_rect, &dest);
    return CONTINUE;
}

static FOREACH_RET_CODE update_bullet(int index, void *dt_cast) {
    TwohuBullet *b = &g_bullets[index];
    if (!b->alive) return CONTINUE;

    float *dt_ptr = (float *)dt_cast;
    float dt = *dt_ptr;

    float dx = b->dx * dt;
    float dy = b->dy * dt;
    float x_next = b->rect.x + dx;
    float y_next = b->rect.y + dy;

    // bounds checking
    if (y_next < 0 || y_next > H_WINDOW - b->rect.w ||
        x_next < 0 || x_next > W_WINDOW - b->rect.w) {
        b->alive = false;
        return CONTINUE;
    }

    b->rect.x = x_next;
    b->rect.y = y_next;
    return CONTINUE;
}

static FOREACH_RET_CODE is_colliding(int bullet_index, void *entity_cast) {
    TwohuBullet *bullet = &g_bullets[bullet_index];
    if (!bullet->alive) return CONTINUE;

    TwohuEntity *entity = (TwohuEntity *)entity_cast;
    if (floatrect_are_colliding(&bullet->rect, &entity->rect)) {
        return TRUE;
    }
    return CONTINUE;
}

/**
 * For each element in the buffer, calls foreach_fn.
 * @param foreach_fn Takes the index of the circular buffer and param1 to be cast to real pointer type
 * @param param1 The generic parameter to pass to foreach_fn
 */
static FOREACH_RET_CODE circular_buffer_foreach(FOREACH_RET_CODE (*foreach_fn)(int index, void *_param1),
                                                void *param1) {
    // from head to tail or end
    int cur = g_head;
    while (cur < g_capacity && cur != g_tail) {
        FOREACH_RET_CODE r = foreach_fn(cur, param1);
        if (r != CONTINUE) return r;
        cur++;
    }

    // if at end, then loop around and reach tail
    if (cur == g_capacity) {
        cur = 0;
        while (cur < g_tail) {
            FOREACH_RET_CODE r = foreach_fn(cur, param1);
            if (r != CONTINUE) return r;
            cur++;
        }
    }

    return CONTINUE;
}

/** Returns the hops between t and h in the circular buffer */
static inline int dist(int t, int h, int capacity) {
    if (t > h) {
        return t-h;
    } else if (t == h) {
        return 0;
    } else {
        return capacity-1;
    }
}

/** Increments the index x accounting for overflow */
static inline void increment_circular(int *x, int capacity) {
    if (*x == capacity-1) {
        *x = 0;
    } else {
        *x = *x + 1;
    }
}
