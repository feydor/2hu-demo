#include "spritesheet_manager.h"
#include "../util/util.h"

#define ARRAY_LEN(arr) (sizeof(arr) / sizeof((arr)[0]))

/** Global sprite sheet map. Use hash(key) to find the index associated with a key. */
SDL_Surface *g_spritesheet_surfaces[MAX_SPRITESHEETS];

static SDL_Surface *twohu_ssm_hashmap_get_or_new(const char *path);
static void change_curr_anim(TwohuSpritesheetManager *self, int anim);
static void set_frame(TwohuSpritesheetManager *self, int nframe);
static int total_frames(TwohuSpritesheetManager *sm);
static int hash(const char *str);
static int ascii_value(const char *str);
/** Render the current frame of animation at dest */
static void render_curr_animation_frame(TwohuSpritesheetManager *self, FloatRect *dest,
                                        SDL_Renderer *renderer);

TwohuSpritesheetManager twohu_ssm_create(const char *path, uint ncols, uint nrows) {
    SDL_Surface *image = twohu_ssm_hashmap_get_or_new(path);
    int sprite_w = image->w / ncols;
    int sprite_h = image->h / nrows;
    TwohuSpritesheetManager self = {
        .n_anims=nrows, .sprite_w=sprite_w, .sprite_h=sprite_h,
        .image=image, .curr_anim=0, .curr_frame=0, .clip=(SDL_Rect){0},
        .render_and_update=twohu_ssm_render_and_update
    };
    change_curr_anim(&self, 0);
    return self;
}

static SDL_Surface *twohu_ssm_hashmap_get_or_new(const char *path) {
    int idx = hash(path);
    if (g_spritesheet_surfaces[idx]) {
        return g_spritesheet_surfaces[idx];
    }

    printf("Loading and saving new sprite sheet surface with path=%s in hashmap idx=%d...\n",
           path, idx);
    g_spritesheet_surfaces[idx] = load_surface_or_exit(path);
    return g_spritesheet_surfaces[idx];
}

void twohu_ssm_render_and_update(TwohuSpritesheetManager *self, SDL_Renderer *renderer, FloatRect *dest) {
    render_curr_animation_frame(self, dest, renderer);
    set_frame(self, self->curr_frame+1);
}

bool twohu_ssm_is_current_anim(TwohuSpritesheetManager *self, int anim) {
    return self->curr_anim == anim;
}

void twohu_ssm_change_current_anim(TwohuSpritesheetManager *self, int anim) {
    return change_curr_anim(self, anim);
}

static void render_curr_animation_frame(TwohuSpritesheetManager *self, FloatRect *dest, SDL_Renderer *renderer) {
    if (!self->texture) {
        self->texture = load_texture_or_exit(renderer, self->image);
    }

    SDL_Rect rect = floatrect_to_sdlrect(dest);
    SDL_RenderCopy(renderer, self->texture, &self->clip, &rect);
}

    /**
 * Change the animation to be rendered this tick
 * @param anim should be in range [0, n_anims-1]
 */
static void change_curr_anim(TwohuSpritesheetManager *self, int anim) {
    if (anim >= self->n_anims) {
        exit(fprintf(stderr, "twohu_ssm: Out of bounds: curr anim: %d is greater than %d!\n",
                     anim, self->n_anims));
    }

    self->curr_anim = anim;
    set_frame(self, 0);
}

/**
 * Sets the current frame and updates the clip. If nframe is out of bounds, sets to first frame (frame 0).
 * @param self
 * @param nframe the frame to change to
 */
static void set_frame(TwohuSpritesheetManager *self, int nframe) {
    self->curr_frame = nframe;
    if (self->curr_frame >= total_frames(self)) {
        self->curr_frame = 0;
    }

    self->clip = (SDL_Rect){
        .x= self->sprite_w * self->curr_frame,
        .y = self->sprite_h * self->curr_anim,
        .w = self->sprite_w,
        .h = self->sprite_h - 1 // in order to remove extra pixels at the sprite's bottom
    };
}

/** The number of frames in an animation */
static inline int total_frames(TwohuSpritesheetManager *self) {
    return self->image->w / self->sprite_w;
}

/** Returns the index of the map associated to given null-terminated key */
static inline int hash(const char *key) {
    return ascii_value(key) % ARRAY_LEN(g_spritesheet_surfaces);
}

/** Returns the sum of the ascii values of the null-terminated string */
static inline int ascii_value(const char *str) {
    int sum = 0;
    while (*str != '\0') {
        sum += (int)(*str);
        ++str;
    }
    return sum;
}
