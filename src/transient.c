#include "../include/common.h"
#include "../include/transient.h"

Transient* 
init_transient(TransientType type, SDL_Texture *frames, int ttl) {
  Transient* tr = malloc( sizeof(Transient) );
  memset( tr, 0, sizeof(Transient) );
  tr->type = type;
  tr->frames = frames;
  tr->ttl = ttl;
  return tr;
}

void
update_transient(Transient* tr) {
  if (tr->ttl > 0)
    tr->ttl -= 1;

  // TODO: Finish update function
}
