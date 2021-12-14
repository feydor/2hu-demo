# 2hu-demo
An SDL2 game engine/demo based on Zun's Touhou Project.

### Usage:
```
git clone https://github.com/feydor/2hu-demo
cd 2hu-demo
mkdir build
meson build
ninja -C build
./build/2hu-demo
```

Run the unit tests:
```
meson test build
```
### Dependencies:
meson   
optional (will be downloaded by meson if not found):  
sdl2 sdl2_image sdl2_mixer sdl2_ttf

### Controls:
z: fire
x: bomb
arrow keys: movement

### TODO
- Bug: memory corruption when shooting an enemy while in range of its hitbox
- Complete the transient struct and its methods to add temporary onscreen fx, most importantly on enemy death.
- Limit player movement to the game window.
    - Refactor game window to use relative positioning rather than magic numbers.
- Add enemy-seeking bullets to player's shot progression.
- Create a level with predefined enemy paths, bullet patterns, and a boss. Will likely need to parse a script file.
    - Also under this, background progression.

### Screenshot
![screenshot](/res/screenshot2.png)
