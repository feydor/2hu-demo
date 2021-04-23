# 2hu-demo
SDL2 game demo based on Touhou Project

### Usage:
```
git clone https://github.com/feydor/2hu-demo
cd 2hu-demo
mkdir build && cd build
meson ..
ninja
./2hu-demo
```

Run the unit tests:
```
cd build
meson test
```
### Dependencies:
meson 
optional (will be downloaded by meson if not found):sdl2 sdl2_image sdl2_mixer sdl2_ttf

### Controls:
z: fire
z: bomb
arrow keys: movement

### Screenshot

![screenshot](/res/screenshot1.png)
