cmake_minimum_required(VERSION 3.16)
project(2hu_demo C)

set(CMAKE_C_STANDARD 11)

include(FindPkgConfig)
pkg_search_module(SDL2 REQUIRED sdl2)
pkg_search_module(SDL2IMAGE REQUIRED SDL2_image>=2.0.0)
include_directories(${SDL2_INCLUDE_DIRS} ${SDL2IMAGE_INCLUDE_DIRS})

add_executable(${PROJECT_NAME}
        src/bullet.c
        src/bullet.h
        src/constants.h
        src/entity.c
        src/entity.h
        src/game.c
        src/input.c
        src/input.h
        src/twohu.c
        src/twohu.h)

target_link_libraries(${PROJECT_NAME} ${SDL2_LIBRARIES} ${SDL2IMAGE_LIBRARIES})

include_directories(src)
