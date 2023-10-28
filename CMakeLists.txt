cmake_minimum_required(VERSION 3.16)
project(2hu_demo C)

set(CMAKE_C_STANDARD 11)

include(FindPkgConfig)
pkg_search_module(SDL2 REQUIRED sdl2)
pkg_search_module(SDL2IMAGE REQUIRED SDL2_image>=2.0.0)
include_directories(${SDL2_INCLUDE_DIRS} ${SDL2IMAGE_INCLUDE_DIRS})

if ( CMAKE_COMPILER_IS_GNUCC )
    set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} -Wall -Wextra")
endif()
if ( MSVC )
    set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} /W4")
endif()

add_executable(${PROJECT_NAME}
        src/bullet/bullet.c
        src/bullet/bullet.h
        src/constants.h
        src/entity/entity.c
        src/entity/entity.h
        src/game.c
        src/input/input.c
        src/input/input.h
        src/twohu/twohu.c
        src/twohu/twohu.h
        src/enemy/enemy_manager.h
        src/enemy/enemy_manager.c
        src/util/rect.c
        src/util/rect.h
#        src/entity/entity-factory.c
#        src/entity/entity-factory.h
        src/util/util.c
        src/util/util.h)

target_link_libraries(${PROJECT_NAME} ${SDL2_LIBRARIES} ${SDL2IMAGE_LIBRARIES})

include_directories(src)
