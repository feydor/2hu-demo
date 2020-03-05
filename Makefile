#Makefile
CC = gcc
CFLAGS = -g -Wall -Wextra $(pkg-config --cflags --libs sdl2)
VPATH = ./src/ ./include/ ./obj/
# Path for .c , .h and .o Files 
SRC_PATH := ./src/
OBJ_PATH := ./obj/
INC_PATH := -I ./include/
# Executable Name 
TARGET = bin/2hu
# Files to compile
OBJ1 := main.o
OBJ := $(patsubst %,$(OBJ_PATH)%,$(OBJ1))

# Build .o first
$(OBJ_PATH)%.o: $(SRC_PATH)%.c 
	@echo [CC] $<
	@$(CC) $(C_FLAGS) -o $@ -c $< $(INC_PATH)    
# Build final Binary
# $@ - what is on the left side of :
# &^ - what is on the right side of :
$(TARGET): $(OBJ)
	@echo [INFO] Creating Binary Executable [$(TARGET)]
	@$(CC) -o $@ $^ 
# Clean all the object files and the binary
clean:   
	@echo "[Cleaning]"
	@$(RM) -rfv $(OBJ_PATH)*
	@$(RM) -rfv $(TARGET)
