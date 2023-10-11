# Makefile

FC = ifx
SRC_DIR = src
OBJ_DIR = build
MOD_DIR = include
LIBRARY = librandom.a
# List of source files
SRCS = $(wildcard $(SRC_DIR)/*.f90)

# Generate object file names based on source file names
OBJS = $(patsubst $(SRC_DIR)/%.f90,$(OBJ_DIR)/%.o,$(SRCS))

# Compiler flags
FFLAGS = -i8 -r8 -warn all -static -O3 -c
MODULE_FLAGS = -module $(MOD_DIR)

# Target to compile all object files and generate module files
all: $(LIBRARY)

$(LIBRARY): $(OBJS)
	ar rcs $@ $^
# Rule to compile individual source files into object files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	$(FC) $(FFLAGS) $(MODULE_FLAGS) -o $@ $<

clean:
	rm -f $(OBJS) $(MODS)

.PHONY: clean
