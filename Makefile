# Makefile

FC = ifx
SRC_DIR = src
OBJ_DIR = build
EX_DIR = Examples
MOD_DIR = include
LIBRARY = librandom.a
# List of source files
SRCS = src/rand.f90 src/pcg32.f90 src/splitmix64.f90 src/xoshiro.f90
EXAMPLES = Examples/xoshiro256_example.f90
# Generate object file names based on source file names
OBJS = $(patsubst $(SRC_DIR)/%.f90,$(OBJ_DIR)/%.o,$(SRCS))
EXAMPLES_EXE = $(patsubst $(EX_DIR)/%.f90,%,$(EXAMPLES))
# Compiler flags
FFLAGS = -i8 -r8 -warn all -static -O3
MODULE_FLAGS = -module $(MOD_DIR)

# Target to compile all object files and generate module files
all: $(LIBRARY)

$(LIBRARY): $(OBJS)
	ar rcs $@ $^
# Rule to compile individual source files into object files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	$(FC) $(FFLAGS) -c $(MODULE_FLAGS) -o $@ $<

clean:
	rm -f $(OBJS) $(MODS)
	rm -f $(EXAMPLES_EXE)
.PHONY: clean
examples: $(EXAMPLES_EXE)

%: $(EX_DIR)/%.f90
	$(FC) $(FFLAGS) -o $@ -I include/ $< -L ./ -lrandom