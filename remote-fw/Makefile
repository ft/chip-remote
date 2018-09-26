PROJECT = hello-world

SOURCES = $(wildcard cr/*.c *.c)
OBJECTS = $(patsubst %.c,%.o,$(SOURCES))
ASMLSTS = $(patsubst %.c,%.lst,$(SOURCES))
OPENCM3 = opencm3

ifneq ($(VERBOSE),1)
QUIET := @
MAKEFLAGS += --no-print-directory
endif

RM = rm -f
CHMOD = chmod
SED = sed

TOOLCHAIN = arm-none-eabi
CC = $(TOOLCHAIN)-gcc
OBJCOPY = $(TOOLCHAIN)-objcopy
OBJDUMP = $(TOOLCHAIN)-objdump
BINSIZE = $(TOOLCHAIN)-size

# Be pretty strict
CFLAGS_STRICTNESS = -Wall -Wextra -Wpedantic -Wimplicit-function-declaration
CFLAGS_STRICTNESS += -Wredundant-decls -Wmissing-prototypes -Wstrict-prototypes
CFLAGS_STRICTNESS += -Wundef -Wshadow

ifneq ($(RELEASE),1)
CFLAGS_STRICTNESS += -Werror
endif

# Use a reasonably modern language standard
CFLAGS_LANGUAGE = -std=c99

# Tell the compiler what machine we're building for
CFLAGS_TARGET = -mcpu=cortex-m7 -mlittle-endian -mthumb
CFLAGS_TARGET += -mfloat-abi=hard -mfpu=fpv5-sp-d16

# Set the compiler up to build with the opencm3 module
CFLAGS_OPENCM3 = -I$(OPENCM3)/include -DSTM32F7

# -Wa,OPTIONS passes OPTIONS to the assembler. This is useful to generate
# assembler listings during the build process. We're assuming the GNU assem-
# bler with its features here. The options we're passing in:
#
#    a   Generate assembler listings
#    g   General information: Version, Switches etc.
#    s   Symbol-table listing
#    l   Output-program listing
#    h   High-level language listing (requires -g with compiler flags to work)
#
# The =... defines an output file for the listings.
CFLAGS_ASSEMBLER = -Wa,-agslh=$(patsubst %.c,%.lst,$(<))

ifeq ($(RELEASE),1)
CFLAGS_DEBUG = -Os
else
CFLAGS_DEBUG = -O0 -ggdb
endif

ifeq ($(WITH_SEMIHOSTING),1)
CFLAGS_SEMIHOSTING = -DWITH_SEMIHOSTING
else
CFLAGS_SEMIHOSTING =
endif

# Store compiler options in ELF section
CFLAGS_EXTRA = -frecord-gcc-switches

CFLAGS = $(CFLAGS_STRICTNESS) $(CFLAGS_LANGUAGE) $(CFLAGS_TARGET)
CFLAGS += $(CFLAGS_OPENCM3) $(CFLAGS_ASSEMBLER) $(CFLAGS_DEBUG)
CFLAGS += $(CFLAGS_STANDALONE) $(CFLAGS_SEMIHOSTING) $(CFLAGS_EXTRA)

COMPILE = $(CC) $(CFLAGS)

# Like for the same target we build for
LDFLAGS_TARGET = $(CFLAGS_TARGET)

# The firmware is a standalone application
LDFLAGS_STANDALONE = -static -nostartfiles -nodefaultlibs -nostdlib

# Link against the right opencm3 build
LDFLAGS_OPENCM3 = -L$(OPENCM3)/lib -lopencm3_stm32f7

# And use the linker script for this particular board
LDFLAGS_SCRIPT = -Tnucleo-144.ld

# Garbage collect sections from linker output
LDFLAGS_CLEANUP = -Wl,--gc-sections
ifeq ($(VERBOSE),1)
LDFLAGS_CLEANUP += -Wl,--print-gc-sections
endif

# Produce map files for firmware
LDFLAGS_MAPFILE = -Wl,-Map=$(patsubst %.elf,%.map,$(@))

# Semihosting support
ifeq ($(WITH_SEMIHOSTING),1)
LDFLAGS_SEMIHOSTING = --specs=rdimon.specs
LDFLAGS_SEMIHOSTING += -Wl,--start-group -lc -lrdimon -lgcc -Wl,--end-group
else
LDFLAGS_SEMIHOSTING =
endif

LDFLAGS = $(LDFLAGS_TARGET) $(LDFLAGS_STANDALONE)
LDFLAGS += $(LDFLAGS_OPENCM3) $(LDFLAGS_SCRIPT)
LDFLAGS += $(LDFLAGS_CLEANUP) $(LDFLAGS_MAPFILE)
LDFLAGS += $(LDFLAGS_SEMIHOSTING)

# Main targets: Use "make opencm3" once. "make all" to build the firmware.
# Similarly there are "make opencm3-clean" and "make clean" for cleanup.

ARTIFACTS = $(PROJECT).elf $(PROJECT).bin $(PROJECT).hex
ARTIFACTS += $(PROJECT).srec $(PROJECT).lst

.SUFFIXES = .c .o .elf .bin .hex .srec .lst

all: $(OBJECTS) $(ARTIFACTS)

clean:
	$(RM) *~ */*~ */*/*~
	$(RM) $(OBJECTS)
	$(RM) $(ASMLSTS)
	$(RM) $(PROJECT).map
	$(RM) $(PROJECT).elf
	$(RM) $(PROJECT).bin
	$(RM) $(PROJECT).hex
	$(RM) $(PROJECT).srec

opencm3:
	@printf '  LIBRARY opencm3\n'
	$(QUIET)$(MAKE) -C $(OPENCM3)

opencm3-clean:
	@printf '  LIBRARY opencm3: clean\n'
	$(QUIET)$(MAKE) -C $(OPENCM3) clean

size:
	@printf 'Brief:\n'
	$(QUIET)$(BINSIZE) -B -x $(PROJECT).elf
	@printf '\nFull:\n'
	$(QUIET)$(BINSIZE) -A -x $(PROJECT).elf | $(SED) -e '/^$$/d'

# Suffix based dependencies

.c.o:
	@printf '  CC       %s\n' "$<"
	$(QUIET)$(COMPILE) -c -o $@ $<

%.elf: $(OBJECTS)
	@printf '  LD       %s\n' "$@"
	$(QUIET)$(CC) -o $@ $(OBJECTS) $(LDFLAGS)
	$(QUIET)$(CHMOD) -x $@

%.bin: %.elf
	@printf '  OBJCOPY  %s\n' "$@"
	$(QUIET)$(OBJCOPY) -Obinary $< $@
	$(QUIET)$(CHMOD) -x $@

%.hex: %.elf
	@printf '  OBJCOPY  %s\n' "$@"
	$(QUIET)$(OBJCOPY) -Oihex $< $@

%.srec: %.elf
	@printf '  OBJCOPY  %s\n' "$@"
	$(QUIET)$(OBJCOPY) -Osrec $< $@
	$(QUIET)$(CHMOD) -x $@

%.lst: %.elf
	@printf '  OBJDUMP  %s\n' "$@"
	$(QUIET)$(OBJDUMP) -S $< > $@

.PHONY: all clean opencm3 opencm3-clean size
