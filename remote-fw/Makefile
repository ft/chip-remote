PROJECT = chip-remote

ASM =  $(wildcard third-party/stm32-f7xx/*.s)
SOURCES = $(wildcard third-party/stm32-f7xx/*.c)
SOURCES += $(wildcard third-party/stm32-hal/*.c)
SOURCES += $(wildcard third-party/stm32-usb/*.c)
SOURCES += $(wildcard nucleo-144/*.c cr/*.c *.c)
OBJECTS = $(patsubst %.s,%.o,$(ASM))
OBJECTS += $(patsubst %.c,%.o,$(SOURCES))
ASMLSTS = $(patsubst %.c,%.lst,$(SOURCES))

ifneq ($(VERBOSE),1)
QUIET := @
MAKEFLAGS += --no-print-directory
endif

RM = rm -f
CHMOD = chmod
SED = sed

TOOLCHAIN = arm-none-eabi
CC = $(TOOLCHAIN)-gcc
AS = $(TOOLCHAIN)-as
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

# Tell the compiler about STM third-party code
CFLAGS_STM = -Ithird-party/stm32-hal -Ithird-party/stm32-usb
CFLAGS_STM += -Ithird-party/stm32-f7xx -Inucleo-144 -DSTM32F767xx

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
AS_DEBUG =
else
CFLAGS_DEBUG = -O0 -ggdb
AS_DEBUG = -g
endif

ifeq ($(WITH_SEMIHOSTING),1)
CFLAGS_SEMIHOSTING = -DWITH_SEMIHOSTING
else
CFLAGS_SEMIHOSTING =
endif

# Store compiler options in ELF section
CFLAGS_EXTRA = -frecord-gcc-switches

CFLAGS = $(CFLAGS_STRICTNESS) $(CFLAGS_LANGUAGE) $(CFLAGS_TARGET)
CFLAGS += $(CFLAGS_STM) $(CFLAGS_ASSEMBLER) $(CFLAGS_DEBUG)
CFLAGS += $(CFLAGS_STANDALONE) $(CFLAGS_SEMIHOSTING) $(CFLAGS_EXTRA)

DFLAGS = $(CFLAGS_STRICTNESS) $(CFLAGS_LANGUAGE) $(CFLAGS_STM)

COMPILE = $(CC) $(CFLAGS)
ASSEMBLE = $(AS) $(CFLAGS_TARGET) $(AS_DEBUG)

# Like for the same target we build for
LDFLAGS_TARGET = $(CFLAGS_TARGET)

# The firmware is a standalone application
LDFLAGS_STANDALONE = -Wl,--start-group -lc -lnosys -Wl,--end-group

# And use the linker script for this particular board
LDFLAGS_SCRIPT = -Tnucleo-144/nucleo-144.ld

# Garbage collect sections from linker output
LDFLAGS_CLEANUP = -Wl,--gc-sections
ifeq ($(VERBOSE),1)
LDFLAGS_CLEANUP += -Wl,--print-gc-sections
endif

# Produce map files for firmware
LDFLAGS_MAPFILE = -Wl,-Map=$(patsubst %.elf,%.map,$(@))

# Semihosting support
ifeq ($(WITH_SEMIHOSTING),1)
LDFLAGS_SEMIHOSTING = -specs=rdimon.specs
LDFLAGS_SEMIHOSTING += -Wl,--start-group -lc -lrdimon -lgcc -Wl,--end-group
else
LDFLAGS_SEMIHOSTING =
endif

LDFLAGS = $(LDFLAGS_TARGET) $(LDFLAGS_SEMIHOSTING) $(LDFLAGS_STANDALONE) $(LDFLAGS_SCRIPT)
LDFLAGS += $(LDFLAGS_CLEANUP) $(LDFLAGS_MAPFILE)

ARTIFACTS = $(PROJECT).elf $(PROJECT).bin $(PROJECT).hex
ARTIFACTS += $(PROJECT).srec $(PROJECT).lst

.SUFFIXES = .c .s .o .elf .bin .hex .srec .lst

all: $(OBJECTS) $(ARTIFACTS)

clean:
	@printf '  CLEAN\n'
	@$(RM) *~ */*~ */*/*~
	@$(RM) $(OBJECTS)
	@$(RM) $(ASMLSTS)
	@$(RM) $(ARTIFACTS)

size:
	@printf 'Brief:\n'
	$(QUIET)$(BINSIZE) -B -x $(PROJECT).elf
	@printf '\nFull:\n'
	$(QUIET)$(BINSIZE) -A -x $(PROJECT).elf | $(SED) -e '/^$$/d'

depend:
	@printf '  DEPEND $$(SOURCES)\n'
	$(QUIET)mkdep $(DFLAGS) $(SOURCES)

-include .depend

# Suffix based dependencies

.c.o:
	@printf '  CC       %s\n' "$<"
	$(QUIET)$(COMPILE) -c -o $@ $<

.s.o:
	@printf '  AS       %s\n' "$<"
	$(QUIET)$(ASSEMBLE) -o $@ $<

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

.PHONY: all clean depend size
