LOAD_PATH = $(TOPDIR)/scheme
TEST_PATH = $(TOPDIR)/tests
GUILE_BINARY = guile
GUILE_CALL = $(GUILE_BINARY) -L $(LOAD_PATH) -C $(LOAD_PATH) --no-auto-compile
GUILD_BINARY = guild

CR = $(LOAD_PATH)/chip-remote

MODULES_CORE = $(CR)/bit-operations.scm
MODULES_CORE += $(CR)/codecs.scm
MODULES_CORE += $(CR)/decode.scm
MODULES_CORE += $(CR)/decode/to-text.scm
MODULES_CORE += $(CR)/decode/types.scm
MODULES_CORE += $(CR)/device.scm
MODULES_CORE += $(CR)/interpreter.scm
MODULES_CORE += $(CR)/item.scm
MODULES_CORE += $(CR)/manufacturer.scm
MODULES_CORE += $(CR)/named-value.scm
MODULES_CORE += $(CR)/page-map.scm
MODULES_CORE += $(CR)/process-plist.scm
MODULES_CORE += $(CR)/register-map.scm
MODULES_CORE += $(CR)/register.scm
MODULES_CORE += $(CR)/semantics.scm
MODULES_CORE += $(CR)/utilities.scm
MODULES_CORE += $(CR)/validate.scm

MODULES_MANUFACTURERS = $(CR)/manufacturer/bosch.scm
MODULES_MANUFACTURERS += $(CR)/manufacturer/linear-technology.scm
MODULES_MANUFACTURERS += $(CR)/manufacturer/texas-instruments.scm

MODULES_DEVICES = $(CR)/devices/bosch/bno055.scm
MODULES_DEVICES += $(CR)/devices/linear-technology/ltc6603.scm
MODULES_DEVICES += $(CR)/devices/texas-instruments/cdce72010.scm

MODULES= $(MODULES_CORE) $(MODULES_MANUFACTURERS) $(MODULES_DEVICES)
