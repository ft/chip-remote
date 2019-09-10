LOAD_PATH = $(TOPDIR)/scheme
TEST_PATH = $(TOPDIR)/tests
GUILE_BINARY ?= guile
GUILE_CALL = $(GUILE_BINARY) -L $(LOAD_PATH) -C $(LOAD_PATH) --no-auto-compile
GUILD_BINARY ?= guild

CR = $(LOAD_PATH)/chip-remote
CRD = $(LOAD_PATH)/documentation
CRDS = $(LOAD_PATH)/data-structures
CRP = $(LOAD_PATH)/protocol

MODULES_CORE = $(CR)/bit-operations.scm
MODULES_CORE += $(CR)/codecs.scm
MODULES_CORE += $(CR)/combination.scm
MODULES_CORE += $(CR)/commander.scm
MODULES_CORE += $(CR)/decode.scm
MODULES_CORE += $(CR)/decode/to-text.scm
MODULES_CORE += $(CR)/decode/to-c-values.scm
MODULES_CORE += $(CR)/decode/types.scm
MODULES_CORE += $(CR)/device.scm
MODULES_CORE += $(CR)/device/access.scm
MODULES_CORE += $(CR)/device/transmit.scm
MODULES_CORE += $(CR)/device/spi.scm
MODULES_CORE += $(CR)/interpreter.scm
MODULES_CORE += $(CR)/item.scm
MODULES_CORE += $(CR)/item/access.scm
MODULES_CORE += $(CR)/item/builder.scm
MODULES_CORE += $(CR)/io.scm
MODULES_CORE += $(CR)/manufacturer.scm
MODULES_CORE += $(CR)/modify.scm
MODULES_CORE += $(CR)/named-value.scm
MODULES_CORE += $(CR)/page-map.scm
MODULES_CORE += $(CR)/process-plist.scm
MODULES_CORE += $(CR)/protocol.scm
MODULES_CORE += $(CR)/register-map.scm
MODULES_CORE += $(CR)/register-window.scm
MODULES_CORE += $(CR)/register.scm
MODULES_CORE += $(CR)/register/common.scm
MODULES_CORE += $(CR)/register/modifiers.scm
MODULES_CORE += $(CR)/register/predicates.scm
MODULES_CORE += $(CR)/semantics.scm
MODULES_CORE += $(CR)/type-operations.scm
MODULES_CORE += $(CR)/units.scm
MODULES_CORE += $(CR)/utilities.scm
MODULES_CORE += $(CR)/validate.scm

MODULES_MANUFACTURERS = $(CR)/manufacturer/analog-devices.scm
MODULES_MANUFACTURERS += $(CR)/manufacturer/bosch.scm
MODULES_MANUFACTURERS += $(CR)/manufacturer/decawave.scm
MODULES_MANUFACTURERS += $(CR)/manufacturer/invensense.scm
MODULES_MANUFACTURERS += $(CR)/manufacturer/linear-technology.scm
MODULES_MANUFACTURERS += $(CR)/manufacturer/microchip.scm
MODULES_MANUFACTURERS += $(CR)/manufacturer/texas-instruments.scm

MODULES_DEVICES = $(CR)/devices/analog-devices/adf4158.scm
MODULES_DEVICES += $(CR)/devices/analog-devices/adf4158/registers.scm
MODULES_DEVICES += $(CR)/devices/analog-devices/adf4158/tables.scm
MODULES_DEVICES += $(CR)/devices/analog-devices/adf4169.scm
MODULES_DEVICES += $(CR)/devices/analog-devices/adf4169/registers.scm
MODULES_DEVICES += $(CR)/devices/analog-devices/adf4169/tables.scm
MODULES_DEVICES += $(CR)/devices/bosch/bno055.scm
MODULES_DEVICES += $(CR)/devices/decawave/dw1000.scm
MODULES_DEVICES += $(CR)/devices/decawave/dw1000/registers.scm
MODULES_DEVICES += $(CR)/devices/decawave/dw1000/tables.scm
MODULES_DEVICES += $(CR)/devices/invensense/icm-20602.scm
MODULES_DEVICES += $(CR)/devices/linear-technology/ltc6603.scm
MODULES_DEVICES += $(CR)/devices/microchip/mcp4351.scm
MODULES_DEVICES += $(CR)/devices/microchip/mcp4351/registers.scm
MODULES_DEVICES += $(CR)/devices/texas-instruments/ads4149.scm
MODULES_DEVICES += $(CR)/devices/texas-instruments/cdce72010.scm

MODULES_PROTO = $(CRP)/slip.scm

MODULES_DATASTRUCT += $(CRDS)/loadable-fifo.scm

MODULES_DOC = $(CRD)/combine-markdown.scm
MODULES_DOC += $(CRD)/module.scm
MODULES_DOC += $(CRD)/more.scm
MODULES_DOC += $(CRD)/render-markdown.scm

MODULES = $(MODULES_CORE) $(MODULES_MANUFACTURERS) $(MODULES_DEVICES)
MODULES += $(MODULES_DOC) $(MODULES_PROTO) $(MODULES_DATASTRUCT)
