TARGET		= utd83bt

ASM		= dasm

EMU		= stella

AFLAGS		:= -f3 -v5

SRC		= $(TARGET).s

default: build

.PHONY: build
build: $(SRC)
	$(ASM) $^ $(AFLAGS) -s$(TARGET).sym -o$(TARGET).bin
	@echo [DASM] Linked $^ into $(TARGET)

.PHONY: test
test:
	@$(EMU) $(TARGET).bin $(TARGET).sym
	@echo testing generated binary

.PHONY: clean
clean:
	@rm -f $(TARGET).bin $(TARGET).sym
	@echo removed $(TARGET).bin $(TARGET).sym

.PHONY: rebuild
rebuild: clean build
