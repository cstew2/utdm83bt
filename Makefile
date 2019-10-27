TARGET		= utd83bt

ASM		= ../tools/dasm/bin/dasm

EMU		= ../tools/stella/stella

AFLAGS		:= -f3 -v5

SRC		= $(TARGET).s

default: build

.PHONY: build
build: $(SRC)
	$(ASM) $^ $(AFLAGS) -s$(TARGET).sym -o$(TARGET).bin
	@echo [DASM] Linked $^ into $(TARGET)

.PHONY: test
test:
	@$(EMU) $(TARGET).bin
	@echo testing generated binary

.PHONY: clean
clean:
	@rm $(TARGET).bin
	@echo removed $(TARGET).bin

.PHONY: rebuild
rebuild: clean build
