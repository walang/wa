MAKEDIRS := src/wa

all clean install uninstall :
	@$(foreach DIR, $(MAKEDIRS), $(MAKE) $(MAKEFLAGS) -C $(DIR) $@)

test: all
	prove -scfe src/wa/wa t/*
