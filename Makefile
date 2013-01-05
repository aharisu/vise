.PHONY: all

GOSH           = "gosh"
GENVISE        = "autoload/genvise.scm"

VIMFILES = autoload/vise_repl.vim autoload/repl_mapping.vim plugin/vise_repl.vim
GENERATED = $(VIMFILES)

#generate vise -> vim file
.SUFFIXES:.vise .vim

.vise.vim:
	$(GOSH) $(GENVISE) -o $(addsuffix .vim, $(basename $<)) $<


CONFIG_GENERATED = Makefile

all : $(VIMFILES)

autoload/vise_repl.vim: autoload/vise_repl.vise

autoload/repl_mapping.vim: autoload/repl_mapping.vise

plugin/vise_repl.vim: plugin/vise_repl.vise

clean :
	rm $(GENERATED)
