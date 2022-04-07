.DEFAULT_TARGET := help
.PHONY := help

EMACS := emacs

help:
	@echo "Makefile to manage configuration."
	@echo "    install        Install all deps to make the config work."
	@echo "    help           Show this."

install:
	@$(EMACS) --batch -l ./setup/install.el -f install

uninstall:
	@$(EMACS) --batch -l ./setup/install.el -f uninstall
