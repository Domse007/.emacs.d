.DEFAULT_TARGET := help
.PHONY := help

PM_UPDATE := pacman -Syyu
PM := pacman -S
PM_NO_CONFIRM := --noconfirm

NON_LINUX := make git
DEPS := texlive-most rustup rust-analyzer texlive-most python
LINUX := unzip zip

deps:
	@echo "Installing all required deps..."
	$(PM_UPDATE) $(PM_NO_CONFIRM)
	$(PM) $(DEPS) $(LINUX) $(PM_NO_CONFIRM)
	@echo "Done."

windows:
	@echo "Installing all required deps..."
	$(PM_UPDATE) $(PM_NO_CONFIRM)
	$(PM) $(NON_LINUX) $(DEPS) $(PM_NO_CONFIRM)
	@echo "Done."

help:
	@echo "Install all dependencies for this configuration."
	@echo "                make deps                       "
	@echo "If this is a windows machine, make sure to have "
	@echo "msys2 installed. Also you have to call:         "
	@echo "                make windows                    "

