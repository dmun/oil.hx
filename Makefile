ARGS ?= LICENSE

dev:
	HELIX_STEEL_CONFIG="$(CURDIR)/.helix-dev" hx $(ARGS)
