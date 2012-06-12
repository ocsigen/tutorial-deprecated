
# This Makefile may help to mainain a directory of versions of the tutorial like
#
# $ tree -L 2 /tmp/tuto
#   /tmp/tuto
#   ├── 1.2.3
#   │   ├── files
#   │   └── src
#   ├── 1.2.4
#   │   ├── files
#   │   └── src
#   ├── dev
#   │   ├── files -> /var/www/data/darcs/tutorial/files
#   │   └── src -> /var/www/data/darcs/tutorial/src
#   └── stable -> 1.2.4
#
# $ make show-help

-include Makefile.local
TUTORIAL_DIR ?= /var/www/data/manualwiki/tutorial
TUTORIAL_VERSION ?= $(error "Specify a version through TUTORIAL_VERSION, the version dev are just links")

show-help:
	@echo "Usage:"
	@echo "TUTORIAL_VERSION=x.y.z make install"
	@echo "The target directory may be passed by TUTORIAL_DIR."
	@echo "Also Makefile.local may be used."

install:
	@echo ">> Installing tutorial version $(TUTORIAL_VERSION) to $(TUTORIAL_DIR)"
	sudo -u www-data rm -rf $(TUTORIAL_DIR)/$(TUTORIAL_VERSION)
	sudo -u www-data mkdir -p $(TUTORIAL_DIR)/$(TUTORIAL_VERSION)
	sudo -u www-data cp -r src $(TUTORIAL_DIR)/$(TUTORIAL_VERSION)/
	sudo -u www-data cp -r files $(TUTORIAL_DIR)/$(TUTORIAL_VERSION)/

set-stable:
	(cd $(TUTORIAL_DIR); sudo -u www-data ln -sf $(TUTORIAL_VERSION) stable)

