# Copyright (c) 2017-2018 Flycheck contributors
# Copyright (c) 2015-2016 Sebastian Wiesner <swiesner@lunaryorn.com>

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# These variables may be overridden by the user
TEXINFO_VERSION ?= 6.1
EMACS_VERSION ?= 25.3
VERBOSE ?= no
MAKE_JOBS ?= 2

ifeq ($(VERBOSE),yes)
SILENT=
else
SILENT=> /dev/null
endif

# Tear the version apart
VERSION_PARTS := $(subst -, ,$(EMACS_VERSION))
VERSION_PART := $(word 1,$(VERSION_PARTS))
PRE_RELEASE_PART := $(word 2,$(VERSION_PARTS))
MAJOR_VERSION := $(word 1,$(subst ., ,$(EMACS_VERSION)))
# Whether the version is a release candidate
PRETEST ?= $(findstring rc,$(PRE_RELEASE_PART))

# Build a minimal Emacs with no special flags, to build as fast as possible
ifndef EMACSCONFFLAGS
EMACSCONFFLAGS := --with-x-toolkit=no --without-x --without-all --with-xml2 \
	CFLAGS='-O2 -march=native' \
	CXXFLAGS='-O2 -march=native'

ifeq ($(shell [ "$(EMACS_VERSION)" = snapshot ] || [ "$(MAJOR_VERSION)" -ge 25 ]; echo $$?),0)
EMACSCONFFLAGS += --with-modules
endif
endif

ifeq ($(TRAVIS_OS_NAME),osx)
# The "games" group does not exist on macOS, so we configure --with-gameuser to the current user (i.e., "travis")
EMACSCONFFLAGS += --with-gameuser=$(USER)
# Non-toolkit scroll bars are not implemented for Nextstep.
EMACSCONFFLAGS += --with-toolkit-scroll-bars=yes
endif

# Clone Emacs from the Github mirror because it's way faster than upstream
EMACS_GIT_URL = https://github.com/emacs-mirror/emacs.git
# Emacs FTP URL.  Prereleases are on alpha.gnu.org
ifeq ($(PRETEST),)
EMACS_FTP_URL = "https://ftp.gnu.org/gnu/emacs"
else
EMACS_FTP_URL = "http://alpha.gnu.org/pub/gnu/emacs/pretest"
endif
# URL of the TAR file
EMACS_TAR_URL = $(EMACS_FTP_URL)/emacs-$(EMACS_VERSION).tar.xz

# If it's an RC the real reported Emacs version is the version without the
# prerelease postfix.  Otherwise it's just the version that we get.
ifneq ($(PRETEST),)
REPORTED_EMACS_VERSION = $(VERSION_PART)
else
REPORTED_EMACS_VERSION = $(EMACS_VERSION)
endif

# FIXME: Add test for minimal Emacs version.
# See: https://github.com/flycheck/emacs-travis/issues/10

# The Emacs 24.3 doesn't have "--enable-silent-rules" configure option
ifeq ($(EMACS_VERSION),24.3)
CONFIGUREFLAGS = --quiet --prefix="$(HOME)"
else
CONFIGUREFLAGS = --quiet --enable-silent-rules --prefix="$(HOME)"
endif

# The Ubuntu 14.04 image on Travis has an outdated GnuTLS which prevents us from
# pulling from ELPA/MELPA under Emacs 26 and above.
# See https://emacs.stackexchange.com/a/38404
ifeq ($(MAJOR_VERSION),26)
configure_emacs: install_gnutls
else ifeq ($(EMACS_VERSION),snapshot)
configure_emacs: install_gnutls
endif

# Tell recipe processes about the reported Emacs version
export REPORTED_EMACS_VERSION

.PHONY: download_emacs_stable clone_emacs_snapshot install_gnutls
.PHONY: configure_emacs install_emacs install_cask install_texinfo
.PHONY: test

install_gnutls:
ifeq ($(TRAVIS_OS_NAME),osx)
#	GnuTLS can be upgraded with Homebrew instead of apt-get (which obviously does not exist on macOS)
	@echo "Upgrade GnuTLS 3"
	@if brew outdated --quiet | grep --quiet "gnutls"; then brew upgrade gnutls; fi
else
	@echo "Install GnuTLS 3"
	@sudo apt-get -qq update
	@sudo apt-get install -y build-essential nettle-dev libgmp-dev
	@wget https://www.gnupg.org/ftp/gcrypt/gnutls/v3.1/gnutls-3.1.23.tar.xz
	@tar -xf gnutls-3.1.23.tar.xz
	@cd gnutls-3.1.23 \
	  && ./configure $(SILENT) \
	  && make -j$(MAKE_JOBS) $(SILENT) \
	  && sudo make install $(SILENT) \
	  && sudo ln -s /usr/local/lib/libgnutls.so.28 /usr/lib/libgnutls.so.28
endif

download_emacs_stable:
	@echo "Download Emacs $(EMACS_VERSION) from $(EMACS_TAR_URL)"
	@curl -o "/tmp/emacs-$(EMACS_VERSION).tar.xz" "$(EMACS_TAR_URL)"
	@tar xf "/tmp/emacs-$(EMACS_VERSION).tar.xz" -C /tmp
	@mkdir -p `dirname "$(EMACS_DIR)"`
	@mv /tmp/emacs-$(REPORTED_EMACS_VERSION) "$(EMACS_DIR)"

clone_emacs_snapshot:
	@echo "Clone Emacs from Git"
	git clone -q --depth=1 '$(EMACS_GIT_URL)' $(EMACS_DIR)
# Create configure
	cd $(EMACS_DIR) && ./autogen.sh

configure_emacs:
	@echo "Configure Emacs $(EMACS_VERSION)"
	@cd "$(EMACS_DIR)" && ./configure $(CONFIGUREFLAGS) $(EMACSCONFFLAGS) $(SILENT)

ifeq ($(EMACS_VERSION),snapshot)
EMACS_DIR = /tmp/emacs
configure_emacs: clone_emacs_snapshot
else
EMACS_DIR = $(HOME)/emacs/$(EMACS_VERSION)
configure_emacs: download_emacs_stable
endif

install_emacs:
	@echo "Install Emacs $(EMACS_VERSION)"
	@make -j$(MAKE_JOBS) -C "$(EMACS_DIR)" V=0 install $(SILENT)
ifeq ($(TRAVIS_OS_NAME),osx)
#	To pretend that an up-to-date emacs exists in $HOME/bin, we must link it out of Emacs.app
	@mkdir -p "$(HOME)/bin"
	@ln -s "$(HOME)/emacs/$(EMACS_VERSION)/nextstep/Emacs.app/Contents/MacOS/Emacs" "$(HOME)/bin/emacs"
endif

# Run configure (and download) only if directory is absent
ifeq ($(wildcard $(EMACS_DIR)/.),)
install_emacs: configure_emacs
endif

install_cask:
	@echo "Install Cask"
	@git clone --depth=1 https://github.com/cask/cask.git "$(HOME)/.cask"
	@mkdir -p "$(HOME)/bin"
	@ln -s "$(HOME)/.cask/bin/cask" "$(HOME)/bin/cask"

install_texinfo:
	@echo "Install Texinfo $(TEXINFO_VERSION)"
	@curl -sS -o "/tmp/texinfo-$(TEXINFO_VERSION).tar.gz" \
		'http://ftp.gnu.org/gnu/texinfo/texinfo-$(TEXINFO_VERSION).tar.gz'
	@tar xzf "/tmp/texinfo-$(TEXINFO_VERSION).tar.gz" -C /tmp
	@cd "/tmp/texinfo-$(TEXINFO_VERSION)" && \
		CFLAGS="$(CFLAGS) -Wno-unused-result" ./configure $(CONFIGUREFLAGS) $(SILENT)
# Patching Makefile to inhibit unexpected warnings.
# See: https://github.com/flycheck/emacs-travis/pull/9
	@sed -i -e "s/^CFLAGS =\(.*\)/CFLAGS = \1 -Wno-unused-result/g" "/tmp/texinfo-$(TEXINFO_VERSION)/info/Makefile"
	@make -j$(MAKE_JOBS) -C "/tmp/texinfo-$(TEXINFO_VERSION)" V=0 install $(SILENT)

test:
	bundle exec rspec --color --format doc
