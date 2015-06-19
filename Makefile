PROJECT = resm
DESC = "Simple resource manager that provides resources on demand"
HOMEPAGE = "http://github.com/hiend/resm"
VSN = $(shell git describe --always --tags | sed -e "s/-[^-]*$$//;s/-/./")
REL = _rel/$(PROJECT)

OTP_VSN = $(shell export PATH="$(PATH)"; erl -noshell -eval 'io:format(erlang:system_info(otp_release)), halt().')
OTP_ROOT = $(shell export PATH="$(PATH)"; erl -noshell -eval 'io:format(code:root_dir()), halt().')
ERTS_VSN = $(shell export PATH="$(PATH)"; erl -noshell -eval 'io:format(erlang:system_info(version)), halt().')

OTP_PLT = .otp_$(OTP_VSN).plt
DEPS_PLT = .deps.plt

REBAR ?= $(CURDIR)/rebar
REBAR_URL ?= https://github.com/rebar/rebar/releases/download/2.5.1/rebar
define get_rebar
	curl -s -L -o $(REBAR) $(REBAR_URL) || rm $(REBAR)
	chmod +x $(REBAR)
endef
export REBAR

RELX ?= $(CURDIR)/relx
RELX_URL ?= https://github.com/erlware/relx/releases/download/v2.0.0/relx
define get_relx
	curl -s -L -o $(RELX) $(RELX_URL) || rm $(RELX)
	chmod +x $(RELX)
endef
export RELX

all: deps release

$(REBAR):
	$(call get_rebar)

$(RELX):
	$(call get_relx)

deps: $(REBAR)
	$(REBAR) get-deps

compile: clean $(REBAR)
	$(REBAR) --quiet compile

release: compile $(RELX)
	$(RELX) -c relx.config
	@find $(REL) -type d -print0 | xargs -0 chmod 755
	@find $(REL) -type f -print0 | xargs -0 chmod 644
	@chmod -R +x $(REL)/bin
	@chmod -R +x $(REL)/erts-$(ERTS_VSN)/bin

console: release
	$(REL)/bin/$(PROJECT)-$(VSN) console

clean: $(REBAR)
	@rm -rf _rel .rebar .eunit *.dump dialyzer.log
	$(REBAR) --quiet clean

test: deps compile $(REBAR)
	$(REBAR) skip_deps=true --quiet eunit

release-dev: compile $(RELX)
	$(RELX) -c relx-dev.config
	@find $(REL) -type d -print0 | xargs -0 chmod 755
	@find $(REL) -type f -print0 | xargs -0 chmod 644
	@chmod -R +x $(REL)/bin
	@chmod -R +x $(REL)/erts-$(ERTS_VSN)/bin

console-dev: release-dev
	$(REL)/bin/$(PROJECT)-$(VSN) console

$(OTP_PLT):
	dialyzer --verbose --build_plt --output_plt $(OTP_PLT) --apps $(OTP_ROOT)/lib

$(DEPS_PLT): deps compile
	dialyzer --verbose --build_plt --output_plt $(DEPS_PLT) --apps deps

dialyzer: $(OTP_PLT) $(DEPS_PLT) compile
	dialyzer --no_check_plt --plts $(OTP_PLT) $(DEPS_PLT) -- ebin | tee dialyzer.log

docs: $(REBAR)
	@rm -rf README.md doc/edoc-info doc/*.md
	$(REBAR) --quiet get-deps compile doc

# need fpm (see https://github.com/jordansissel/fpm)
deb: release
	fpm -f -s dir -t deb -a native \
		--version $(VSN) \
		--name $(PROJECT) \
		--description $(DESC) \
		--url $(HOMEPAGE) \
		--license MIT \
		--maintainer "Dmitry Averbakh <adm@ruhub.com>" \
		--depends erlang-base \
		--deb-pre-depends adduser \
		--after-install deb/postinst \
		--after-remove deb/postrm \
		--config-files /etc/$(PROJECT)/sys.config \
		--config-files /etc/$(PROJECT)/vm.args \
		--config-files /etc/init.d/$(PROJECT) \
		$(REL)/=/usr/lib/$(PROJECT) \
		$(REL)/log/=/var/log/$(PROJECT) \
		etc/sys.config=/etc/$(PROJECT)/sys.config \
		etc/vm.args=/etc/$(PROJECT)/vm.args \
		deb/init=/etc/init.d/$(PROJECT)

# need lintian ($ sudo apt-get install lintian)
lint: deb
	lintian $(PROJECT)_$(VSN)_amd64.deb

.PHONY: deps test deb
