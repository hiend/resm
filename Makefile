PROJECT = resm
VSN	= 1.0.1

ERLC_OPTS  := +debug_info +"{cover_enabled, true}"
EUNIT_OPTS := verbose
EDOC_OPTS  := {doclet, edown_doclet}, {top_level_readme, {"./README.md", "http://github.com/hiend/resm"}}, \
				{stylesheet, ""}, {image, ""}

DEPS = cowboy jiffy lager edown sync
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.1
dep_jiffy = git https://github.com/davisp/jiffy 0.13.3
dep_lager = git https://github.com/basho/lager 2.1.1
dep_edown = git https://github.com/esl/edown master
dep_sync = git https://github.com/rustyio/sync master

include erlang.mk

REL = _rel/$(PROJECT)

dev-rel: deps
	$(RELX) -c relx-dev.config

console:
	$(REL)/bin/$(PROJECT) console

# need fpm (see https://github.com/jordansissel/fpm)
deb: rel
	find $(REL) -type d -print0 | xargs -0 chmod 755
	find $(REL) -type f -print0 | xargs -0 chmod 644
	chmod -R +x $(REL)/bin
	chmod -R +x $(REL)/*/bin
	fpm -f -s dir -t deb -a native \
		--version $(VSN) \
		--name $(PROJECT) \
		--description "Simple resource manager that provides resources on demand" \
		--url "http://github.com/hiend/resm" \
		--license MIT \
		--maintainer "Dmitry Averbakh <adm@ruhub.com>" \
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

.PHONY: deb
