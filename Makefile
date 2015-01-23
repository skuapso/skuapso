ifeq ($(PREFIX),)
  PWD := "./skuapso"
else
  PWD	:= "$(PREFIX)/skuapso"
endif
ifeq ($(DETAILS),)
	GIT_MSG_CMD := "git status --porcelain"
else
	GIT_MSG_CMD := "git status"
endif
ifeq ($(SKUAPSO_GIT_PREFIX),)
	GIT_PREFIX := "https://github.com/skuapso/"
else
	GIT_PREFIX := $(SKUAPSO_GIT_PREFIX)
endif
APPSFILES := $(wildcard ebin/*.app lib/*/ebin/*.app)

.PHONY: compile install clean deps rebar_% config

compile:
	@rebar compile

deps:
	@SKUAPSO_GIT_PREFIX=$(GIT_PREFIX) rebar get-deps
	@SKUAPSO_GIT_PREFIX=$(GIT_PREFIX) rebar update-deps

install: config
	@echo "installing in $(PWD)"
	@mkdir -p $(PWD)
	@find . -name \*.beam -exec cp '{}' $(PWD) \;
	@find . -name \*.app -exec cp '{}' $(PWD) \;
	@echo done

config:
	@APPS="$(APPSFILES)" escript make_config.erl

clean:
	@rebar clean

rebar_%:
	@rebar $*

lib-changes:
	@for i in $$(ls lib); do\
		(\
			cd lib/$$i;\
			MSG_CMD=$(GIT_MSG_CMD);\
			MSG=`$$MSG_CMD`;\
			if [ ! -z "$$MSG" ]; then\
				printf "\t\t\t \033[01;33m** in $$i\033[00m\n"; \
				echo "$$MSG";\
			fi\
		);\
	done
