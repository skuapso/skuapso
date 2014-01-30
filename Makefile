ifeq ($(PREFIX),)
  PWD := "./skuapso"
else
  PWD	:= "$(PREFIX)/skuapso"
endif

.PHONY: compile install clean deps rebar_%

compile:
	@rebar compile

deps:
	@rebar get-deps
	@rebar update-deps

install: config
	@echo "installing in $(PWD)"
	@mkdir -p $(PWD)
	@find . -name \*.beam -exec cp '{}' $(PWD) \;
	@find . -name \*.app -exec cp '{}' $(PWD) \;
	@echo done

config: apps/*/ebin/*.app
	@escript make_config.erl

clean:
	@rebar clean

rebar_%:
	@rebar $*
