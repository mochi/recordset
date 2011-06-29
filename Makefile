REBAR:=./rebar

.PHONY: all erl up test clean doc

all: erl

erl:
	$(REBAR) get-deps compile

up:
	$(REBAR) update-deps compile

test: all
	@rm -rf .eunit
	@mkdir -p .eunit
	$(REBAR) skip_deps=true eunit

clean:
	$(REBAR) clean delete-deps

doc:
	$(REBAR) skip_deps=true doc
