REBAR=rebar3

all: compile

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

check: test xref

test:
	mkdir -p logs
	$(REBAR) eunit verbose=1

xref:
	$(REBAR) xref

doc: edoc
docs: edoc
edoc:
	$(REBAR) edoc

clean:
	$(REBAR) clean skip_deps=true

clean-all:
	rm -rf .eunit
	$(REBAR) clean

.PHONY: all deps compile compile check test xref doc clean clean-all


