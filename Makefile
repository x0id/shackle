CACHEGRIND=qcachegrind
REBAR3=$(shell which rebar3)
ifeq ($(REBAR3),)
REBAR3=./bin/rebar3
endif

all: compile

bench:
	@echo "Benchmarking..."
	@$(REBAR3) as bench compile
	@./bin/bench.sh

clean:
	@echo "Running rebar3 clean..."
	@$(REBAR3) clean -a

compile:
	@echo "Running rebar3 compile..."
	@$(REBAR3) as compile compile

dialyzer:
	@echo "Running rebar3 dialyze..."
	@$(REBAR3) dialyzer

edoc:
	@echo "Running rebar3 edoc..."
	@$(REBAR3) as edoc edoc

elvis:
	@echo "Running elvis rock..."
	@$(REBAR3) lint

eunit:
	@echo "Running rebar3 eunit..."
	@$(REBAR3) do eunit -cv, cover -v

profile:
	@echo "Profiling..."
	@$(REBAR3) as profile compile
	@./bin/profile.sh
	@_build/profile/lib/fprofx/erlgrindx -p fprofx.analysis
	@$(CACHEGRIND) fprofx.cgrind

test: elvis xref eunit dialyzer

xref:
	@echo "Running rebar3 xref..."
	@$(REBAR3) xref

.PHONY: bench clean compile dialyzer edoc elvis eunit profile xref
