all: compile xref eunit dialyze edoc

rebar ?= rebar3
rebar_cmd = $(rebar) $(profile:%=as %)

compile:
	@$(rebar_cmd) compile

xref:
	@$(rebar_cmd) xref

clean:
	@$(rebar_cmd) clean

eunit:
	@$(rebar_cmd) do eunit,cover

edoc: profile=edown
edoc:
	@$(rebar_cmd) edoc

start: compile
	-@$(rebar_cmd) shell

dialyze: compile
	@$(rebar_cmd) dialyzer
