all: compile xref eunit dialyze edoc

compile:
	@./rebar3 compile

xref:
	@./rebar3 xref

clean:
	@./rebar3 clean

eunit:
	@./rebar3 eunit

edoc:
	@./rebar3 as doc edoc

start: compile
	@./rebar3 shell

dialyze: compile
	@./rebar3 dialyzer
