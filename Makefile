APP=jsone
NODE=$(APP)@localhost

DIALYZER_OPTS=-Werror_handling -Wrace_conditions -Wunmatched_returns

DEPENDED_APPS=

all: compile xref eunit dialyze

init:
	@./rebar get-deps compile 

compile:
	@./rebar compile skip_deps=true

xref:
	@./rebar xref skip_deps=true

clean:
	@./rebar clean skip_deps=true

eunit:
	@./rebar eunit skip_deps=true

edoc:
	@./rebar doc skip_deps=true

start: compile
	erl -sname $(NODE) -pz ebin deps/*/ebin \
      -eval 'erlang:display({start_depended_app, [{A, application:start(A)} || A <- [$(DEPENDED_APPS)]]}).' \
      -eval 'erlang:display({start_app, $(APP), application:start($(APP))}).'

.dialyzer.plt:
	touch .dialyzer.plt
	dialyzer --build_plt --plt .dialyzer.plt --apps erts kernel stdlib $(shell echo $(DEPENDED_APPS) | sed -e 's/,/ /g')

dialyze: .dialyzer.plt
	dialyzer --plt .dialyzer.plt -r ebin $(DIALYZER_OPTS)

create_app:
	@./rebar create-app appid=$(APP) skip_deps=true
