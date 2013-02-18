CWD=$(shell pwd)
NAME=pusherman
APP_DIR=${CWD}/apps/${NAME}
DEPS=$(wildcard ${CWD}/deps/*/ebin)
APPS=$(wildcard ${CWD}/apps/*/ebin)
NODE=${NAME}@`hostname`
CT_LOG=${APP_DIR}/logs
REBAR?=rebar
ERL?=/usr/bin/env erl
ERLARGS=-pa ${DEPS} -pa ${APPS} -smp enable -name ${NODE} \
	-boot start_sasl -config ${CWD}/rel/files/sys -s lager
DIALYZER?=/usr/bin/env dialyzer
DIALYZER_OUT=${NAME}.plt

all: compile release

quick:
	@${REBAR} skip_deps=true compile

# Clean all.
clean:
	@${REBAR} clean

# Gets dependencies.
getdeps:
	@${REBAR} get-deps

# Compiles.
compile:
	@${REBAR} compile

# Dialyzer plt
${DIALYZER_OUT}:
	${DIALYZER} --verbose --build_plt -pa ${DEPS} --output_plt ${DIALYZER_OUT} \
		--apps kernel \
		stdlib \
		erts \
		compiler \
		hipe \
		crypto \
		edoc \
		gs \
		syntax_tools \
		tools \
		runtime_tools \
		inets \
		xmerl \
		ssl \
		mnesia \
		webtool

# Runs Dialyzer
analyze: ${DIALYZER_OUT} xref
	${DIALYZER} --verbose -pa ${DEPS} --plt ${DIALYZER_OUT} -Werror_handling \
		`find ${APP_DIR} -type f -name "${NAME}*.beam" | grep -v SUITE`

# Runs xref
xref:
	${REBAR} skip_deps=true xref
        
# Creates the rebar node.
node:
	(mkdir -p rel && cd rel && ${REBAR} create-node nodeid=${NAME} && cd ..)

# Creates a release.
release:
	@echo 'Generating ${NAME} release'
	@(mkdir -p rel && cd rel && ${REBAR} generate && cd ..)

# Runs the release, no console.
run:
	${CWD}/rel/${NAME}/bin/${NAME} start

# Cleans, recompiles, runs a console on the release.
allconsole: all console

# Runs a release with a console.
console:
	${CWD}/rel/${NAME}/bin/${NAME} console

# This one runs without a release.
shell: compile
	${ERL} ${ERLARGS} -config ${CWD}/rel/files/sys -s ${NAME}

# Intended to be run by a CI server.
test: compile #analyze
	@rm -rf ${CT_LOG}
	@find ${APP_DIR}/src -type f -exec cp {} ${APP_DIR}/ebin \;
	@find ${APP_DIR}/test -type f -exec cp {} ${APP_DIR}/ebin \;
	@ERL_FLAGS="${ERLARGS} -config ${CWD}/etc/test" \
		ERL_AFLAGS="${ERLARGS}" \
		${REBAR} -v 3 skip_deps=true ct
	@find ${APP_DIR}/ebin -type f -name "*.erl" -exec rm {} \;

# If you're a dev, run this target instead of the one above: it will open
# the coverage results on your browser.
devtest: test
	@open ${CT_LOG}/index.html


