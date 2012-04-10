
## Sample Makefile for eliom application.

# Adapt those values:
PROJECT_NAME :=
PORT := 8080
PREFIX := ${shell pwd}/local
DATADIR := ${PREFIX}/var/www/data/${PROJECT_NAME}
LOGDIR := ${PREFIX}/var/log/${PROJECT_NAME}
CMDPIPE := ${PREFIX}/var/run/${PROJECT_NAME}-cmd
STATICDIR := ${PREFIX}/var/www/${PROJECT_NAME}
LIBDIR := ${PREFIX}/var/lib/${PROJECT_NAME}

ifeq (${PROJECT_NAME}, )
  $(error "Please adapt the PROJECT_NAME in the Makefile. Then type: make start; make byte install run")
endif

## Packages required to build the server part of the application

SERVER_PACKAGES :=

## Packages to be linked in the client part

CLIENT_PACKAGES :=

## Source files for the server part

SERVER_FILES := ${wildcard *.eliom}

## Source files for the client part

CLIENT_FILES := ${wildcard *.eliom}

## Required binaries

ELIOMC      := eliomc
ELIOMOPT    := eliomopt
ELIOMDEP    := eliomdep
JS_OF_ELIOM := js_of_eliom

## Where to put intermediate object files.
## - ELIOM_{SERVER,CLIENT}_DIR must be distinct
## - ELIOM_CLIENT_DIR mustn't be the local dir.
## - ELIOM_SERVER_DIR could be ".", but you need to
##   remove it from the "clean" rules...

export ELIOM_SERVER_DIR := _server
export ELIOM_CLIENT_DIR := _client
export ELIOM_TYPE_DIR   := .

#####################################

all: byte

reload: install
	@if [ -e $(CMDPIPE) ]; then echo reload > $(CMDPIPE); fi

byte:: ${PROJECT_NAME}.cma ${PROJECT_NAME}.js
opt:: ${PROJECT_NAME}.cmxs ${PROJECT_NAME}.js

#### Server side compilation #######

SERVER_INC  := ${addprefix -package ,${SERVER_PACKAGES}}

SERVER_OBJS := $(patsubst %.eliom,${ELIOM_SERVER_DIR}/%.cmo, ${SERVER_FILES})
SERVER_OBJS := $(patsubst %.ml,${ELIOM_SERVER_DIR}/%.cmo, ${SERVER_OBJS})

${PROJECT_NAME}.cma: ${SERVER_OBJS}
	${ELIOMC} -a -o $@ $^
${PROJECT_NAME}.cmxa: ${SERVER_OBJS:.cmo=.cmx}
	${ELIOMOPT} -a -o $@ $^

${ELIOM_TYPE_DIR}/%.type_mli: %.eliom
	${ELIOMC} -infer ${SERVER_INC} $<

${ELIOM_SERVER_DIR}/%.cmi: %.mli
	${ELIOMC} -c ${SERVER_INC} $<

${ELIOM_SERVER_DIR}/%.cmo: %.ml
	${ELIOMC} -c ${SERVER_INC} $<
${ELIOM_SERVER_DIR}/%.cmo: %.eliom
	${ELIOMC} -c -noinfer ${SERVER_INC} $<

${ELIOM_SERVER_DIR}/%.cmx: %.ml
	${ELIOMOPT} -c ${SERVER_INC} $<
${ELIOM_SERVER_DIR}/%.cmx: %.eliom
	${ELIOMOPT} -c -noinfer ${SERVER_INC} $<

%.cmxs: %.cmxa
	$(ELIOMOPT) -shared -linkall -o $@ $<

##### Client side compilation ####

CLIENT_LIBS := ${addprefix -package ,${CLIENT_PACKAGES}}
CLIENT_INC  := ${addprefix -package ,${CLIENT_PACKAGES}}

CLIENT_OBJS := $(patsubst %.eliom,${ELIOM_CLIENT_DIR}/%.cmo, ${CLIENT_FILES})
CLIENT_OBJS := $(patsubst %.ml,${ELIOM_CLIENT_DIR}/%.cmo, ${CLIENT_OBJS})

${PROJECT_NAME}.js: ${CLIENT_OBJS}
	${JS_OF_ELIOM} -o $@ ${CLIENT_LIBS} $^

${ELIOM_CLIENT_DIR}/%.cmi: %.mli
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<

${ELIOM_CLIENT_DIR}/%.cmo: %.eliom
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<
${ELIOM_CLIENT_DIR}/%.cmo: %.ml
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<

############

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f *.type_mli
	-rm -f ${PROJECT_NAME}.js
	-rm -rf ${ELIOM_CLIENT_DIR} ${ELIOM_SERVER_DIR}

distclean: clean.local
	-rm -f *~ \#* .\#*

## Dependencies

depend: .depend
.depend: ${SERVER_FILES} ${CLIENT_FILES}
	$(ELIOMDEP) -server ${SERVER_INC} ${SERVER_FILES} > .depend
	$(ELIOMDEP) -client ${CLIENT_INC} ${CLIENT_FILES} >> .depend

## Warning: Dependencies towards *.eliom are not handled by eliomdep yet.

${PROJECT_NAME}.conf: .PROJECT.conf.in
	sed \
	  -e "s|%%LIBDIR%%|${LIBDIR}|" \
	  -e "s|%%STATICDIR%%|${STATICDIR}|" \
	  -e "s|%%CMDPIPE%%|${CMDPIPE}|" \
	  -e "s|%%PORT%%|${PORT}|" \
	  -e "s|%%DATADIR%%|${DATADIR}|" \
	  -e "s|%%LOGDIR%%|${LOGDIR}|" \
	  -e "s|%%PROJECT_NAME%%|${PROJECT_NAME}|" \
	  $< > $@

${PROJECT_NAME}.eliom: .PROJECT.eliom.in
	sed \
	  -e "s|%%PROJECT_NAME%%|${PROJECT_NAME}|" \
	  -e "s|%%MODULE_NAME%%|${shell echo ${PROJECT_NAME}|sed 's/^./\u&/;'}|" \
	  $< > $@

static/${PROJECT_NAME}.css: static/.PROJECT.css
	cp $< $@

PHONY: start install run

start: ${PROJECT_NAME}.eliom static/${PROJECT_NAME}.css ${PROJECT_NAME}.conf

install: byte
	mkdir -p ${DATADIR}
	mkdir -p ${LIBDIR}
	mkdir -p ${STATICDIR}
	mkdir -p ${LOGDIR}
	mkdir -p ${shell dirname ${CMDPIPE}}
	cp ${PROJECT_NAME}.cma ${LIBDIR}
	cp ${PROJECT_NAME}.js ${STATICDIR}
	cp -r static/* ${STATICDIR}

run: ${PROJECT_NAME}.conf
	ocsigenserver -c $<

include .depend
