
## Sample Makefile for eliom application.

LIB_NAME     := chat
APP_NAME     := chat_site
MAIN_FILE    := chat_site.eliom

-include Makefile.local

INSTALLDIR   ?= $(shell pwd)/local
VARDIR       ?= $(shell pwd)/local/var

PORT         ?= 8080
STATICDIR    ?= ${INSTALLDIR}/static
LIBDIR       ?= ${INSTALLDIR}/lib
LOGDIR       ?= ${VARDIR}/log
RUNDIR       ?= ${VARDIR}/run
COMMAND_PIPE ?= $(RUNDIR)/ocsigenserver_command
CONFDIR      ?= ${INSTALLDIR}

SERVER_LIB   := $(LIB_NAME)_server
CLIENT_LIB   := $(LIB_NAME)_client

## Packages required to build the server part of the application

SERVER_PACKAGES :=

## Packages to be linked in the client part

CLIENT_PACKAGES :=

## Source files for the server part

SERVER_FILES := utils.ml shared.ml widgets.eliom user_management.ml chat.eliom chat_site.eliom

## Source files for the client part

CLIENT_FILES := utils.ml shared.ml widgets.eliom client.ml chat.eliom chat_site.eliom

## Where to put intermediate object files.
## - ELIOM_{SERVER,CLIENT}_DIR must be distinct
## - ELIOM_CLIENT_DIR mustn't be the local dir.
## - ELIOM_SERVER_DIR could be ".", but you need to
##   remove it from the "clean" rules...

export ELIOM_SERVER_DIR := _server
export ELIOM_CLIENT_DIR := _client
export ELIOM_TYPE_DIR   := .

SERVER_INC  := ${addprefix -package ,${SERVER_PACKAGE}}
SERVER_OBJS := $(patsubst %.eliom,${ELIOM_SERVER_DIR}/%.cmo, ${SERVER_FILES})
SERVER_OBJS := $(patsubst %.ml,${ELIOM_SERVER_DIR}/%.cmo, ${SERVER_OBJS})

CLIENT_LIBS := ${addprefix -package ,${CLIENT_PACKAGE}}
CLIENT_INC  := ${addprefix -package ,${CLIENT_PACKAGE}}

CLIENT_OBJS := $(patsubst %.eliom,${ELIOM_CLIENT_DIR}/%.cmo, ${CLIENT_FILES})
CLIENT_OBJS := $(patsubst %.ml,${ELIOM_CLIENT_DIR}/%.cmo, ${CLIENT_OBJS})

## Required binaries

ELIOMC      := eliomc
ELIOMOPT    := eliomopt
ELIOMDEP    := eliomdep
JS_OF_ELIOM := js_of_eliom

#####################################

all: byte opt

libraries.byte: ${SERVER_LIB}.cmo ${CLIENT_LIB}.cmo
libraries.opt: ${SERVER_LIB}.cmx ${CLIENT_LIB}.cmo

$(LOGDIR):
	mkdir -p $(LOGDIR)

$(RUNDIR):
	mkdir -p $(RUNDIR)

run.byte: byte install.byte $(LOGDIR) $(RUNDIR)
	ocsigenserver ${SERVER_PARAM} -c $(CONFDIR)/$(APP_NAME).conf
run.opt: opt install.opt $(LOGDIR) $(RUNDIR)
	ocsigenserver.opt ${SERVER_PARAM} -c $(CONFDIR)/$(APP_NAME).conf

restart: all
	@if [ -e $(COMMAND_PIPE) ]; then; echo reload >  $(COMMAND_PIPE); fi

archive:
	git archive -o ochat-$(shell date +%F).tar.gz master

byte:: ${APP_NAME}.cma ${APP_NAME}.js libraries.byte
opt:: ${APP_NAME}.cmxa ${APP_NAME}.cmxs ${APP_NAME}.js libraries.opt

#### Libraries compilation ####

$(SERVER_LIB).cmo: $(filter-out $(patsubst %.eliom, $(ELIOM_SERVER_DIR)/%.cmo, $(MAIN_FILE)), $(SERVER_OBJS))
	ocamlc -pack -o $@ $^
$(CLIENT_LIB).cmo: $(filter-out $(patsubst %.eliom, $(ELIOM_CLIENT_DIR)/%.cmo, $(MAIN_FILE)), $(CLIENT_OBJS))
	ocamlc -pack -o $@ $^

$(SERVER_LIB).cmx: $(filter-out $(patsubst %.eliom, $(ELIOM_SERVER_DIR)/%.cmx, $(MAIN_FILE)), $(patsubst %.cmo, %.cmx, $(SERVER_OBJS)))
	ocamlopt -pack -o $@ $^
$(CLIENT_LIB).cmx: $(filter-out $(patsubst %.eliom, $(ELIOM_CLIENT_DIR)/%.cmx, $(MAIN_FILE)), $(patsubst %.cmo, %.cmx, $(CLIENT_OBJS)))
	ocamlopt -pack -o $@ $^

#### Server side compilation ######

${APP_NAME}.cma: ${SERVER_OBJS}
	${ELIOMC} ${ELIOMCFLAGS} -a -o $@ $^
${APP_NAME}.cmxa: ${SERVER_OBJS:.cmo=.cmx}
	${ELIOMOPT} ${ELIOMOPTFLAGS} -a -o $@ $^

${ELIOM_TYPE_DIR}/%.type_mli: %.eliom
	${ELIOMC} ${ELIOMCFLAGS} -infer ${SERVER_INC} $<

${ELIOM_SERVER_DIR}/%.cmi: %.mli
	${ELIOMC} ${ELIOMCFLAGS} -c ${SERVER_INC} $<

${ELIOM_SERVER_DIR}/%.cmo: %.ml
	${ELIOMC} ${ELIOMCFLAGS} -c ${SERVER_INC} $<
${ELIOM_SERVER_DIR}/%.cmo: %.eliom
	${ELIOMC} ${ELIOMCFLAGS} -c -noinfer ${SERVER_INC} $<

${ELIOM_SERVER_DIR}/%.cmx: %.ml
	${ELIOMOPT} ${ELIOMOPTFLAGS} -for-pack Chat_server -c ${SERVER_INC} $<
${ELIOM_SERVER_DIR}/%.cmx: %.eliom
	${ELIOMOPT} ${ELIOMOPTFLAGS} -for-pack Chat_server -c -noinfer ${SERVER_INC} $<

%.cmxs: %.cmxa
	$(ELIOMOPT) ${ELIOMOPTFLAGS} -shared -linkall -o $@ $<

##### Client side compilation ####

${APP_NAME}.js: ${CLIENT_OBJS}
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
	-rm -f ${APP_NAME}.js
	-rm -rf ${ELIOM_CLIENT_DIR} ${ELIOM_SERVER_DIR}
	-rm -rf ${STATICDIR} ${LIBDIR} ${RUNDIR}

distclean: clean
	-rm -f *~ \#* .\#*
	-rm -f .depend $(APP_NAME).conf
	-rm -rf ${INSTALLDIR}

## Dependencies

depend: .depend
.depend: ${SERVER_FILES} ${CLIENT_FILES}
	$(ELIOMDEP) -server ${SERVER_INC} ${SERVER_FILES} > .depend
	$(ELIOMDEP) -client ${CLIENT_INC} ${CLIENT_FILES} >> .depend

## Warning: Dependencies towards *.eliom are not handled by eliomdep yet.
-include .depend
_server/chat_site.cmo: _server/chat.cmo
_client/chat_site.cmo: _client/chat.cmo
_client/client.cmo: _client/widgets.cmo
_client/chat.cmo: _client/widgets.cmo
_server/chat.cmo: _server/widgets.cmo

## installation #########

install: install.byte install.opt

${APP_NAME}.conf: ${APP_NAME}.conf.in
	sed -e "s|%%SRC%%|$(shell pwd)|" \
	    -e "s|%%LIBDIR%%|${LIBDIR}|" \
	    -e "s|%%STATICDIR%%|${STATICDIR}|" \
	    -e "s|%%CMDPIPEPATH%%|${COMMAND_PIPE}|" \
	    -e "s|%%PORT%%|${PORT}|" \
	    -e "s|%%LOGDIR%%|${LOGDIR}|" \
	    -e "s|%%DATADIR%%|${RUNDIR}|" \
	    -e "s|%%APPNAME%%|${APP_NAME}|" \
	    $< > $@

$(STATICDIR):
	mkdir -p $@

$(LIBDIR):
	mkdir -p $@

install.byte: byte $(STATICDIR) $(LIBDIR) $(LOGDIR) $(RUNDIR) $(APP_NAME).conf
	install -m 664 $(INSTALL_USER) $(APP_NAME).js $(STATICDIR)
	install -m 664 $(INSTALL_USER) static/* $(STATICDIR)
	install -m 664 $(INSTALL_USER) $(APP_NAME).cma $(LIBDIR)
	install -m 664 $(INSTALL_USER) $(APP_NAME).conf $(CONFDIR)

install.opt: opt $(STATICDIR) $(LIBDIR) $(LOGDIR) $(RUNDIR) $(APP_NAME).conf
	install -m 664 $(INSTALL_USER) $(APP_NAME).js $(STATICDIR)
	install -m 664 $(INSTALL_USER) static/* $(STATICDIR)
	install -m 664 $(INSTALL_USER) $(APP_NAME).cmxa $(LIBDIR)
	install -m 664 $(INSTALL_USER) $(APP_NAME).cmxs $(LIBDIR)
	install -m 664 $(INSTALL_USER) $(APP_NAME).conf $(CONFDIR)


