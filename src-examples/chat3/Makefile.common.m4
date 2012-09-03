changecom('/*','*/')dnl
#################################### Variables ##################################
ELIOMC           := eliomc
ELIOMOPT         := eliomopt
ELIOMDEP         := eliomdep
JS_OF_ELIOM      := js_of_eliom
OCLOSURE_REQ     := oclosure_req

FILES		 := ${wildcard *.eliom} $(wildcard *.ml)

SERVER_FILES     := ${wildcard *.eliom} $(wildcard *.ml)

CLIENT_FILES	 := ${wildcard *.eliom}

DEP_FILES	 := $(patsubst %, \
	${DEP_DIR}/%.d, \
	$(basename ${SERVER_FILES}) $(basename ${CLIENT_FILES}))

SERVER_INC       := $(addprefix -package , ${SERVER_PACKAGE})

CLIENT_INC       := $(addprefix -package , ${CLIENT_PACKAGE})

SERVER_OBJS	 := $(patsubst %, \
	${SERVER_DIR}/%.cmo, \
	$(basename ${SERVER_FILES}))

CLIENT_OBJS	 := $(patsubst %, \
	${CLIENT_DIR}/%.cmo, \
	$(basename ${CLIENT_FILES}))


################################# Compilation ###################################
ifeq (${OPT},true)
all:: opt
else
all:: byte
endif

byte:: ${DATA_DIR}/appName.cma

opt:: ${DATA_DIR}/appName.cmxs


######################################### Libraries
${DATA_DIR}/appName.cma:: ${SERVER_OBJS} ${DEP_DIR}/sortedServerCmo
	$(eval SORTED_OBJS:=$(shell cat ${DEP_DIR}/sortedServerCmo))
	${ELIOMC} -a -o $@ ${SORTED_OBJS}

${DATA_DIR}/appName.cmxs: ${DATA_DIR}/appName.cmxa
	$(ELIOMOPT) ${ELIOMOPTFLAGS} -shared -linkall -o $@ $<

${DATA_DIR}/appName.cmxa:: ${SERVER_OBJS:.cmo=.cmx} ${DEP_DIR}/sortedServerCmo
	$(eval SORTED_OBJS:=$(shell cat ${DEP_DIR}/sortedServerCmo))
	${ELIOMOPT} -a -o $@ ${SORTED_OBJS:.cmo=.cmx}

${STATIC_DIR}/appName.js: ${CLIENT_OBJS} ${DEP_DIR}/sortedClientCmo
	$(eval SORTED_OBJS:=$(shell cat ${DEP_DIR}/sortedClientCmo))
	${JS_OF_ELIOM} -jsopt -pretty -jsopt -noinline -o $@ ${CLIENT_INC} \
	${SORTED_OBJS}

ifeq (${CLIENT},true)
byte:: ${STATIC_DIR}/appName.js
opt:: ${STATIC_DIR}/appName.js
endif

ifeq (${OCLOSURE},true)
byte:: ${STATIC_DIR}/appName`'_oclosure.js
opt:: ${STATIC_DIR}/appName`'_oclosure.js
${STATIC_DIR}/appName`'_oclosure.js: ${STATIC_DIR}/appName.js
	${OCLOSURE_REQ} $^
endif

######################################### Dependencies
-include ${DEP_FILES}

eliomdepCall=${ELIOMDEP} \
		-$(shell echo $(2) | tr "[:upper:]" "[:lower:]") \
		-dir ${$(2)_DIR} \
                -modules ${SERVER_INC} ${CLIENT_INC} $(1).$(3)

get_Modules=$(shell \
	$(call eliomdepCall,$(1),$(2),$(3)) \
	| cut -d' ' -f2- | sed 's|${$(2)_DIR}/||g')

get_modules=$(shell \
	for module in $(call get_Modules,$(1),$(2),$(3)); do \
	echo $$(echo $${module} | cut -c1 | tr "[:upper:]" "[:lower:]")$\
	$$(echo $${module} | cut -c2-);\
	done)

filterDep=$(filter $(call get_modules,$(1),$(2),$(3)), $(basename ${FILES}))

dependencies= \
	"${$(2)_DIR}/$(1).cmo: \
	 $(addsuffix .cmo, \
		$(addprefix ${$(2)_DIR}/, \
			$(call filterDep,$(1),$(2),$(3))))\n$\
	 ${$(2)_DIR}/$(1).cmx: \
	 $(addsuffix .cmx, \
		$(addprefix ${$(2)_DIR}/, \
			$(call filterDep,$(1),$(2),$(3))))"

# Vouillon parry
${DEP_DIR}/preemptive:
	mkdir -p ${DEP_DIR}
	touch $@

${DEP_DIR}/%.d: %.ml ${DEP_DIR}/preemptive
	@echo $(call eliomdepCall,$*,SERVER,ml)
	@echo $(call dependencies,$*,SERVER,ml) > $@
	@echo $(call eliomdepCall,$*,CLIENT,ml)
	@echo $(call dependencies,$*,CLIENT,ml) >> $@

${DEP_DIR}/%.d: %.eliom ${DEP_DIR}/preemptive
	@echo $(call eliomdepCall,$*,SERVER,eliom)
	@echo $(call dependencies,$*,SERVER,eliom) > $@
	@echo $(call eliomdepCall,$*,CLIENT,eliom)
	@echo $(call dependencies,$*,CLIENT,eliom) >> $@
	@echo "${SERVER_DIR}/$*.type_mli: $<" >> $@
	@echo "${CLIENT_DIR}/$*.cmo: ${SERVER_DIR}/$*.type_mli" >> $@
	@echo "${CLIENT_DIR}/$*.cmx: ${SERVER_DIR}/$*.type_mli" >> $@

${DEP_DIR}/sortedServerCmo: ${SERVER_FILES}
	@cat ${DEP_DIR}/*.d | grep -v type_mli | grep -v ${CLIENT_DIR} | \
	sed 's|${SERVER_DIR}/||g' | \
	ocamldsort -byte ${SERVER_FILES:.eliom=.ml} | \
	sed 's&\(^\| \)& ${SERVER_DIR}/&g' > $@

${DEP_DIR}/sortedClientCmo: ${CLIENT_FILES}
	$(eval CLIENT_OBJS:=\
	$(shell find ${CLIENT_DIR} -name "*.cmo" -exec basename {} \;))
	@cat ${DEP_DIR}/*.d | grep -v type_mli | grep -v ${SERVER_DIR} | \
	sed 's|${CLIENT_DIR}/||g' | \
	ocamldsort -byte ${CLIENT_OBJS:.cmo=.ml} | \
	sed 's&\(^\| \)& ${CLIENT_DIR}/&g' > $@


######################################### Server side
${SERVER_DIR}/%.cmo: %.ml
	${ELIOMC} -dir ${SERVER_DIR} -c ${SERVER_INC} $<

${SERVER_DIR}/%.cmo: %.eliom
	${ELIOMC} -dir ${SERVER_DIR} -c -noinfer ${SERVER_INC} $<

${SERVER_DIR}/%.cmx: %.ml
	${ELIOMOPT} -dir ${SERVER_DIR} -c ${SERVER_INC} $<

${SERVER_DIR}/%.cmx: %.eliom
	${ELIOMOPT} -dir ${SERVER_DIR} -c -noinfer ${SERVER_INC} $<


######################################### Client side
${SERVER_DIR}/%.type_mli: %.eliom
	${ELIOMC} -dir ${SERVER_DIR} -infer ${SERVER_INC} $<

${CLIENT_DIR}/%.cmo: %.eliom
	${JS_OF_ELIOM} -dir ${CLIENT_DIR} -c ${CLIENT_INC} $<

${CLIENT_DIR}/%.cmo: %.ml
	${JS_OF_ELIOM} -dir ${CLIENT_DIR} -c ${CLIENT_INC} $<


#################################### Clean ######################################
clean:
	-rm -rf ${SERVER_DIR}
	-rm -rf ${CLIENT_DIR}
	-rm -f  ${STATIC_DIR}/appName.js
	-rm -f  ${DATA_DIR}/*
	-rm -f  ${LOG_DIR}/*
	-rm -f  ${RUN_DIR}/*

distclean: clean
	-rm -rf ${DEP_DIR}
	-find . -name "*~" -exec rm -v {} \;
