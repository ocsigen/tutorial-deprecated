
test:
	./self-test

.PHONY: dist
DIST_DIR=eliom-destillery-${shell date +%F}
dist: 
	mkdir ${DIST_DIR}
	cp -r README eliom-destillery template ${DIST_DIR}
	tar czfv ${DIST_DIR}.tar.gz ${DIST_DIR}
	rm -rf ${DIST_DIR}

