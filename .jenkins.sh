opam pin add --no-action tuto .

do_build_doc () {
  cp -Rf files/* ${MANUAL_FILES_DIR}/
  cp -Rf src/* ${MANUAL_SRC_DIR}/
}

do_remove () {
    echo "nothing to remove"
}
