<ocsigen>
  <server>
    <port>PORT</port>
    <logdir>LOG_DIR</logdir>
    <datadir>DATA_DIR</datadir>
    <commandpipe>COMMAND_PIPE</commandpipe>
    <charset>utf-8</charset>
    `<debugmode/>' <!-- Only to use during development -->
    <extension findlib-package="ocsigenserver.ext.staticmod"/>
    <extension findlib-package="ocsigenserver.ext.extendconfiguration"/>
    <extension findlib-package="ocsigenserver.ext.userconf"/>
    <extension findlib-package="ocsigenserver.ext.ocsipersist-sqlite"/>
    <extension findlib-package="eliom.server"/>
    <!-- Uncomment to use macaque as a database backend -->
    <!-- <extension findlib-package="macaque"/> -->
    <host hostfilter="*">
      <static dir="STATIC_DIR" />
      <eliom module="DATA_DIR/appName.cma" />
    </host>
  </server>
</ocsigen>

