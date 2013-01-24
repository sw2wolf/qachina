########
# ocamlfind ocamlc -verbose -package str,ocsigen,postgresql,mysql -linkpkg


LAPTOP_FLAGS= -I /usr/lib/ocaml/pcre       -ccopt -I/usr/lib/ocaml/pcre       \
              -I /usr/lib/ocaml/netsys     -ccopt -I/usr/lib/ocaml/netsys     \
			  -I /usr/lib/ocaml/netstring  -ccopt -I/usr/lib/ocaml/netstring  \
			  -I /usr/lib/ocaml/ssl        -ccopt -I/usr/lib/ocaml/ssl        \
			  -I /usr/lib/ocaml/lwt        -ccopt -I/usr/lib/ocaml/lwt        \
			  -I /usr/lib/ocaml/ocsigen    -ccopt -I/usr/lib/ocaml/ocsigen    \
			  -I /usr/lib/ocaml/postgresql -ccopt -I/usr/lib/ocaml/postgresql \
			  -I /usr/lib/ocaml/mysql      -ccopt -I/usr/lib/ocaml/mysql      \
			  -ccopt -L/usr/lib/ocaml/pcre                                    \
			  -ccopt -L/usr/lib/ocaml/netsys                                  \
			  -ccopt -L/usr/lib/ocaml/netstring                               \
		      -ccopt -L/usr/lib/ocaml/ssl                                     \
			  -ccopt -L/usr/lib/ocaml/lwt                                     \
			  -ccopt -L/usr/lib/ocaml/ocsigen                                 \
			  -ccopt -L/usr/lib/ocaml/postgresql                              \
		      -ccopt -L/usr/lib/ocaml/mysql                                   \
			  /usr/lib/ocaml/unix.cma                                         \
			  /usr/lib/ocaml/threads/threads.cma                              \
			  /usr/lib/ocaml/str.cma                                          \
			  /usr/lib/ocaml/pcre/pcre.cma                                    \
			  /usr/lib/ocaml/netsys/netsys.cma                                \
			  /usr/lib/ocaml/netstring/netstring.cma                          \
		      /usr/lib/ocaml/netstring/netstring_mt.cmo                       \
			  /usr/lib/ocaml/netstring/netaccel.cma                           \
			  /usr/lib/ocaml/netstring/netaccel_link.cmo                      \
			  /usr/lib/ocaml/ssl/ssl_threads.cma                              \
			  /usr/lib/ocaml/dynlink.cma                                      \
			  /usr/lib/ocaml/lwt/lwt.cma                                      \
			  /usr/lib/ocaml/postgresql/postgresql.cma                        \
			  /usr/lib/ocaml/mysql/mysql.cma

OCAMLC = ocamlc -c -thread
gs: 
	$(OCAMLC) $(LAPTOP_FLAGS) gs.ml
index:  gs 
	$(OCAMLC) $(LAPTOP_FLAGS) gs.cmo index.ml
all:  gs index 
	echo "all OK"
