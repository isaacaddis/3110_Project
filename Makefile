MODULES=card deck player state main serve user
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
CLIENT=user.byte
SERVER=serve.byte
OCAMLBUILD=corebuild -cflag -thread -use-ocamlfind -pkgs cohttp-lwt-unix,yojson,str
PKGS=unix,oUnit,str,cohttp-lwt-unix,lwt,cohttp,yojson

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

server:
	$(OCAMLBUILD) $(SERVER) && ./$(SERVER)

client:
	$(OCAMLBUILD) $(CLIENT) && ./$(CLIENT)

test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

reset:
	printf "{\n\t\"money\": 500\n}" > stats.json

check:
	bash checkenv.sh && bash checktypes.sh

finalcheck: check
	bash checkzip.sh
	bash finalcheck.sh

bisect: clean test
	bisect-ppx-report -I _build -html report bisect0001.out

zip:
	zip src.zip *.ml* _tags Makefile INSTALL.txt

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report search_src.zip bisect*.out
