NAME	=	trainmanager

ML	=	src/train.ml	\
		src/data.ml     \
		src/entry.ml	\
		src/trip.ml	\
		src/main.ml	\

MLI 	=	inc/train.mli	\
		inc/data.mli	\
		inc/entry.mli	\
		inc/trip.mli	\

CMI 	= 	$(MLI:.mli=.cmi)
CMO 	= 	$(ML:.ml=.cmo)
CMX	= 	$(ML:.ml=.cmx)

OCAMLDPE	= 	ocamldep
CAMLFLAGS 	= 	-w Aelz -warn-error A
OCAMLC 		=	ocamlc $(CAMLFLAGS)
OCAMLOPT	=	ocamlopt $(CAMLFLAGS) str.cmxa

all:		.depend  $(CMI) $(NAME)

$(NAME):	$(CMX)
		@$(OCAMLOPT) -o $@ $(CMX)
		@echo "[OK] $(NAME) linked"

$(NAME).byte:	$(CMO)
		@$(OCAMLC) -o $@ $(CMO)
		@echo "[OK] $(NAME).byte linked"

%.cmx:		%.ml
		@$(OCAMLOPT) -c $<
		@echo "[OK] [$<] builded"

%.cmo:		%.ml
		@$(OCAMLC) -c $<
		@echo "[OK] [$<] builded"

%.cmi:		%.mli
		@$(OCAMLC) -c $<
		@echo "[OK] [$<] builded"

re:		fclean all


clean:
		@/bin/rm -f src/*.cm* src/*.o inc/*.cm* inc/*.o .depend
		@echo "[OK] clean"


fclean: 	clean
		@/bin/rm -f $(NAME) $(NAME).byte
		@echo "[OK] fclean"

.depend:
		@/bin/rm -f .depend
		@$(OCAMLDPE) $(MLI) $(ML) > .depend
		@echo "[OK] dependencies"
