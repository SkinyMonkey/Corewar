GHC = ghc

NAME = asm

BIN_DIR = ../bin

INTERMEDIATE_DIR = ../tmp/intermediates

COMMON_SRCS = ../common/header.hs\
							../common/op.hs\
							../common/utils.hs

SRCS =	asm.hs\
				championData.hs\
				checkArgs.hs\
				codeGeneration.hs\
				parseAsm.hs\
				parseBase.hs

all: $(NAME)

test:
	@echo "FIXME : NOT IMPLEMENTED"

$(NAME): $(COMMON_SRCS) $(SRCS)
	"$(GHC)" -i../common:. -outputdir $(INTERMEDIATE_DIR) -O -XHaskell2010 --make $@
	mv $(NAME) $(BIN_DIR)

clean:
	rm -f $(INTERMEDIATE_DIR)/*.o
	rm -f $(INTERMEDIATE_DIR)/*.hi

fclean:
	rm -f $(BIN_DIR)/$(NAME)

re: fclean all

.PHONY: clean fclean re $(NAME)
