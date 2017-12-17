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
#				codeGeneration.hs\
				parseAsm.hs\
				parseBase.hs

FLAGS = -O -W -Wall

all: $(NAME)

test:
	@echo "FIXME : NOT IMPLEMENTED"

$(NAME): $(COMMON_SRCS) $(SRCS)
	"$(GHC)" -i../common:. -outputdir $(INTERMEDIATE_DIR) $(FLAGS) $@
	mv $(NAME) $(BIN_DIR)

debug:
	@echo "HELP : To debug an exception use :args to set the program argument and :back\n"
	ghci $(SRCS) $(COMMON_SRCS)

clean:
	rm -f $(INTERMEDIATE_DIR)/*.o
	rm -f $(INTERMEDIATE_DIR)/*.hi

fclean: clean
	rm -f $(BIN_DIR)/$(NAME)

re: fclean all

.PHONY: clean fclean re $(NAME)
