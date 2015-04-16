SUBDIRS = asm vm

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean:
	$(MAKE) clean -C asm;
	$(MAKE) clean -C vm;

fclean: clean
	rm -rf bin/*

re: fclean all

.PHONY: asm vm clean fclean re
