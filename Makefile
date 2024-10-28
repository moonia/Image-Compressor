BIN_NAME=imageCompressor-exe

NAME=imageCompressor

PATH_BIN=$(shell stack path --local-install-root)/bin/$(BIN_NAME)

all: $(NAME)

$(NAME):
	stack build
	cp $(PATH_BIN) $(NAME)

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)

tests_run:
	stack test --coverage
	stack hpc report --all --destdir test/coverage

re: fclean all

.PHONY: fclean all re tests_run
