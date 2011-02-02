DIR_EBIN   = ebin
DIR_SRC    = src
DIR_LEXER  = $(DIR_SRC)/lexer
DIR_PARSER = $(DIR_SRC)/parser

PROJECT_NAME = ducc
LEXER_NAME   = lexer
PARSER_NAME  = parser
SCRIPTS      = analyzer analyzer_test ducc lexer lexer_test parser parser_test

ERLC_FLAGS = -Wall -Ddebug
ERLC       = erlc -o $(DIR_EBIN) $(ERLC_FLAGS)
ERL        = erl -pa $(DIR_EBIN) -noshell

all: setup clean compile

setup:
	mkdir -p $(DIR_EBIN)
	@- ruby src/trim_and_clean.rb $(DIR_SRC)/*.* $(DIR_SRC)/**/*.* \
		report/**/*.md $(SCRIPTS)

clean:
	rm -rf $(DIR_LEXER)/$(LEXER_NAME).erl
	rm -rf $(DIR_PARSER)/$(PARSER_NAME).erl
	rm -rf $(DIR_EBIN)/*.beam
	rm -rf *.dump *.gz *.beam

compile:
	clear
	@- echo '--- Generating lexer and parser ...'
	$(ERL) -eval 'io:format("~p~n",[leex:file("$(DIR_LEXER)/$(LEXER_NAME)")]), halt().'
	$(ERL) -eval 'io:format("~p~n",[yecc:file("$(DIR_PARSER)/$(PARSER_NAME)")]), halt().'
	@- echo '--- Compiling ...'
	$(ERLC) $(DIR_SRC)/*.erl
	$(ERLC) $(DIR_SRC)/**/*.erl

gen_tests: all
	src/gen_lexer_tests.rb suite/**/*.c
	src/gen_parser_tests.rb suite/**/*.c
	# analyzer generator, sometime

tests: all
	@- echo '--- Running tests ...'
	lexer_test
	parser_test
	analyzer_test

pack:
	tar -czvf $(PROJECT_NAME)-`date +"%Y-%m-%d"`.tar.gz \
		$(DIR_EBIN) $(DIR_SRC) report suite \
		$(SCRIPTS) Makefile *.md
