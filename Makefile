DIR_EBIN   = ebin
DIR_SRC    = src
DIR_LEXER  = $(DIR_SRC)/lexer
DIR_PARSER = $(DIR_SRC)/parser

PROJECT_NAME = ducc
LEXER_NAME   = lexer
PARSER_NAME  = parser
SCRIPTS      = lexer parser

ERLC_FLAGS = -Wall -Ddebug
ERLC       = erlc -o $(DIR_EBIN) $(ERLC_FLAGS)
ERL        = erl -pa $(DIR_EBIN) -noshell

all: setup clean compile

setup:
	mkdir -p $(DIR_EBIN)
	@- ruby src/trim_and_clean.rb src/**/* $(SCRIPTS)

clean:
	rm -rf $(DIR_LEXER)/$(LEXER_NAME).erl
	rm -rf $(DIR_PARSER)/$(PARSER_NAME).erl
	rm -rf $(DIR_EBIN)/*.beam
	rm -rf *.dump *.gz *.beam

pack:
	tar -czvf $(PROJECT_NAME).tar.gz \
		$(DIR_EBIN) $(DIR_SRC) report suite Makefile

compile: 
	clear
	@- echo '--- Generating lexer and parser ...'
	$(ERL) -eval 'leex:file("$(DIR_LEXER)/$(LEXER_NAME)"), halt().'
	$(ERL) -eval 'yecc:file("$(DIR_PARSER)/$(PARSER_NAME)"), halt().'
	@- echo '--- Compiling ...'
	$(ERLC) $(DIR_LEXER)/*.erl
	$(ERLC) $(DIR_PARSER)/*.erl
	$(ERLC) $(DIR_SRC)/*.erl
