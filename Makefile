DIR_EBIN   = ebin
DIR_SRC    = src
DIR_LEXER  = $(DIR_SRC)/lexer
DIR_PARSER = $(DIR_SRC)/parser
DIR_LIB    = lib

PROJECT_NAME = ducc
LEXER_NAME   = lexer
PARSER_NAME  = parser

ERLC_FLAGS = -Wall -Ddebug
ERLC       = erlc -o $(DIR_EBIN) $(ERLC_FLAGS)
ERL        = erl -pa ebin -noshell

all: setup clean compile

setup:
	mkdir -p $(DIR_EBIN)
	@- ruby lib/trim_and_clean.rb **/*.xrl **/*.yrl lib/*

clean:
	$(RM) $(DIR_LEXER)/$(LEXER_NAME).erl
	$(RM) $(DIR_PARSER)/$(PARSER_NAME).erl
	$(RM) $(DIR_EBIN)/*.beam
	$(RM) *.dump *.gz *.beam

pack:
	tar -czvf $(PROJECT_NAME).tar.gz \
		$(DIR_EBIN) $(DIR_SRC) report suite \
		Makefile

compile: 
	clear
	@- echo '--- Generating lexer and parser ...'
	$(ERL) -eval 'leex:file("$(DIR_LEXER)/$(LEXER_NAME)"), halt().'
	$(ERL) -eval 'yecc:file("$(DIR_PARSER)/$(PARSER_NAME)"), halt().'
	@- echo '--- Compiling ...'
	$(ERLC) $(DIR_LEXER)/*.erl
	$(ERLC) $(DIR_PARSER)/*.erl
	$(ERLC) $(DIR_LIB)/*.erl
