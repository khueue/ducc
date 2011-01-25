DIR_EBIN   = ebin
DIR_LEXER  = lexer
DIR_PARSER = parser

PROJECT_NAME = ducc
LEXER_NAME   = lexer
PARSER_NAME  = parser

ERLC_FLAGS = -Wall -Ddebug
ERLC       = erlc -o $(DIR_EBIN) $(ERLC_FLAGS)
ERL        = erl -pa ebin -noshell

all: setup clean ass1 ass2 main

setup:
	mkdir -p $(DIR_EBIN)
	@- ruby trim_and_clean.rb *.erl **/*.xrl **/*.yrl

clean:
	rm -rf $(DIR_LEXER)/$(LEXER_NAME).erl
	rm -rf $(DIR_PARSER)/$(PARSER_NAME).erl
	rm -rf $(DIR_EBIN)/*.beam
	rm -rf *.dump *.gz *.beam

pack:
	tar -czvf $(PROJECT_NAME).tar.gz \
		$(DIR_EBIN) $(DIR_LEXER) $(DIR_PARSER) report suite \
		Makefile *.erl testfil

ass1: 
	clear
	@- echo '--- Generating the lexer ...'
	$(ERL) -eval 'io:format("~p~n", [leex:file("$(DIR_LEXER)/$(LEXER_NAME)")]), halt().'

	@- echo '--- Compiling lexer ...'
	$(ERLC) $(DIR_LEXER)/$(LEXER_NAME).erl

ass2:
	@- echo '--- Generating the parser ...'
	$(ERL) -eval 'io:format("~p~n", [yecc:file("$(DIR_PARSER)/$(PARSER_NAME)")]), halt().'

	@- echo '--- Compiling parser ...'
	$(ERLC) $(DIR_PARSER)/$(PARSER_NAME).erl

main:
	@- echo '--- Compiling main ...'
	$(ERLC) main.erl

	@- echo '--- Testing lexer and parser ...'
	$(ERL) -run main start testfil2.c

test: ass1
	$(ERL) 'ass1_test:test(), halt().'
