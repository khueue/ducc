DIR_EBIN   = ebin
DIR_LEXER  = lexer
DIR_PARSER = parser

PROJECT_NAME = ducc
LEXER_NAME   = lexer
PARSER_NAME  = parser

ERLC_FLAGS = -Wall -Ddebug
ERLC       = erlc -o $(DIR_EBIN) $(ERLC_FLAGS)
ERL        = erl -pa ebin -noshell

all: clean ass1

setup:
	mkdir -p $(DIR_EBIN)
	@- ruby trim_and_clean.rb *.erl

clean:
	rm -rf  $(DIR_LEXER)/$(LEXER_NAME).erl
	rm -rf  $(DIR_EBIN)/*.beam
	rm -rf *.dump

pack:
	tar -czvf $(PROJECT_NAME).tar.gz \
		$(DIR_EBIN) $(DIR_LEXER) report suite \
		Makefile *.erl testfil

ass1: setup
	clear
	@- echo '--- Generating the lexer ...'
	$(ERL) -eval 'leex:file("$(DIR_LEXER)/$(LEXER_NAME)"), halt().'

	@- echo '--- Compiling lexer ...'
	$(ERLC) $(DIR_LEXER)/$(LEXER_NAME).erl

	@- echo '--- Compiling main ...'
	$(ERLC) main.erl

	@- echo '--- Testing lexer ...'
	$(ERL) -run main start testfil

# XXX
ass2:
	$(ERL) 'yecc:file("$(DIR_PARSER)/$(PARSER_NAME)"), halt().'

# XXX
test: ass1
	$(ERL) 'ass1_test:test(), halt().'
