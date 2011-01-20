DIR_EBIN   = ebin
DIR_LEXER  = lexer
DIR_PARSER = parser

PARSER_BASE_NAME = ducc
LEXER_NAME       = $(PARSER_BASE_NAME)_lexer
PARSER_NAME      = $(PARSER_BASE_NAME)_parser

ERLC_FLAGS = -W0 -Ddebug +debug_info
ERLC       = erlc -o $(DIR_EBIN) $(ERLC_FLAGS)
ERL        = erl -I -pa ebin -noshell -eval

ass1:
	mkdir -p $(DIR_EBIN)
	@- echo '--- Generating the lexer ...'
	$(ERL) 'leex:file("$(DIR_LEXER)/$(LEXER_NAME)"), halt().'
	@- echo '--- Compiling lexer ...'
	$(ERLC) $(DIR_LEXER)/$(LEXER_NAME).erl
	@- echo '--- Compiling main ...'
	$(ERLC) main.erl
	@- echo '--- Testing lexer ...'
	erl -pa ebin -noshell -s main start testfil

# XXX
ass2:
    $(ERL) 'yecc:file("$(DIR_PARSER)/$(PARSER_NAME)"), halt().'

# XXX
test: ass1
	$(ERL) 'ass1_test:test(), halt().'

clean:
	rm $(DIR_LEXER)/$(LEXER_NAME).erl
	rm $(DIR_EBIN)/*.beam
	rm *.dump

all: clean ass1

pack:
	tar -czvf ducc.tar.gz ebin lexer parser suite Makefile main.erl testfil
