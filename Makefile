DIR_EBIN   = ebin
DIR_SRC    = src
DIR_LEXER  = $(DIR_SRC)/lexer
DIR_PARSER = $(DIR_SRC)/parser

PROJECT_NAME = ducc
LEXER_NAME   = lexer
PARSER_NAME  = parser
SCRIPTS      = analyzer codegen ducc lexer parser runtest translator

ERLC_FLAGS = -Wall -Ddebug
ERLC       = erlc -o $(DIR_EBIN) $(ERLC_FLAGS)
ERL        = erl -pa $(DIR_EBIN) -noshell

all: setup clean compile

setup:
	@ echo '--- Trimming and cleaning ...'
	mkdir -p $(DIR_EBIN)
	- ruby src/trim_and_clean.rb $(DIR_SRC)/*.* $(DIR_SRC)/**/*.* \
		report/**/*.* $(SCRIPTS)

clean:
	@ echo '--- Removing generated files ...'
	rm -rf $(DIR_LEXER)/$(LEXER_NAME).erl
	rm -rf $(DIR_PARSER)/$(PARSER_NAME).erl
	rm -rf $(DIR_EBIN)/*.beam
	rm -rf *.dump *.gz *.beam

compile:
	clear
	@ echo '--- Generating lexer and parser ...'
	$(ERL) -eval 'io:format("~p~n",[leex:file("$(DIR_LEXER)/$(LEXER_NAME)")]), halt().'
	$(ERL) -eval 'io:format("~p~n",[yecc:file("$(DIR_PARSER)/$(PARSER_NAME)")]), halt().'
	@ echo '--- Compiling ...'
	$(ERLC) $(DIR_SRC)/*.erl
	$(ERLC) $(DIR_SRC)/**/*.erl

gen_tests: all
	@ echo '--- Regenerating tests ...'
	ruby src/gen_tests.rb -l suite/**/*.c suite/**/**/*.c
	ruby src/gen_tests.rb -p suite/**/*.c suite/**/**/*.c
	ruby src/gen_tests.rb -a suite/**/*.c suite/**/**/*.c
	ruby src/gen_tests.rb -t suite/**/*.c suite/**/**/*.c
	# ruby src/gen_tests.rb -c suite/**/*.c suite/**/**/*.c

tests: all
	@ echo '--- Running tests ...'
	runtest

pack: all
	tar -czvf $(PROJECT_NAME)-`date +"%Y-%m-%d"`.tar.gz \
		$(DIR_EBIN) $(DIR_SRC) report suite \
		$(SCRIPTS) Makefile *.md
