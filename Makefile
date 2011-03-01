DIR_EBIN   = ebin
DIR_SRC    = src
DIR_LEXER  = $(DIR_SRC)/lexer
DIR_PARSER = $(DIR_SRC)/parser

PROJECT_NAME = ducc
LEXER_NAME   = lexer
PARSER_NAME  = parser
SCRIPTS      = analyzer codegen ducc emitter lexer parser runtest translator

ERLC_FLAGS = -Wall -Ddebug
ERLC       = erlc -o $(DIR_EBIN) $(ERLC_FLAGS)
ERL        = erl -pa $(DIR_EBIN) -noshell

all: setup clean compile

setup:
	@ echo '--- Trimming ...'
	mkdir -p $(DIR_EBIN)
	- ruby src/trim.rb $(DIR_SRC)/*.* $(DIR_SRC)/**/*.* \
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

gen_tests: gen_test_l gen_test_p gen_test_a gen_test_t gen_test_c gen_test_e

TEST_FILES = suite/*.c suite/**/*.c suite/**/**/*.c

gen_test_l: all
	@ echo '--- Regenerating lexer tests ...'
	ruby src/gen_tests.rb -l $(TEST_FILES)

gen_test_p: all
	@ echo '--- Regenerating parser tests ...'
	ruby src/gen_tests.rb -p $(TEST_FILES)

gen_test_a: all
	@ echo '--- Regenerating analyzer tests ...'
	ruby src/gen_tests.rb -a $(TEST_FILES)

gen_test_t: all
	@ echo '--- Regenerating translator tests ...'
	ruby src/gen_tests.rb -t $(TEST_FILES)

gen_test_c: all
	@ echo '--- Regenerating codegen tests ...'
	ruby src/gen_tests.rb -c $(TEST_FILES)

gen_test_e: all
	@ echo '--- Regenerating emitter tests ...'
	ruby src/gen_tests.rb -e $(TEST_FILES)

tests: all
	@ echo '--- Running tests ...'
	runtest

pack: all
	tar -czvf $(PROJECT_NAME)-`date +"%Y-%m-%d"`.tar.gz \
		$(DIR_EBIN) $(DIR_SRC) report suite \
		$(SCRIPTS) Makefile *.md
