define colorecho
      @tput setaf $1
      @echo $2 $3
      @tput sgr0
endef

RED='\033[0;31m'
GREEN='\033[0;32'
NC='\033[0m'

ALISP_EXE ?= ../build/bin/alisp
ALISP_FLAGS=-Q
SHELL=/bin/sh

basic_scripts=basic_bool.al basic_comments.al  basic_cond.al  basic_constant.al  basic_defun.al  basic_dump.al  basic_funcall.al  basic_if.al  basic_let.al  basic_loops.al  basic_mapc.al  basic_math.al  basic_predicates.al  basic_print.al  basic_progn.al  basic_quote.al  basic_set.al  basic_setq.al  basic_string_literals.al  basic_unless.al  basic_when.al basic_lambda.al

example_scripts=arguments.al backquote.al big_example.al callstack_dump.al env_dump.al  gen_func_sym.al imports.al lists.al macros.al mathy.al parses.al ranges.al scope.al strings.al 

all : basic examples
.PHONY : all

basic : $(basic_scripts)
examples : $(example_scripts)


$(basic_scripts):
	$(call colorecho, 10, "-n", "Executing $@... ")
	@$(ALISP_EXE) $(ALISP_FLAGS) ./basic/$@ > ./basic/$@.output || (echo -n $(RED) "failed" $(NC); exit 1)
	@echo ""

$(example_scripts):
	$(call colorecho, 10, "-n", "Executing $@... ")
	@$(ALISP_EXE) $(ALISP_FLAGS) ./examples/$@ > ./examples/$@.output || (echo -n $(RED) "failed" $(NC); exit 1)
	@echo ""

clean:
	@rm -rf ./basic/*.output
	@rm -rf ./examples/*.output
	@rm -rf ./*.output
