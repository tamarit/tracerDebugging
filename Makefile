ROOT_DIR = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
ERLC_DIR = $(shell which erlc)
ERLC_PATH = $(shell dirname $(lastword $(ERLC_DIR)))

compile:
	@rm -Rf ebin
	@mkdir ebin
	@erlc -W0 -o ebin src/*.erl 

clean:
	@rm -Rf ebin
	@rm -f *.txt

run_paper:
	erl -pa ebin tracerDebugging/ebin -run adbg run -noshell -s erlang halt

# install:
# 	@erl -pa ebin -run make_script from_path $(ROOT_DIR)  -noshell -s erlang halt
# 	@chmod +x pn_suite_temp
# 	@mv -f pn_suite_temp $(ERLC_PATH)/pn_suite


