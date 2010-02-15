# Make file for cecho v0.0.1

.PHONY: all clean

BEAMS       := $(patsubst src/%, ebin/%, $(patsubst %.erl, %.beam, $(wildcard src/*.erl)))
ECINCLUDES  := -I include
ECFLAGS     := +debug_info +strict_record_tests +netload

EDEF        := include/cecho_commands.hrl
CDEF        := $(patsubst %.hrl, %.h, $(EDEF))

ERLDIR      := /usr/local/lib/erlang
DRIVER      := priv/lib/cecho.so
CFLAGS      := -g -Iinclude -I$(ERLDIR)/erts-5.6.5/include -I$(ERLDIR)/lib/erl_interface-3.5.9/include
LDFLAGS     := -L$(ERLDIR)/lib/erl_interface-3.5.9/lib

all: $(EDEF) $(BEAMS) $(DRIVER)

$(EDEF): $(CDEF)
	@echo "[SED]" $<": "$@
	@sed 's|//|%%|g' $< | sed 's|.define \([A-Z\_]*\) \([0-9]*\)|-define(\1, \2).|g' > $(EDEF)

ebin/%.beam: src/%.erl
	@echo "[ERLC]" $<": "$@
	@erlc -o ebin/ $(ECINCLUDES) $(ECFLAGS) $<

priv/lib/%.so: c_src/%.c
	@echo "[GCC]" $<": "$@
	@gcc -o $@ -Wall -fpic -shared $(CFLAGS) $(LDFLAGS) $<  -lerl_interface -lei -lncurses

clean:
	rm -f ebin/*.beam
	rm -f priv/lib/*.so
	rm -f $(EDEF)
