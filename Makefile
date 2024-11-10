WFLAGS=-Wfunction-references=y

DEPS=erwasm/minibeam/math.wat \
  erwasm/minibeam/minibeam_mem.wat \
  erwasm/minibeam/minibeam_list.wat \
  erwasm/minibeam/minibeam_eq.wat \
  erwasm/minibeam/minibeam_tuple.wat \
  erwasm/minibeam/shim.wat \
  erwasm/minibeam/minibeam_bs.wat

SOURCES_SYNC=$(SOURCES) $(DEPS) sync-entry.wat

%.S: %.erl
	erlc -DNO_MAP_TYPE -S -o $(shell dirname $<) $<

%.wat: %.S
	python erwasm/erwasmc.py $< $@

%.wasm: %.wat
	wat2wasm $< -o $@

%.fat.wat: %.wat
	python watcat/watmerge.py $@ $< $(DEPS)

all: elib.fat.wasm