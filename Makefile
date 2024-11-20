WFLAGS=-Wfunction-references=y

DEPS=erwasm/minibeam/math.wat \
  erwasm/minibeam/minibeam_mem.wat \
  erwasm/minibeam/minibeam_list.wat \
  erwasm/minibeam/minibeam_eq.wat \
  erwasm/minibeam/minibeam_tuple.wat \
  erwasm/minibeam/shim.wat \
  erwasm/minibeam/minibeam_bs.wat

JSONE=vendor/jsone_decode.wat vendor/jsone_encode.wat

SOURCES_SYNC=$(SOURCES) $(DEPS) sync-entry.wat

%.S: %.erl
	erlc -DNO_MAP_TYPE -S -o $(shell dirname $<) $<

%.S: %.ex
	env ERL_COMPILER_OPTIONS="'S'" elixirc $< 2> /dev/null ; [ "$(shell basename $<)".S -nt $< ]
	mv "$(shell basename $<).S" $@

%.wat: %.S
	python erwasm/erwasmc.py $< $@

%.wasm: %.wat
	wat2wasm $< -o $@

jsone%.fat.wat: jsone%.wat $(JSONE)
	python watcat/watmerge.py $@ $< $(DEPS) $(JSONE)

atom%.fat.wat: atom%.wat esrc/datalib.erl esrc/datalib.wat
	python watcat/watmerge.py $@ $< $(DEPS) esrc/datalib.wat

%.fat.wat: %.wat
	python watcat/watmerge.py $@ $< $(DEPS)

clean:
	rm -f {vendor,esrc}/*.{S,wat,wasm}

all: elib.fat.wasm
