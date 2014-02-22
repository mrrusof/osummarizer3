all: osummarizer

osummarizer: osummarizer.sav
	spld --output="osummarizer" --main=restore --moveable --static --resources-from-sav --resources=osummarizer.sav=/osummarizer/osummarizer.sav

osummarizer.sav: main.pl osummarizer.pl terms.ml log.pl debug.pl ext/utils/misc.pl
	rm -f osummarizer.sav
	sicstus --goal "compile(main), save_program('osummarizer.sav'), halt." 2>&1 | tee osummarizer.sav.log
	(! sed -e '/\[Choices\] treated as local in do-loop but also used outside/D' \
               -e '/suggest renaming or adding param(\[Choices\])/D' \
               -e '/Approximate lines: 274-286, file: .*ext\/utils\/misc.pl/D' <osummarizer.sav.log | \
	   grep -n -e '^\!' -e '^\*') \
         || (rm -f osummarizer.sav && false)

ext/utils/misc.pl:
	./ext/checkout-externals.sh

unit:
	sicstus -l unit.pl --goal 'flush_output, halt.' 2>/dev/null

clean:
	rm -f *.sav *.sav.log osummarizer 1 tests/*/*.qarmc
