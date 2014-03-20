GREEN="\033[0;32m"
RED="\033[0;31m"
NO_COLOR="\033[0m"

all: osummarizer

osummarizer: osummarizer.sav
	spld --output="osummarizer" --main=restore --moveable --static --resources-from-sav --resources=osummarizer.sav=/osummarizer/osummarizer.sav

osummarizer.sav: main.pl osummarizer.pl terms.pl log.pl debug.pl ext/utils/misc.pl
	@rm -f osummarizer.sav
	sicstus --goal "compile(main), save_program('osummarizer.sav'), halt." 2>&1 | tee /tmp/osummarizer.sav.log
	@(! sed -e '/\[Choices\] treated as local in do-loop but also used outside/D' \
               -e '/suggest renaming or adding param(\[Choices\])/D' \
               -e '/Approximate lines: 274-286, file: .*ext\/utils\/misc.pl/D' </tmp/osummarizer.sav.log | \
	   grep -n -e '^\!' -e '^\*') \
         || (rm -f osummarizer.sav && false)

ext/utils/misc.pl:
	./ext/checkout-externals.sh

tests: osummarizer
	@./run-tests.sh | tee /tmp/osum-tests

unit:
	sicstus -l unit.pl --goal 'halt.' >/tmp/unit.pl.log 2>&1
	@(! sed -e '/\[Choices\] treated as local in do-loop but also used outside/D' \
	       -e '/suggest renaming or adding param(\[Choices\])/D' \
               -e '/Approximate lines: 274-286, file: .*ext\/utils\/misc.pl/D' </tmp/unit.pl.log | \
	   grep -n -e '^\!' -e '^\*')
	@grep -e PASSED -e FAILED </tmp/unit.pl.log | \
	sed -e 's/PASSED/\\033[0;32mPASSED\\033[0m/' -e 's/FAILED/\\033[0;31mFAILED\\033[0m/' | \
	(while read -r l; do echo "$$l"; done) | \
        tee /tmp/osum-unit

check: osummarizer unit tests
	@grep FAILED /tmp/osum-tests /tmp/osum-unit 2>/dev/null | sed 's/[^:]*://' >/tmp/osum-failed
	@test -s /tmp/osum-failed && echo '\nFAILED TESTS:\n' && cat /tmp/osum-failed

clean:
	rm -f *.sav osummarizer 1 tests/*/*.qarmc counterexample
