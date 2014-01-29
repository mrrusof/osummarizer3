all: osummarizer

osummarizer: osummarizer.sav
	spld --output="osummarizer" --main=restore --moveable --static --resources-from-sav --resources=osummarizer.sav=/osummarizer/osummarizer.sav

osummarizer.sav: main.pl osummarizer.pl log.pl
	rm -f osummarizer.sav
	sicstus --goal "compile(main), save_program('osummarizer.sav'), halt." 2>&1 | tee osummarizer.sav.log
	(! sed '/\[Choices\] treated as local in do-loop but also used outside/D' <osummarizer.sav.log | \
	   grep -e error -e 'treated as local in do-loop but also used outside') \
         || (rm -f osummarizer.sav && false)

clean:
	rm -f *.sav *.sav.log osummarizer 1