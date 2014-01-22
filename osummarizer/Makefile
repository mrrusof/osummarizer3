osummarizer: osummarizer.sav
	spld --output="osummarizer" --main=restore --moveable --static --resources-from-sav --resources=osummarizer.sav=/osummarizer/osummarizer.sav

osummarizer.sav: main.pl osummarizer.pl log.pl
	rm -f osummarizer.sav
	sicstus --goal "compile(main), save_program('osummarizer.sav'), halt." 2>&1 | tee osummarizer.sav.log
	! grep error osummarizer.sav.log || (rm -f osummarizer.sav && false)

clean:
	rm -f *.sav *.sav.log osummarizer 1