
analysis:
	R --vanilla --slave --file=analysis.r

sample:
	R --vanilla --slave --file=sample.r

import:
	python import.py

untar:
	tar -vxJf query-results.tar.xz

clean:
	find . -name "*.pdf" -print -delete
	find . -name "*.def" -print -delete
