
dist:
	R --vanilla --slave --file=dist.r

analysis:
	R --vanilla --slave --file=analysis.r
	sed -i '' '5,6d' table-patterns.pdf
	find ./patterns -name "*.pdf" -exec sed -i '' '5,6d' {} \; 

sample:
	R --vanilla --slave --file=sample.r

import:
	python import.py

untar:
	tar -vxJf query-results.tar.xz

clean:
	find . -name "*.pdf" -print -delete
	find . -name "*.def" -print -delete

overview:
	pandoc README.md --pdf-engine=pdflatex -o README.pdf
