all: services content communication utils caption

services:
	 pdflatex $@.tex


content:
	 pdflatex $@.tex


communication:
	 pdflatex $@.tex


utils:
	 pdflatex $@.tex


caption:
	pdflatex $@.tex ; pdfcrop $@.pdf $@.pdf

clean:
	rm *~ *.aux *.bbl *.blg *.log *.out


cleanall: clean
	rm *.pdf