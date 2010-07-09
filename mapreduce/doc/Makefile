.SUFFIXES: .tex .fig .eps .pdf ;

.fig.pdf:
	fig2dev -L pdf $(@:.pdf=.fig) > $@

all: mr.pdf

mr.pdf: mr.tex mr_arch.pdf Yahoo-hadoop-cluster.pdf
	pdflatex mr.tex
	makeindex mr.idx
	pdflatex mr.tex
	pdflatex mr.tex

Yahoo-hadoop-cluster.pdf: Yahoo-hadoop-cluster.jpg
	convert Yahoo-hadoop-cluster.jpg Yahoo-hadoop-cluster.pdf
mr_arch.pdf: mr_arch.fig

clean:
	rm -f *.ind *.ilg *.toc *.log *.idx *.dvi *.aux *.bak *~
	rm -f *.pdf *.out
