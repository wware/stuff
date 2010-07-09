.SUFFIXES: .tex .dvi .ps .pdf .dot .eps ;

.dot.eps:
	dot -Tps $(@:.eps=.dot) -o $@

.tex.dvi:
	latex $(@:.dvi=.tex)
	latex $(@:.dvi=.tex)
	latex $(@:.dvi=.tex)

.dvi.ps:
	dvips $(@:.ps=.dvi)

.ps.pdf:
	ps2pdf $(@:.pdf=.ps)

view: readme.pdf
	evince readme.pdf &

ex1.dot ex2.dot ex3.dot: stateMachine.py smtests.py
	python smtests.py dot

readme.dvi: ex1.eps ex2.eps ex3.eps

clean:
	rm -f *.dot *.dvi *.aux *.log *.out *.eps *.pdf *.pyc
