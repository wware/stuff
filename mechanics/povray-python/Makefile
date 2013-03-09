POVRAY=/opt/local/bin/povray

.SUFFIXES: .in .png

.in.png:
	$(POVRAY) +I$(@:.png=.in) +W800 +H600

all: sample.png output.png

sample.png: sample.in
output.png: output.in

output.in: tryit.py povray/basics.py povray/shapes.py
	python tryit.py > output.in

clean:
	rm -f *.png *~ output.in *.pyc */*.pyc
