all: presentation.pdf
	
presentation.pdf: presentation.md makefile
	pandoc -t beamer presentation.md -o presentation.pdf
