#########################################################################
# makefile for latex documents: relies heavily on the  latexmk  script
# possible targets:
#   dvi       default:  run latexmk
#   !         latex at least once for sure
#   dep       re-scan dependencies
#   ps        make postscript
#   pdf       make pdf
#   html      make html with latex2html
#   clean     remove non-essential files
#   all       make postscript, pdf, html
#########################################################################
TARGET   = refman
INSTALLDIR = /home/newville/public_html/ifeffit/reftest/
TEXFILE  = $(TARGET).tex
OUTFILE  = $(TARGET).out
PSFILE   = $(TARGET).ps
DVIFILE  = $(TARGET).dvi
PDFFILE  = $(TARGET).pdf
BBLFILE  = $(TARGET).bbl
AUXFILE  = $(TARGET).aux
INDEXFILE= $(TARGET).ind

#
HTMLMACS  = ifmacs_html.tex
PDFMACS   = ifmacs_pdf.tex
TEXMACS   = ifmacs.tex

PDFTITLE  = title_pdf.tex
HTMLTITLE = title_html.tex
TITLE     = title.tex

MAKE_ARCHIVE =  ../scripts/MakeArchive
FIXTEX     = ../scripts/fixtex
LMAKE      = ../scripts/latexmk
FIX_PDF    = ../scripts/FixPDFout
L2H_INIT   = -init_file ../scripts/dot.latex2html-init
PDFLATEX   = pdflatex
LATEX2HTML = latex2html -dir $(INSTALLDIR) -mkdir
L2H_OPTS   = -local_icons -image_type gif -verbosity 0 -no_math -html_version 3.2,math -show_section_numbers -t "IFEFFIT Reference Manual"


.PHONY: ! dep ps pdf bib clean html all install

dvi: $(PDFTITLE) $(PDFMACS)
	$(FIXTEX) -t ps  *.tex
	$(LMAKE) -f $(TEXFILE)


! : $(PDFTITLE) $(PDFMACS)
	$(LMAKE) -g -f $(TEXFILE)


dep: $(PDFTITLE) $(PDFMACS)
	$(LMAKE) -f -i $(TEXFILE)

bib:
	make $(BBLFILE)

ps: dvi
	$(LMAKE) -f -ps $(TEXFILE)


pdf:
	$(FIXTEX) -t pdf *.tex
	$(PDFLATEX) $(TEXFILE)
	$(FIX_PDF)  $(OUTFILE)
	$(PDFLATEX) $(TEXFILE)

html: 
	$(FIXTEX) -t html *.tex
	rm -rf $(INSTALLDIR) *.bak
	mkdir $(INSTALLDIR)
	(cd $(INSTALLDIR) ; ln -s ../figs . ;cd ..)
	$(LATEX2HTML) $(L2H_INIT) $(L2H_OPTS) $(TEXFILE)

all: ps pdf html

install: all
	cp -pr $(PSFILE) $(PDFFILE) $(INSTALLDIR).


clean:
	$(LMAKE) -C $(TEXFILE)
	rm -f *.aux $(PDFFILE)  $(OUTFILE)
	rm -rf $(INSTALLDIR)

$(DVIFILE): *.tex
	latex $(TEXFILE)

$(PDFFILE): pdf


$(BBLFILE): ifx.bib  *.aux
	bibtex $(TARGET)

$(INDEXFILE): *.aux *.tex
	makeindex $(TARGET)

$(AUXFILE):  *.tex
	latex $(TEXFILE)

