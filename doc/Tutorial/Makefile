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
TARGET   = tutor
INSTALLDIR = /home/newville/public_html/ifeffit/tut_test/
TEXFILE  = $(TARGET).tex
OUTFILE  = $(TARGET).out
PSFILE   = $(TARGET).ps
DVIFILE  = $(TARGET).dvi
PDFFILE  = $(TARGET).pdf
BBLFILE  = $(TARGET).bbl
AUXFILE  = $(TARGET).aux
INDEXFILE= $(TARGET).ind
#
#
HTMLMACS  = ifmacs_html.tex
PDFMACS   = ifmacs_pdf.tex
PSMACS    = ifmacs_ps.tex
TEXMACS   = ifmacs.tex

PDFTITLE  = title_pdf.tex
HTMLTITLE = title_html.tex
TITLE     = title.tex

MAKE_ARCHIVE =  MakeArchive
FIXSBOX    = ../scripts/Fixsbox
LMAKE      = ../scripts/latexmk
FIX_PDF    = ../scripts/FixPDFout
L2H_INIT   = -init_file ../scripts/dot.latex2html-init
PDFLATEX   = pdflatex
HTMLDIR    = tutorial
LATEX2HTML = latex2html -dir $(HTMLDIR) -mkdir
L2H_OPTS   = -local_icons -image_type gif -transparent -white -verbosity 0 -no_math -html_version 3.2,math,table -show_section_numbers -title "IFEFFIT Tutorial"  

.PHONY: ! dep ps pdf bib clean html all

dvi: $(TITLE) $(TEXMACS)
	cp $(PSMACS)   $(TEXMACS)
	$(FIXSBOX) -d dvi $(TEXFILE)
	rm -rf *.bak
	$(LMAKE) -f $(TEXFILE)

! : $(TITLE) $(TEXMACS)
	$(LMAKE) -g -f $(TEXFILE)

dep: $(TITLE) $(TEXMACS)
	$(LMAKE) -f -i $(TEXFILE)

ps: $(TITLE) $(TEXMACS)
	make dvi
	$(LMAKE) -f -ps $(TEXFILE)

$(TITLE):
	cp $(PDFTITLE) $(TITLE)

$(TEXMACS):
	cp $(PSMACS)  $(TEXMACS)

pdf: $(PDFTITLE) $(PDFMACS)
	cp $(PDFMACS)  $(TEXMACS)
	cp $(PDFTITLE) $(TITLE)
	$(FIXSBOX) -d pdf $(TEXFILE)
	rm -rf *.bak
	$(PDFLATEX) $(TEXFILE)
	$(FIX_PDF) $(OUTFILE) > x.out ; mv x.out $(OUTFILE)
	$(PDFLATEX) $(TEXFILE)

html: $(HTMLTITLE) $(HTMLMACS)
	cp $(HTMLTITLE) $(TITLE)
	cp $(HTMLMACS)  $(TEXMACS)
	$(FIXSBOX) -d html $(TEXFILE)
	rm -rf *.bak
	rm -rf $(HTMLDIR)
	$(LMAKE) -f  $(TEXFILE)
	$(LATEX2HTML) $(L2H_INIT) $(L2H_OPTS) $(TEXFILE)
	rm -rf $(HTMLDIR)/TMP $(HTMLDIR)/images.* $(HTMLDIR)/*.pl	

all: pdf ps html

install: all
	rm -rf $(INSTALLDIR)
	cp -pr $(HTMLDIR) $(INSTALLDIR)
	cp -pr $(PSFILE) $(PDFFILE) $(INSTALLDIR).

bib:
	make $(BBLFILE)

clean :
	$(LMAKE) -C $(TEXFILE)
	rm -f *.aux $(PDFFILE)  $(TITLE) $(TEXMACS) $(OUTFILE)
	rm -rf $(HTMLDIR)

$(DVIFILE): *.tex
	latex $(TEXFILE)

$(PDFFILE): pdf


$(BBLFILE): biblio.bib  *.aux
	bibtex $(TARGET)

$(INDEXFILE): *.aux *.tex
	makeindex $(TARGET)

$(AUXFILE):  *.tex
	latex $(TEXFILE)

