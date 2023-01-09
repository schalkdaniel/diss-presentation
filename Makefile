ENGINE    = lualatex --interaction=nonstopmode --no-file-line-error
BIBENGINE = bibtex
FILE      = main
MAKE      = make

.PHONY: all clean compile bcompile

all:
	@$(MAKE) --no-print-directory clean
	@$(MAKE) --no-print-directory compile || true # Prevent make from stopping during the first compilation
	@$(MAKE) --no-print-directory bcompile
	@$(MAKE) --no-print-directory compile
	@$(MAKE) --no-print-directory compile
	@$(MAKE) --no-print-directory clean

clean:
	@rm "$(FILE).aux" \
 	    "$(FILE).log" \
 		  "$(FILE).toc" \
 		  "$(FILE).blg" \
 		  "$(FILE).bbl" \
 		  "$(FILE).lof" \
 		  "$(FILE).lot" \
 		  "$(FILE).out" \
 		  "$(FILE).dvi" \
 		  "$(FILE).vrb" \
 		  "$(FILE).snm" \
 		  "$(FILE).nav" || true

compile:
	$(ENGINE) "$(FILE).tex"

bcompile:
	$(BIBENGINE) "$(FILE).aux"
