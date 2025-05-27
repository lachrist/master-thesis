#!/bin/bash
pdflatex -shell-escape thesis
bibtex thesis
makeglossaries thesis
pdflatex -shell-escape thesis
pdflatex -shell-escape thesis
