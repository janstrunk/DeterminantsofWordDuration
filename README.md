# Data for the paper Determinants of Word Duration in Ten Language Documentation Corpora
This repository contains R scripts and analysis protocols for the paper "Determinants of phonetic word duration in ten language documentation corpora" by Jan Strunk, Frank Seifart, Swintha Danielsen, Iren Hartmann, Brigitte Pakendorf, Søren Wichmann, Alena Witzlack-Makarevich, and Balthasar Bickel.

This repository contains the following three files:

data_preparation.r_functions.R    A collection of R functions for further annotating language documentation corpora in R and for preparing them for analysis (saved as a text file in UTF-8 format, which can be read into R using source("data_preparation.r_functions.R"))

data_preparation.txt              A text file containing a protocol of how the language documentation data was read into R using Taras Zakharko's R library ToolboxSearch: https://bitbucket.org/tzakharko/toolboxsearch and then further annotated and prepared for analysis using the functions contained in data_preparation.r_functions.R.

analysis.rmd                      A KnitR workbook (https://yihui.org/knitr/) which contains the R code for the statistical analyses carried out for the paper.

Additional Python scripts used to prepare Toolbox files for import into R can be found in the GitHub repository:
https://github.com/janstrunk/ToolboxTools

