# Data preparation and analysis logs for the paper *Determinants of Phonetic Word Duration in Ten Language Documentation Corpora*
This repository contains R scripts and analysis logs for the paper *Determinants of phonetic word duration in ten language documentation corpora* by Jan Strunk, Frank Seifart, Swintha Danielsen, Iren Hartmann, Brigitte Pakendorf, Søren Wichmann, Alena Witzlack-Makarevich, and Balthasar Bickel.

The repository contains the following three files:

file | description
-----|-------------
`data_preparation.r_functions.R` | A collection of R functions for further annotating language documentation corpora in R and for preparing them for analysis. The functions have been saved as a text file in UTF-8 format, which can be read into R using `source("data_preparation.r_functions.R")`.
`data_preparation.txt` | A text file containing a log of how the language documentation data was read into R using Taras Zakharko's R library `ToolboxSearch` (https://bitbucket.org/tzakharko/toolboxsearch) and then further annotated and prepared for analysis using the functions contained in `data_preparation.r_functions.R`.
`analysis.rmd` | A KnitR workbook (https://yihui.org/knitr/) containing the R code for the statistical analyses reported in the paper.

Additional Python scripts to prepare Toolbox files for import into R and to count words, morphs, etc. in Toolbox corpora can be found in the GitHub repository: https://github.com/janstrunk/ToolboxTools.

