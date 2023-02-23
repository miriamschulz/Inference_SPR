# Self-Paced-Reading study


## 1. General information

This directory contains the materials and results for a self-paced-reading study on inference generation.

Experimental design: 2 x 2 context manipulation.

8 lists (4 Latin lists in pseudorandomized order; 2 versions of each list, with button order / trial order reversed in the second version of each).

SPR experiment PCIbex demonstration link: https://farm.pcibex.net/r/jDVSWb/


## 2. Files and folders in this directory

- `ibex_files` folder: contains the PCIbex experiment files
- `results` folder: contains the cleaned, anonymized RT results (`results_reads.csv`) and the following R-markdown-scripts to process and analyze the results:
  - `pretests.{Rmd|html}`:
    - generates the pretest plots and runs pair-wise t-tests
  - `ratings.{Rmd|html}`:
    - contains the results of the plausibility ratings: task accuracy and reaction times
  - `regression_analysis.{Rmd|html}`:
    - reads in the preprocessed results file, inspects reading times and performs a regression-based RT analysis.
