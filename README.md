# Self-Paced-Reading study


## 1. General information

This directory contains the materials and results for a self-paced-reading study on inference generation.

Experimental design: 2 x 2 context manipulation.

8 lists (4 Latin lists in pseudorandomized order; 2 versions of each list, with button order / trial order reversed in the second version of each).

SPR experiment on PCIbex:
- Demonstration link: https://farm.pcibex.net/r/jDVSWb/
- Data collection link: https://farm.pcibex.net/p/jjOKBX/


## 2. Files and folders in this directory

- `ibex_files` folder: contains the PCIbex experiment files
- `results` folder: contains the full downloaded PCIbex results (`results_reads.csv`) and the R-scripts to process the results:
  - `inspect.{Rmd|html}`:
    - performs data completeness checks
    - inspects the demographics and survey data
    - provides a first, *purely numeric/visual* inspection of the results (plausiblity ratings & reading times)
  - `pretests.{Rmd|html}`:
    generates the pretest plots and runs pair-wise t-tests
  - `ratings.{Rmd|html}`:
    - contains the results of the plausibility ratings: task accuracy and reaction times
  - `regression_analysis.{Rmd|html}`:
    - reads in the preprocessed results file, inspects reading times and performs a regression-based RT analysis.
