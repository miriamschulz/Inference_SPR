# Self-Paced-Reading study


## 1. General information

This directory contains the materials and results of the self-paced-reading study for my master's thesis experiment on inference generation.

Experimental design: 2 x 2 within subjects context manipulation, crossing the factors Event Association x Inference.

8 lists (4 Latin lists in pseudorandomized order; 2 versions of each list, with button order / trial order reversed in the second version of each).

SPR experiment on PCIbex:
- Demonstration link: https://farm.pcibex.net/r/jDVSWb/
- Data collection link: https://farm.pcibex.net/p/jjOKBX/


## 2. Files and folders in this directory

- `format_stimuli` folder: contains the R-scripts used to prepare the items for PCIbex: pseudorandomize the lists and put the items into the required format (`.csv` item tables).
- `ibex_files` folder: contains the PCIbex experiment files
- `results` folder: contains the full downloaded PCIbex results (`results.csv`) and the R-scripts to process the results:
  - `results_SPR_preprocess.Rmd`:
    - Reads in the raw file from PC Ibex and allows to perform some completeness checks.
    - Annotates subject and list numbers / button order.
    - Merges the results with the stimuli file and exports this as `results_reads.csv`.
    - Also exports survey text answers by participants as `Survey_textanswers.csv`.
    - The rest of the script contains several descriptive statistics / plots of the data.
    - At the end, inspects the instruction reading times per participant to see whether any participants skipped the instructions (ignore 0.000 results here; #TODO fix bug).
  - `results_SPR_sample.Rmd`: Reads in the preprocessed results file and randomly samples a subset of participants from the data (equally distributed across lists) to (visually) inspect the variation in the data.
  - `results_SPR_rRT.Rmd`: Reads in the preprocessed results file and performs a regression-based RT analysis.
- `safety_copies` folder: contains downloaded safety copies of the full experiment for each order from PC Ibex.


## 3. Results

### 3.1 Participants and recruitment

- 42 subjects were recruited through Prolific (10 per Latin list; 2 were excluded due to a technical problem and a double submission (timed out)).
- Prolific criteria:
    - Location: Germany / Austria
    - Desktop devices only
    - age 18-32
    - Nationality: German
    - First language: German
    - No language related disorders
    - No literacy difficulties
    - No participation in any of the pretest studies  
    - Approval rate: 99-100
- Experiment length and reward
    - Estimated study length: 30 minutes
    - Reward set to 8 pounds/hour
    - Actual median study completion time: 21:03 minutes
    - Actual average reward per hour: £11.40
- Demographics and survey results (41 subjects):
    - Age: mean 25.54, range: 19-32
    - Gender: 21 female, 20 male, 0 other
    - Handedness: 37 righthanded, 3 lefthanded, 1 ambidexter
    - Native language: 39x German only, 1x German and Turkish, 1x German and Czech
    - Problems and remarks reported: none
    - Guesses, strategy, remarks: see exported file `Survey_textanswers.csv`. 4 participants noted that negation was a factor in the design; 1 of these noted that trials with "nie" were mostly implausible.
    - Should their data be kept, i.e. did they take part seriously? 41x yes



### 3.2 Plausibility ratings and rating reaction times

Mean rating per condition:
- ...


### 3.3 Reading times

Mean critical word reading time by condition/region:
- ...


## 4 Time log

- 30.08.2022: Format items for SPR, set up SPR
- September 2022: Run SPR
- October 2022: Analyze SPR data
