# <h1 align="center">*<ins>Data Science Salaries - US West Coast</ins>*</h1>

## Authors
- Andrea Lamonarca (andrea.lamonarca@studbocconi.it)
- Fabian Menekshi (fabian.menekshi@studbocconi.it)

## Introduction

This project was done as part of the "30549 - Mathematical Statistics" course at Bocconi University.
In our project we set out to investigate how the salaries in Data Science related professions
in the western region of the US are influenced by a number of variables, like education
and skills, that candidates are required or advised to possess by job offers. To accomplish
this task, we have carefully assembled our own dataset consisting of data from job offers
in the data science field taken from the indeed.com platform. With the data collected,
our objective is to train and select a simple and interpretable regression model that best
captures the dependence of salary on the set of prerequisites suggested in a given job listing.
Furthermore, we aim to test different hypotheses on the requirements in order to evaluate
their effect on salaries and are particularly interested in analysing education levels, especially
seeking an answer to the question: does a PhD guarantee a higher salary?

## Repository contents

```text
.
├── README.md
├── LICENSE
├── .gitignore
├── requirements.txt                     # Python deps for NLP/scraping
├── data/
│   ├── raw/                             # original exports / untouched input files
│   └── processed/                       # final analysis-ready dataset
├── src/
│   ├── python/                          # NLP code                                  
│   │   ├── ner_model/
│   │   └── nlp_notebook.ipynb/
│   └── R/                               # statistical analysis and modeling code
│       ├── analysis/
│       ├── modeling/
│       └── visualization/
├── docs/
│   └── report/
│       └── Lamonarca_Menekshi_Statistics_Project.pdf

├── results/                             # generated plots, tables, and model outputs
│   ├── tables/
│   └── plots/
```

## Main findings
- Salary is strongly associated with experience, location, education, and job type.
- Some technical and soft skills are associated with higher salary.
- Doctorate-level education was associated with higher salaries in this dataset.

## Reproducibility
1. Prepare the raw job-posting dataset in `data/raw/`
2. Run NLP extraction pipeline
3. Save processed dataset to `data/processed/`
4. Run the R analysis scripts in `src/R/analysis/`

## Installation and setup

### 1. Clone the repository
```text
git clone https://github.com/FabianMenekshi/Data_Science_Salaries-West_Coast.git
cd Data_Science_Salaries-West_Coast
pip install -r requirements.txt
```
### 2. Install Python dependencies
```text
pip install -r requirements.txt
```

### 3. Run R analysis
```text
Rscript src/R/analysis/salary_model_analysis.R
```

## Report
See `docs/report/Lamonarca_Menekshi_Statistics_Project.pdf`