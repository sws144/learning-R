#
# Auto GLM chapter for
# Predictive Modeling Applications in Actuarial Science
# Volume II
#
# Ernesto Schirmacher
#

File                        Description
=========================== ------------------------------------------------------------
README-pmaas.txt            This file. It describes the content of files for this
                            chapter.

pmaas-data.R                R script file to read the CSV file sim-modeling-dataset.csv
                            and prepare the modeling dataset and create additional
                            variables that are needed during modeling.
                            This script creates the R Object all-data.RData

pmaas-text-calcs.R          This R script has all the calculations that support
                            statements made in the main text of this chapter.

sim-modeling-dataset.csv    A text file containing the raw dataset for this chapter.
                            The file contains one header row with variable names,
                            40,760 rows of data, and 27 columns.

all-data.RData              This file is not distributed with the raw data; it's the
                            output from running the file pmaas-data.R
                            This is the modeling dataset used throughout the chapter.

fns.R                       Some utility functions used in the text.
