# Getting and Cleaning Data Course Project
For the script run_analysis.R to work, you will need to do the following steps
- Download and unzip the data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
- The unzipped folder will need to be (re)named as 'dataset' and in the same directory where the script is located. 
- Use the command, source("run_analysis.R") in RStudio.
- There will be an output of tidy\_data\_average.txt
- Use read.table("tidy\_data\_average.txt") to read the output. The dimensions of the data frame would be 180 68, 6 activities for 30 subjects and 66 variables (the first two columns are subjects and activities respectively).