# workshop_gdcp_2019
Repository for the workshop attendees of the GDCP 2019

##
Currently this repository fits a topic model (Latent Dirichlet Allocation) on a training set of conference abstracts from a German chemistry and physics education conference. We use the package `topicmodels`. 
The repository contains two R-files, which can be run in the following order:

* 01_webscraping.R
  - gets the abstracts from http://www.gdcp.de/ 
  - creates a new directory for the CSV-File of the abstracts
  - saves the CSV-File
  
* 02_analysis.R
  - sample train data
  - prepares text
  - fits a LDA Model that showed to be somewhat interpretable
  - outputs a CSV-file for interpretation (contaiing highest ranked words (via probabilities) and texts (via topic proportions) for each topic)
  - outputs additional PNG-files containing the distributions

Additionally two folders are present:
 
* "other_data"
  - folder containing CSV-file with names of relevant cities, countries and bundesländer (to be excluded from the analysis)
               
* "packrat"
  - lock-file
