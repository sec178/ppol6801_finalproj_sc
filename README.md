# Text as Data Final Project: Topic Modeling and Sentiment Analysis of UN Speeches

This repository contains the code and deliverables for the Text as Data: Computational Linguistics final project, for which I decided to explore how topic modeling and sentiment anlaysis can be applied to UN speeches, as well as how these findings can be validated historically and geographically. 

While much of the data files were too large to push to GitHub, all of the R Markdown scripts, slide decks, and papers can be found in this repository. The final deliverables include the R Markdown file used to perform my analysis, an R Shiny app (available [here](https://scohen97.shinyapps.io/tad_app/)), and a report and slide deck on my findings. The structure of this repository is as follows:

### Code
The code used to clean, visualize, and perform topic modeling and sentiment analysis is the main R Markdown file in the root directory: *ppol6801_TAD_finalproj*.

### App
The code for the R Shiny app and dashboard I used to illustrate my findings can be found int the *App* folder. A published, accessible, and publicly-available version of this dashboard can be found [here](https://scohen97.shinyapps.io/tad_app/).

### Report & Proposal
A report detailing my findings and motivations, as well as my original proposal, can be found in the *Report* and *Proposal* folders respectively. A rendered version of the R Markdown file can also be found within the *Report* folder.

### Slide Deck 
A slide deck detailing my findings can be found in the *Slide Deck* folder.

### Sources
The main source of the data used in this project is from the UN General Assembly [General Debate Corpus](https://www.kaggle.com/datasets/namigabbasov/united-nations-general-debate-corpus-1946-2023?resource=download), which I accessed from Kaggle. If replicating code, download corpus data to root directory.

I augmented this data set with data from the [World Development Indicators](https://databank.worldbank.org/source/world-development-indicators) portal from the World Bank. If replicating code, select GINI and GDP per capita data at the country-year level, move to root directory, and rename to "wbi.csv".

