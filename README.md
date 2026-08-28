# Video-Games-Sales-Analysis

# Overview

Analyzed a video games dataset containing 16,598 entries. Began with an exploratory data analysis with visualizations to identify interesting patterns and findings across genres, publishers, years, platforms, and sales. After these observations, research questions and hypotheses were formulated and tested using permutation tests.

# Research Questions

1) For Nintendo games, do Sports games have a higher average North American sales than Action games?
2) Is the proportion of Sports games higher on Wii than DS in 2008?

# Methods 

1) Exploratory data analysis and visualizations
2) Data subsetting
3) Calculation of difference in means
4) Calculation of difference in proportions
5) Permutation tests
6) P-values
7) 95% confidence intervals
8) Bonferroni correction for multiple hypotheses

# Findings 

1) After applying permutation test with a p-value < 0.05, found evidence that Sports games had a higher observed average North American sales than Action games among Nintendo titles.
2) After applying permutation test with a p-value < 0.05, found evidence of a difference in the proportion of Sports games between Wii and DS titles in 2008.
3) After applying bonferonni correction for multiple hypotheses, the difference in means discovery was proved statistically insignificant, while the difference in proportions discovery remained statisically significant. 


# Files 

**1) Video_Games_Sales_Analysis.R** -> Contains code for exploratory data analysis 
**2) Video_Games_Permutations.R** -> Contains code for hypothesis testing, permutation tests, and confidence intervals based on formulated research questions. 
**3) Video Game Sales Data Analysis** -> Report about exploratory data analysis. Identified 3 interesting patterns noticed involving differences in regional sales depending on the video game genre.
**4) Video Game Sales Hypothesis Testing Report** -> Report about the two research questions, as well as previously tested questions that were statistically insignificant after permutation tests. 
**5) vgsales.csv** -> The CSV file containing 16,598 entries of video game sales information.

# Data

Contains 16,598 entries of video games including the genre, publisher, year, platform, global sales, North American sales, Europe sales, and Japan sales. 
Source: https://www.kaggle.com/datasets/gregorut/videogamesales


The dataset was independently sourced from Kaggle for a course project. The project instructions included an exploratory analysis, hypothesis testing, and permutation tests. 
