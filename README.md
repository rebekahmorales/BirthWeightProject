# BirthWeightProject
Analyzing Baystate Medical Center's Birth Weight Data

## Overview

The goal of this project is to determine key factors in the birth weight of a baby by using supervised data mining techniques combine with linear regression and confidence intervals. 

There are comments throughout the R-Code to help you understand what I was doing or thinking. There is also a **write-up** where I gathered my thoughts and useful graphs to help you understand the process outside of R. 


## The Data Set

The overall dataset came from Baystate Medical Center in Springfield Massachusetts during 1986. There are 189 cases and 10 total variables in my data set.
* Response Variable
  * bwt - Birth Weight of Baby in Grams

* Explanatory Variable
  * low - Indicator/Flag if a birth weight is less than 2.5kg
  * race - Mother's Race (1=White, 2=African American, 3=Other)
  * age - Mother's Age in years
  * ht - History of Hypertension (0=No, 1=Yes)
  * ptl - Number of previous premature labor(s)
  * lwt - Mother's weight in pounds at last menstrual period
  * smoke - Mother smoked during pregnancy (0=No, 1=Yes)
  * ui - Presence of Uterine Irritability (0=No, 1=Yes)
  * ftv - Number of first trimester visits
