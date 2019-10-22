# Rvivo
## Introduction
Rvivo has been written to simplify data analysis from in vivo experiments using R 3.6.
*In vivo* experiments start after a certain treshold has been reached, often as dictated by the animal license. Unfortunately,these start dates don't all occur on the same day as tumour growth is variable
in mice. This means that most scientists work with error prone and complicated 
excell sheets to allign all the data.

This package takes a simple comma (or tab) delimited file with three columns of
sample information followed by either tumour measurements, tumour volumes or
luminesence. Together with an indication of the treshold given by the user it 
selects experiment start days, determines the growth and survival over the course
of the experiment. 

## Input data
### Pre-calculated tumour volumes or luminesence:

| Cage  |Treatment | Mouse ID | 2020-01-01 | 2020-01-07 |
|:-----:|:--------:|:--------:|:----------:|:----------:|
|   1   |  DrugA   |    L     |    60      |     91     |
|   1   |  DrugA   |    R     |   132      |     150    |
|   2   |  DrugB   |    BE    |    92      |     108    |
|   2   |  DrugB   |    NM    |    81      |     95     |

### Tumour measurements (height and width):

| Cage  |Treatment | Mouse ID | 2020-01-01 | 2020-01-01 | 2020-01-07 | 2020-01-07 |
|:-----:|:--------:|:--------:|:----------:|:----------:|:----------:|:----------:|
|   1   |  DrugA   |    L     |    10      |     6      |    10      |     9.1    |
|   1   |  DrugA   |    R     |    12      |     11     |    12.5    |     12     |
|   2   |  DrugB   |    BE    |    11.5    |     8      |    12      |     9      |
|   2   |  DrugB   |    NM    |    9       |     9      |    10      |     9.5    |

Note: when providing measurements the tumour height and width are adjacent columns
with the same date as column name.

### Cull data
To generate survival curves and decide which mice have been excluded from the 
experiment Rvivo uses the provided cul data. A seperate tab or comma delimited file with
three columns identifyin mice and a fourth column with the cull date.

| Cage  |Treatment | Mouse ID | culldate    |
|:-----:|:--------:|:--------:|:-----------:|
|   1   |  DrugA   |    L     |  2020-02-21 |
|   1   |  DrugA   |    R     |             |
|   2   |  DrugB   |    BE    |  2020-03-01 |
|   2   |  DrugB   |    NM    |             |

If no cull date has been entered the mice is presumed to be alive unless
there are missing tumour measurements. At which point Rvivo presumes the mice
has been culled due to reasons unrelated to the experiment and it will be excluded.

Dates in the format : year-month-day (2020-12-31) and day/month/year (31/12/2020) are supported.


```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```
## Executing Rvivo
Rvivo requires two inputs: 

* Measurement or volumes file structured as indicated above
* Cull data.

If you provide both measurements and volumes data an error will be returned. 
Use volumes for pre-calculated tumour sizes and Luminesence data. Measurements is 
solely for height and width data. It calculates the tumour size under the assumption the tumour is spherical.

```{r setup, eval = FALSE}
library(Rvivo)
# to get parameter explanation type ?rvivo
rvivo(volumes = NULL,
      measures = "path/to/measures.csv",
      cul = "/path/to/cull.csv",
      minvol = 90,
      colstyle = "Dark2",
      figures = TRUE,
      table = TRUE,
      axis = "linear",
      survival = TRUE,
      output = "/path/to/outputfolder")
```
## Output
Rvivo will output a table with the start date of each animal, the difference in tumour size from the beginning to the end of the treatment and tumour size over the course of the experiment. 

Additionally, it will plot tumour growth over time for each animal as well as KM-survival curves. As luminesence follows a logaritmic scale the axis of the growth plots can be set to log-scale using axis = "log".
