# Rvivo
---
## Introduction
Rvivo has been written to simplify data analysis for *in vivo* experiments.
These experiments often involve treatments of mice which start after a certain 
treshold has been reached. This threshold is dictated by the animal license. 
Unfortunately, due to the unique growth rate of tumours these start dates
don't all occur on the same day. This means that most scientists work with
error prone and complicated excel sheets to allign all the data.

This package takes a simple comma (or tab) delimited file with three columns of
sample information followed by either tumour measurements (height and width),
tumour volumes or luminesence. Together with an treshold given by the user it 
selects experiment start days, determines the growth and survival over the course
of the experiment. 

## Input data

Either pre-calculated values or a file containing height/width values in adjacent
columns can be used as input. Before version 0.9.2.0000 a seperate file with 
cul dates was required. This is still supported, but the prefered format is as 
follows:

### Pre-calculated tumour volumes or luminesence:

| Cage  |Treatment | Mouse ID | Exclude | 2020-01-01 | 2020-01-07 |
|:-----:|:--------:|:--------:|:-------:|:----------:|:----------:|
|   1   |  DrugA   |    L     |    y    |    60      |     91     |
|   1   |  DrugA   |    R     |         |   132      |     150    |
|   2   |  DrugB   |    BE    |         |    92      |     108    |
|   2   |  DrugB   |    NM    |         |    81      |     95     |

### Tumour measurements (height and width):

| Cage  |Treatment | Mouse ID | Exclude | 2020-01-01 | 2020-01-01 | 2020-01-07 | 2020-01-07 |
|:-----:|:--------:|:--------:|:-------:|:----------:|:----------:|:----------:|:----------:|
|   1   |  DrugA   |    L     |    y    |    10      |     6      |    10      |     9.1    |
|   1   |  DrugA   |    R     |         |    12      |     11     |    12.5    |     12     |
|   2   |  DrugB   |    BE    |         |    11.5    |     8      |    12      |     9      |
|   2   |  DrugB   |    NM    |         |    9       |     9      |    10      |     9.5    |

Note 1: When providing measurements the tumour height and width are adjacent columns
with the same date as column name.
Note 2: Any letter or symbol in the Exclude column will exclude that mouse from
the study.
Note 3: When a mice is culled leave the remaining columns of the experiment blank.
When a mouse does not show any symptons, but has not been sacrificed fill in 0 for 
remaining columns.

### Cull data
If you are working with a separate file containing information on the culdates of mice
**the volumes / measurement file should not contain the exclude column.** 

In the case of a seperate culdata file Rvivo determines the mice have been excluded from the 
experiment by referencing the culdata when a sample does not contain measurements
up to the last day of the experiment (the final column in Volumes/measurements).
Culdata should be a seperate tab or comma delimited file with
three columns identifying mice and a fourth column with the cull date.

| Cage  |Treatment | Mouse ID | culldate    |
|:-----:|:--------:|:--------:|:-----------:|
|   1   |  DrugA   |    L     |  2020-02-21 |
|   1   |  DrugA   |    R     |             |
|   2   |  DrugB   |    BE    |  2020-03-01 |
|   2   |  DrugB   |    NM    |             |

If no cull date has been entered the mice is presumed to be alive unless
there are missing tumour measurements. At which point Rvivo presumes the mice
has been culled due to reasons unrelated to the experiment and it will be excluded.

Dates in the format : Year-month-day (2020-12-31) and day/month/year (31/12/2020) are supported.


```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```
## Executing Rvivo
Rvivo calculates tumour growth and survival over an experiment time. The 
beginning and end of this experiment time are determined by combining the dates of measurement 
and experiment thresholds that are often dictated by licences, such as minimum tumour volume.

Rvivo requires at minimum two variables:

* Measurement or Volumes: input files structured as indicated above.
* min: minimum volume (mm3) or luminsence values.

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

Rvivo will output a table with the start date of each animal, the difference in tumour size from the beginning to the end of the treatment and tumour size over the course of the experiment. Additionally it will plot tumour growth over time for each animal as well as KM-survival curves. As luminesence follows a logaritmic scale the axis of the growth plots can be set to log-scale using axis = "log".

