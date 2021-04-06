# c2ergometer

An R package for parsing CSV plaintext files output by the Concept2 PM5 Performance Monitor into a longitudinal, relational database.

*This package is for personal, non-commercial use and is not affiliated with Concept2, Inc.* This code is maintained by Bret Beheim and licensed under Creative Commons [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/). See LICENSE.md for details.

[![License: CC BY-NC-SA 4.0](https://licensebuttons.net/l/by-nc-sa/4.0/80x15.png)](https://creativecommons.org/licenses/by-nc-sa/4.0/)

# Versions Supported

This software loads CSV plaintext Logbook files downloaded from the PM5 Performance Monitor using the [Concept2 Utility](https://www.concept2.com/service/software) software. I have tested it using Logbook Versions `6.97`, `7.05.3`, `7.09` and above.

# Instructions

In the R programming environment, install the [remotes](https://cran.r-project.org/web/packages/remotes/index.html) package using. This package can then be installed directly from Github:

```r
install.packages("remotes")
remotes::install_github("babeheim/c2ergometer")
```

The primary function for reading the PM5 output is `read_pm5`. In R, we can load a logbook file Logbook.csv via

```r
library(c2ergometer)
db <- read_pm5("Logbook.csv")
```

This will create a list object called `db` holding two R tables ("data frames"): `workouts`, which is one-row-per-workout, and `splits`, which is one-row-per-split inside each workout. The two tables are connected by the `date` and `time_of_day` variables.

If a set of logbook files needs to be absorbed into this database, we can use the `create_pm5_database` function. Here is an example:

```r
my_logbooks <- c("Logbook.csv", "Logbook2.csv", "Logbook3.csv")
db <- create_pm5_database(my_logbooks)
```

This function will load all three files and combine them into `workouts` and `splits` tables as above. Duplicate entries across logbook files (those with identical dates and times of day) will be compacted to unique entries only, allowing aggregation of multiple files from the same Performance Monitor.

