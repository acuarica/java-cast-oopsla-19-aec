
# Casting about in the Dark - Artifact Evaluation for OOPSLA'19

Our paper is an empirical study about how developers use casting in Java.
We have manually inspected and categorized 5000 cast operations.
This artifact provides the cast categorization with the appropriate scripts to make the plots we have used in the paper.
Moreover, this artifact provides an Overview, a Getting Started Guide and Step by Step on how to extract a sample from our cast dataset.

A note about *consistency*.
This artifact differs in *three* ways with the *submitted* paper.
The first two reasons are because of 

1. More patterns. The reviewers pointed out that some patterns were too big. Thus, we had to split them in new pattern.
In section ? we describe which are the new patterns.
2. Distribution of casts, there is a new step. We plan to incorporate these new results into the final version of the paper.
3. 

Because of 

[Distribution of Casts](#distribution-of-casts)

## Overview

This artifact is provided through a source code control repository.
It can be found online at [https://gitlab.com/acuarica/java-cast-oopsla-19-aec](https://gitlab.com/acuarica/java-cast-oopsla-19-aec).
Given the nature of our empirical study,
no need to compile source code and just a few dependencies,
we decided that a source code control repository would be a more appropriate to distribute the artifact rather than a Virtual Machine (recommended method).

The root folder contains the following files:

* `Makefile`: The Makefile script to run all commands described in the *Step by Step Instructions*.
* `query-results.tar.xz` and `query-results.json`: The dataset with casts can be found online as well. This is the dataset provided directly by Semmle. The `query-results.tar.xz` can be found [online](https://drive.google.com/a/semmle.com/file/d/1Wo7cfA5_nwml4lqrZWMhTWY7W8a-Xq_6/view?usp=drive_web).
* `casts-*.csv`: The samples that have been manually inspected.
* `casts.csv`: Consolidated casts from `casts-*.csv`.
* `sample-casts-5000`: A sample output of casts.
* `import.py`: Script to extract the cast instances into a SQLite database.
* `sample.r`: Script to create a sample from the cast database.
* `analysis.r`: Main script to make the plot shown in the paper.
* `dist.r`: Script to make the cast distribution plots.

## Getting Started Guide

The scripts uses different tools and languages:

* `make`
* `tar`
* `python`
* `R`

To run the R scripts, the following R packages are needed to be installed:

```R
install.packages("DBI")
install.packages("RSQLite")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("plyr")
install.packages("reshape2")
```

The scripts in the following section were tested on macOS.

## Step by Step Instructions

### Uncompress cast results

The cast dataset provided by Semmle is a `.tar.xz` compressed file.
The following command uncompress this file.

```sh
make untar
```

*Expected output*

The `query-results` folder should be created.
This folder contains the casts found per source control host and project.

For each project there is an `output.csv` file with the cast results as well as `stderr` and `stdout` files,
which are not interesting (these are log files).
Note that each `output.csv` file contains a header line,
so if the file has one line this means there were not any casts.

The directory names tell you which project the query was run on.
For example,
`query-results/github/yanzhenjie/NoHttp/1506231257083:1506170287574:2966721477769636017/output.csv`
contains the results of running your query on the GitHub project `yanzhenjie/NoHttp` (i.e., [https://github.com/yanzhenjie/NoHttp](https://github.com/yanzhenjie/NoHttp)).
The last component is an internal Semmle identifier is of not interest to us.

### Importing all casts into a single database

Once the query results were extracted,
they need to be imported in a SQLite database for better manipulation.
This is done with the following command.

```sh
make import
```

*Expected output*

The database `output.sqlite3` should be created.

### Creating sample table

```sh
make sample
```

*Expected output*

The sample file `sample-casts-5000.csv` should be created.

### Manual categorization of cast samples

The manual categorization tables are `casts-5000.csv`, `casts-480.csv`, `casts-47.csv` and `casts-3.csv`.
A consolidated table made can be found in `casts.csv`.
These files are comma-separated values (CSV) tables.

Each row represents a cast instance.
This table contains 6 columns.
The *castid* and *repoid* columns represent internal IDs to uniquely identify each cast instance and each project.
The *target* and *source* columns indicate the source and target types used in the cast.
The last two columns---*link* and *value*---are the link to the source code file in [lgtm.com](https://lgtm.com) and the result of the manual inspection.

We had to sample more than 5000 casts.
The CSV table mentioned above contains 5,530 casts (rows).
This is because we found 526 links that were not accessible during our analysis,
making manual code inspection impossible.
Inaccessible links can be found because some projects were removed from the lgtm platform.
We also found 1 cast that was clearly a bug,
a downcast using the wrong cast operand.
Thus, we had to resample the cast instances until we reach 5,000 manually inspected casts.
When resampling, we took care of inspecting *different* cast instances,
i.e., we have discarded duplicated casts.
We found 3 duplicated casts when resampling.

The script to process the results of the manual inspection is `analysis.r`.

To run the analysis, run the following command:

```sh
make analysis
```

### Distribution of casts

```sh
make dist
```
