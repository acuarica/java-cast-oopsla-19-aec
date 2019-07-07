
# Java Cast Empirical Study for OOPSLA'19 - Artifact Evaluation

The data set with casts can be found online as well.
This is the dataset provided directly by Semmle.

https://drive.google.com/a/semmle.com/file/d/1Wo7cfA5_nwml4lqrZWMhTWY7W8a-Xq_6/view?usp=drive_web

## Getting Started Guide

The scripts uses different tools and languages.

### Dependencies

* make, tar
* python
* R

To run the R scripts, the following packages are needed to be installed:

```R
install.packages("DBI")
install.packages("RSQLite")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("plyr")
install.packages("reshape2")
```

## Step by Step Instructions

### Uncompress cast results

```sh
make untar
```

### Importing all cast into a single database

```sh
make import
```

#### Expected result

```sh
python import.py
1 Importing ./query-results/gitlab/sebastianbauersfeld/commons-io-gitlab/1506231257083:1506221246512:3251173281933460169/output.csv dist-1506221246512-1524814812150
2 Importing ./query-results/gitlab/fdroid/fdroidclient/1506231257083:1506169596183:2359306719844906717/output.csv dist-1506169596183-1524814812150
[...]
7558 Importing ./query-results/bitbucket/AndrewSwan_au/bgg4j/1506231257083:1290503729:2893971791070855794/output.csv dist-1290503729-1524814812150
7559 Importing ./query-results/bitbucket/outofcoffee/api-blueprint-mockserver/1506231257083:1280732355:2933174477644986629/output.csv dist-1280732355-1524814812150
Total files/repos imported: 7559
Total casts rows imported: 10193435
Total nonSnapshots: 215
```

### Creating sample table

```sh
make sample
```
