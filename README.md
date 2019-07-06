
# Java Cast Empirical Study for OOPSLA'19 - Artifact Evaluation

The data set with casts can be found online as well.
This is the dataset provided directly by Semmle.

https://drive.google.com/a/semmle.com/file/d/1Wo7cfA5_nwml4lqrZWMhTWY7W8a-Xq_6/view?usp=drive_web

## Getting Started Guide

### Dependencies

* make, tar
* python
* r

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
3 Importing ./query-results/github/Kuvaldis/playground/1506231257083:1505765346197:3190877618806210603/output.csv dist-1505765346197-1524814812150
4 Importing ./query-results/github/ManuelB/facebook-recommender-demo/1506231257083:45150017:3295913788698196517/output.csv dist-45150017-1524814812150
5 Importing ./query-results/github/sd6352051/NiftyNotification/1506231257083:2032380230:2834292473977066734/output.csv dist-2032380230-1524814812150
[...]
```