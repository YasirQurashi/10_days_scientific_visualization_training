Day_08_scientific_visualization_mastery
================
By Muhammad Yasir Qurashi
2026-03-15

# **Differential Gene Expression**

## **Today’s Goal**

The goal is to transform a gene expression matrix into a
publication-quality Volcano plot & MAplot that highlights Statistical
significance difference between genes in two different experimental
conditions.

## Step 01a

- Loading libraries & dataset

``` r
library(DESeq2)
```

    ## Loading required package: S4Vectors

    ## Loading required package: stats4

    ## Loading required package: BiocGenerics

    ## Loading required package: generics

    ## 
    ## Attaching package: 'generics'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.difftime, as.factor, as.ordered, intersect, is.element, setdiff,
    ##     setequal, union

    ## 
    ## Attaching package: 'BiocGenerics'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     IQR, mad, sd, var, xtabs

    ## The following objects are masked from 'package:base':
    ## 
    ##     anyDuplicated, aperm, append, as.data.frame, basename, cbind,
    ##     colnames, dirname, do.call, duplicated, eval, evalq, Filter, Find,
    ##     get, grep, grepl, is.unsorted, lapply, Map, mapply, match, mget,
    ##     order, paste, pmax, pmax.int, pmin, pmin.int, Position, rank,
    ##     rbind, Reduce, rownames, sapply, saveRDS, table, tapply, unique,
    ##     unsplit, which.max, which.min

    ## 
    ## Attaching package: 'S4Vectors'

    ## The following object is masked from 'package:utils':
    ## 
    ##     findMatches

    ## The following objects are masked from 'package:base':
    ## 
    ##     expand.grid, I, unname

    ## Loading required package: IRanges

    ## 
    ## Attaching package: 'IRanges'

    ## The following object is masked from 'package:grDevices':
    ## 
    ##     windows

    ## Loading required package: GenomicRanges

    ## Loading required package: Seqinfo

    ## Loading required package: SummarizedExperiment

    ## Loading required package: MatrixGenerics

    ## Loading required package: matrixStats

    ## 
    ## Attaching package: 'MatrixGenerics'

    ## The following objects are masked from 'package:matrixStats':
    ## 
    ##     colAlls, colAnyNAs, colAnys, colAvgsPerRowSet, colCollapse,
    ##     colCounts, colCummaxs, colCummins, colCumprods, colCumsums,
    ##     colDiffs, colIQRDiffs, colIQRs, colLogSumExps, colMadDiffs,
    ##     colMads, colMaxs, colMeans2, colMedians, colMins, colOrderStats,
    ##     colProds, colQuantiles, colRanges, colRanks, colSdDiffs, colSds,
    ##     colSums2, colTabulates, colVarDiffs, colVars, colWeightedMads,
    ##     colWeightedMeans, colWeightedMedians, colWeightedSds,
    ##     colWeightedVars, rowAlls, rowAnyNAs, rowAnys, rowAvgsPerColSet,
    ##     rowCollapse, rowCounts, rowCummaxs, rowCummins, rowCumprods,
    ##     rowCumsums, rowDiffs, rowIQRDiffs, rowIQRs, rowLogSumExps,
    ##     rowMadDiffs, rowMads, rowMaxs, rowMeans2, rowMedians, rowMins,
    ##     rowOrderStats, rowProds, rowQuantiles, rowRanges, rowRanks,
    ##     rowSdDiffs, rowSds, rowSums2, rowTabulates, rowVarDiffs, rowVars,
    ##     rowWeightedMads, rowWeightedMeans, rowWeightedMedians,
    ##     rowWeightedSds, rowWeightedVars

    ## Loading required package: Biobase

    ## Welcome to Bioconductor
    ## 
    ##     Vignettes contain introductory material; view with
    ##     'browseVignettes()'. To cite Bioconductor, see
    ##     'citation("Biobase")', and for packages 'citation("pkgname")'.

    ## 
    ## Attaching package: 'Biobase'

    ## The following object is masked from 'package:MatrixGenerics':
    ## 
    ##     rowMedians

    ## The following objects are masked from 'package:matrixStats':
    ## 
    ##     anyMissing, rowMedians

``` r
library(airway)
data("airway")

## DESeq2 package, which is one of the most widely used tools for differential gene expression analysis in RNA-seq studies.

## DESeq2 performs:

# 1. normalization

# 2. dispersion estimation

# 3. statistical testing; to determine which genes change significantly between conditions.
```

## Step 01b

- Extract count matrix & metadata

``` r
counts <- assay(airway) # Extract gene expression count matrix
head(counts)
```

    ##                 SRR1039508 SRR1039509 SRR1039512 SRR1039513 SRR1039516
    ## ENSG00000000003        679        448        873        408       1138
    ## ENSG00000000005          0          0          0          0          0
    ## ENSG00000000419        467        515        621        365        587
    ## ENSG00000000457        260        211        263        164        245
    ## ENSG00000000460         60         55         40         35         78
    ## ENSG00000000938          0          0          2          0          1
    ##                 SRR1039517 SRR1039520 SRR1039521
    ## ENSG00000000003       1047        770        572
    ## ENSG00000000005          0          0          0
    ## ENSG00000000419        799        417        508
    ## ENSG00000000457        331        233        229
    ## ENSG00000000460         63         76         60
    ## ENSG00000000938          0          0          0

``` r
colData(airway) # Extract gene expression metadata
```

    ## DataFrame with 8 rows and 9 columns
    ##            SampleName     cell      dex    albut        Run avgLength
    ##              <factor> <factor> <factor> <factor>   <factor> <integer>
    ## SRR1039508 GSM1275862  N61311     untrt    untrt SRR1039508       126
    ## SRR1039509 GSM1275863  N61311     trt      untrt SRR1039509       126
    ## SRR1039512 GSM1275866  N052611    untrt    untrt SRR1039512       126
    ## SRR1039513 GSM1275867  N052611    trt      untrt SRR1039513        87
    ## SRR1039516 GSM1275870  N080611    untrt    untrt SRR1039516       120
    ## SRR1039517 GSM1275871  N080611    trt      untrt SRR1039517       126
    ## SRR1039520 GSM1275874  N061011    untrt    untrt SRR1039520       101
    ## SRR1039521 GSM1275875  N061011    trt      untrt SRR1039521        98
    ##            Experiment    Sample    BioSample
    ##              <factor>  <factor>     <factor>
    ## SRR1039508  SRX384345 SRS508568 SAMN02422669
    ## SRR1039509  SRX384346 SRS508567 SAMN02422675
    ## SRR1039512  SRX384349 SRS508571 SAMN02422678
    ## SRR1039513  SRX384350 SRS508572 SAMN02422670
    ## SRR1039516  SRX384353 SRS508575 SAMN02422682
    ## SRR1039517  SRX384354 SRS508576 SAMN02422673
    ## SRR1039520  SRX384357 SRS508579 SAMN02422683
    ## SRR1039521  SRX384358 SRS508580 SAMN02422677

## Step 02

- Create DESeq dataset

``` r
DE_data <- DESeqDataSet(airway, design = ~dex)

## DESeqDataSet creates Creates an object that stores: 1. Expression counts 2. Sample metadata 3. Experimental design; design tells DESeq to compare gene in different treatments
```

## Step 03

- Filter low count reads

``` r
DE_data <- DE_data[rowSums(counts(DE_data)) > 10, ]
                   
## RNA-seq datasets contain many genes with very low counts
 
# Low-count genes; 1. increase noise 2. reduce statistical power
```

## Step 04

- Run Differential Expression analysis

``` r
DE_data <- DESeq(DE_data)
```

    ## estimating size factors

    ## estimating dispersions

    ## gene-wise dispersion estimates

    ## mean-dispersion relationship

    ## final dispersion estimates

    ## fitting model and testing

DESeq2 performs three operations;

- Normalization

Adjusts for sequencing depth differences between samples

- Dispersion estimation

Measures biological variability for each gene

- Negative binomial model testing

Tests whether gene expression differs between conditions

## Step 05

- Extract significant results from DESeq

``` r
DE_result <- results(DE_data)
head(DE_result)
```

    ## log2 fold change (MLE): dex untrt vs trt 
    ## Wald test p-value: dex untrt vs trt 
    ## DataFrame with 6 rows and 6 columns
    ##                  baseMean log2FoldChange     lfcSE      stat    pvalue
    ##                 <numeric>      <numeric> <numeric> <numeric> <numeric>
    ## ENSG00000000003  708.5839      0.3788379  0.173102  2.188526 0.0286313
    ## ENSG00000000419  520.2833     -0.2037671  0.100332 -2.030920 0.0422631
    ## ENSG00000000457  237.1568     -0.0340551  0.126108 -0.270048 0.7871234
    ## ENSG00000000460   57.9312      0.1171924  0.301660  0.388492 0.6976522
    ## ENSG00000000971 5817.1912     -0.4409662  0.258982 -1.702691 0.0886259
    ## ENSG00000001036 1282.0737      0.2419260  0.119482  2.024792 0.0428887
    ##                      padj
    ##                 <numeric>
    ## ENSG00000000003  0.136383
    ## ENSG00000000419  0.177656
    ## ENSG00000000457  0.927516
    ## ENSG00000000460  0.891163
    ## ENSG00000000971  0.293579
    ## ENSG00000001036  0.179390

``` r
nrow(DE_result)
```

    ## [1] 22008

## Step 06

- Order gene by significance

``` r
DE_result <- DE_result[order(DE_result$padj), ]
head(DE_result)
```

    ## log2 fold change (MLE): dex untrt vs trt 
    ## Wald test p-value: dex untrt vs trt 
    ## DataFrame with 6 rows and 6 columns
    ##                  baseMean log2FoldChange     lfcSE      stat       pvalue
    ##                 <numeric>      <numeric> <numeric> <numeric>    <numeric>
    ## ENSG00000152583   997.410       -4.60253 0.2117604  -21.7346 9.66061e-105
    ## ENSG00000148175 11193.408       -1.45147 0.0845138  -17.1744  4.12887e-66
    ## ENSG00000179094   776.573       -3.18385 0.2014865  -15.8018  3.02279e-56
    ## ENSG00000134686  2737.917       -1.38715 0.0913347  -15.1876  4.27556e-52
    ## ENSG00000125148  3656.161       -2.20345 0.1472655  -14.9624  1.29235e-50
    ## ENSG00000120129  3408.961       -2.94901 0.2015704  -14.6302  1.80296e-48
    ##                         padj
    ##                    <numeric>
    ## ENSG00000152583 1.75021e-100
    ## ENSG00000148175  3.74014e-62
    ## ENSG00000179094  1.82546e-52
    ## ENSG00000134686  1.93651e-48
    ## ENSG00000125148  4.68268e-47
    ## ENSG00000120129  5.44404e-45

``` r
top10_genes <- rownames(DE_result)[1:10]
```

## Step 07

- Volcano plot

``` r
library(EnhancedVolcano)
```

    ## Loading required package: ggplot2

    ## Loading required package: ggrepel

``` r
# Converting DE_result to data frame

res_df <- as.data.frame(DE_result)
head(res_df)
```

    ##                   baseMean log2FoldChange      lfcSE      stat        pvalue
    ## ENSG00000152583   997.4104      -4.602529 0.21176042 -21.73461 9.660609e-105
    ## ENSG00000148175 11193.4080      -1.451473 0.08451376 -17.17440  4.128875e-66
    ## ENSG00000179094   776.5733      -3.183853 0.20148653 -15.80182  3.022786e-56
    ## ENSG00000134686  2737.9169      -1.387152 0.09133474 -15.18756  4.275564e-52
    ## ENSG00000125148  3656.1612      -2.203449 0.14726555 -14.96242  1.292345e-50
    ## ENSG00000120129  3408.9608      -2.949012 0.20157038 -14.63019  1.802961e-48
    ##                          padj
    ## ENSG00000152583 1.750213e-100
    ## ENSG00000148175  3.740141e-62
    ## ENSG00000179094  1.825461e-52
    ## ENSG00000134686  1.936510e-48
    ## ENSG00000125148  4.682683e-47
    ## ENSG00000120129  5.444041e-45

``` r
class(res_df)
```

    ## [1] "data.frame"

``` r
EnhancedVolcano( res_df, x = 'log2FoldChange', y = 'padj', lab = row.names(res_df), pCutoff = 0.05, FCcutoff = 1, pointSize = 1, labSize = 2,
                 selectLab = top10_genes,
                 title = "Differential Expression: Dexamethasone Treatment",
                 col = c("grey30","forestgreen","royalblue","red2"),
                 subtitle = "Airway RNA-seq dataset",caption = "Made by Muhammad Yasir Qurashi", ylim = c(0,30), xlim = c(-5,+5)) +
  theme_bw()
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## ℹ The deprecated feature was likely used in the EnhancedVolcano package.
    ##   Please report the issue to the authors.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    ## ℹ Please use the `linewidth` argument instead.
    ## ℹ The deprecated feature was likely used in the EnhancedVolcano package.
    ##   Please report the issue to the authors.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](day_08_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

- Innterpretation of VOlcano plot

| Color | Meaning                 |
|-------|-------------------------|
| grey  | not significant         |
| blue  | significant fold change |
| green | significant p-value     |
| red   | both significant        |

Differential expression analysis identified multiple genes significantly
regulated by dexamethasone treatment. The volcano plot revealed a subset
of genes with both large fold changes and strong statistical support
(adjusted p-value \< 0.05), indicating substantial transcriptional
responses to glucocorticoid exposure.

Best Regards,

*Muhammad Yasir Qurashi*

Research Data Analysis Tools Mentor
