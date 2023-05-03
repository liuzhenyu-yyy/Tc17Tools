# Tc17Tools

Some requently used R functions during work. Just pack them up to make my life easier...

## Installation

You can install the development version of Tc17Tools from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("liuzhenyu-yyy/Tc17Tools")
```

## Example

There are many functions and data serving different purpose in the package, some example include:

```{r example}
library(Tc17Tools)

# load gene annotation file:
data("gene.info.hg38")

# plot overlap of genomic regions (bed files)
myBedOverlap(GM12878.H3K4me3.rep1, GM12878.H3K4me3.rep2, extend = 50)

# calculate point density using 2D KDE method
myGetDensity(plot.data$x, plot.data$y)

# plot pairwise alignmen
myPrintPairwiseAlignment(pwAln)
```
