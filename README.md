# hepaticas: Colombian Liverworts

hepaticas is an R package that provides datasets of phylogenetic relationships of Colombian liverworts (i.e. cladograms),
among with functions for manipulating these datasets. Datasets can be found as a newcick file (.nwk)in 
[trees/newick](trees/newick), as a tree file (.tre) in [trees/tree](trees/tree), and as an R data file (.rda) in [data](data).

You can prune the family cladogram with the function `tree_by_family`, the genera cladogram with the function `tree_by_genus`, and
the species cladogram with the function `tree_by_species`. 

There is also a species for adding morphotypes or adding species not included in the datasets. It is `add_species`. The function 
automatically include the species with the exisiting genus.

## Installation

We have not a released version yet, however, you can install the
development version of hepaticas:

``` r
install.packages("remotes")
remotes::install_github("davidbiol/hepaticas")
```
