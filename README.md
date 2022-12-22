# r-nonwear-methods
R code for classifying nonwear accelerometry data with Hidden Markov Models and Gaussian Mixture Models

## Files

* __manuscript.R__ - Follows the calculations from the manuscript (see citation) and produces the plot of the X, Y, and Z axis centroids, activity state, and log of the determinant of the covariance matrix for one subject.  Requires the __*accelerometR*__ library (instructions below).
* __performanceResultsBoxPlots.R__ - Generates box plots from nonwear methods performance results which are saved in `results/performanceResults.csv`.  The plots can be saved to disk (.tiff) by editing the R code and changing the top level assignment of `saveAsTiffs` from `saveAsTiffs = FALSE` to `saveAsTiffs = TRUE`.
* __gmmPartitioned.R__ - GMM training/testing of 15 subjects with a 60/40 partition.  Outputs table summaries.  Plots can be enabled by uncommenting these entries in the source code.  

# Requirements and installation

## accelerometR library

The accelerometR library is required by the R scripts in this repository.  The library included in the accelerometR folder but must be built first before it can used with the other R scripts.  The terms _library_ and _package_ are used interchangeably here.  

### Dependencies

The accelerometR library has the following dependencies:

* `mhsmm`
* `randomForest`
* `resahpe2`

These can be installed from the CRAN repository using R-Studio, for example.

### Building the accelerometR library
1. Open a command prompt or terminal.  One is available in R Studio.  
2. Navigate to your repository.  For example: `cd ~/git/r-non-wear-methods`
3. Run R's build command:  `r CMD build accelerometR`

You should see the following, if done successfully:

    * checking for file ‘accelerometR/DESCRIPTION’ ... OK
    * preparing ‘accelerometR’:
    * checking DESCRIPTION meta-information ... OK
    * checking for LF line-endings in source and make files and shell scripts
    * checking for empty or unneeded directories
    * building ‘accelerometR_0.0.0.9000.tar.gz’

If there are dependency ERRORs, then install the missing dependencies and try again.  

### Installing the accelerometR package

Once the accelerometR package has been built, you may install it from the same command prompt or terminal using R's install command:

`r CMD INSTALL accelerometR_0.0.0.9000.tar.gz`

You should see somemthing similar to the following, if done successfully:

    * installing to library ‘/Library/Frameworks/R.framework/Versions/4.1/Resources/library’
    * installing *source* package ‘accelerometR’ ...
    ** using staged installation
    ** R
    ** data
    *** moving datasets to lazyload DB
    ** inst
    ** byte-compile and prepare package for lazy loading
    ** help
    *** installing help indices
    ** building package indices
    ** installing vignettes
    ** testing if installed package can be loaded from temporary location
    ** testing if installed package can be loaded from final location
    ** testing if installed package keeps a record of temporary installation path
    * DONE (accelerometR)






