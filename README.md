# r-nonwear-methods
R code for classifying nonwear accelerometry data with Hidden Markov Models and Gaussian Mixture Models

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






