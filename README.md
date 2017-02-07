# README #

``occCompare`` is designed to be a general purpose package for comparing one-class classifiers.
It uses the [**oneClass**](https://github.com/benmack/oneClass) package and, thus, supports comparisons between the biased SMV, Maxent and the one-class SVM.
It is easy to extend the comparison for other one-class classifiers when they are implemented in the ``oneClass`` package.  

It is necessary to define functions for deriving training, validation and test data from your data in a specific format as required by the package.
Then ``occCompare`` can be used to run the experiments and analyse the results.

You can best learn how to set up such functions by visiting [**occCompareExp**](https://github.com/benmack/occCompareExp). 
The package has been used for producing the results for the paper

Mack, B., Waske, B. (2017): In-depth comparisons of MaxEnt, biased SVM and one-class SVM for one-class classification of remote sensing data. Remote Sensing Letters 8, 290–299. http://dx.doi.org/10.1080/2150704X.2016.1265689.

It can be used as an example to shows how to define the user-defined functions required for using ``occCompare`` and how to set up the experiments.
