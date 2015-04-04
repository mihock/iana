Iana is a browser-based graphical user interface to R functions for the psychometric analysis of questionnaires and tests with a binary or ordinal response format (e.g., Likert scales). Iana tries to integrate the essential statistical analysis steps into a convenient interface. The application is "reactive", meaning that its output is immediately updated if any of the inputs (e.g., the selected variables) are changed. This makes it easy to compare the impact of different statistical options or of item selection on the results. Iana keeps track of the R commands it constructs as a response to user input, so the analysis steps are documented and can be replicated. Iana comes with a small built-in data set, with which the interface can be tested, however, other data can easily be read in.

### First Start

If you read this, you probably know how to start Iana:

    library(iana)
    runiana()

You may have noticed that the first start takes some time. This is because Iana first pulls the R packages it needs for its computations from one of the CRAN mirrors and installs them on your machine. This happens only once. Subsequent starts of Iana are much faster and should not take more than a few seconds.

### Analyzing Data

On startup, Iana checks which data frames are present in your R environment and shows them in the dropdown list located in the right part of the GUI. Selecting a data frame makes its variables available to the functions implemented in Iana. You see these variables as labels of the check boxes located beneath the data frame dropdown list. These labels are updated whenever you select a new data frame. 

Possibly, not all variables in the data frame are shown as labels. For each variable in the selected data frame, Iana checks whether it looks like a binary or a Likert-type item. Such items normally have a low number of discrete values (2--9), which are usually coded as integers with an origin of 0 or 1. Iana considers variables that meet these criteria as items and hides the other variables in the data frame. You can change these criteria by specifying a different range of values in the numeric input box located above the item list. Setting this value to the maximum (20) shows all numeric variables in the data frame, including variables with non-integer or negative values. Notice, however, that not all functions are able to handle non-integer variables, so that you will get error messages if you try to analyze such variables.

The check boxes allow you to compile a set of items for analysis. For example, if you check four specific variables, the reliability of the scale composed of these four items will be reported. Instead to checking the items one after another you can also define a range of successive items to be combined using the input field below the check boxes. A variable range is specified by typing the names of two items (separated by a colon) into the input box, for example `e1:e4`. This is often faster than checking and unchecking single items. Typing only one variable name into the box deselects all variables except the specified variable. A quick way of unchecking all variables is to type any character into the field that is not the name of a variable.

### Links to Packages Used by Iana

For psychometric analyses, Iana builds mainly upon the following packages:

* [psych](http://CRAN.R-project.org/package=psych) for exploratory factor analysis,

* [lavaan](http://CRAN.R-project.org/package=lavaan) and [semTools](http://CRAN.R-project.org/package=semTools) for confirmatory factor analysis,

* [eRm](http://CRAN.R-project.org/package=eRm) for Rasch models,

* [mirt](http://CRAN.R-project.org/package=mirt) for multidimensional IRT models.
