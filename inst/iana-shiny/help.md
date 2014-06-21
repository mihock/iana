Iana is a browser-based graphical user interface to R functions for the psychometric analysis of questionnaires and tests with an ordinal response format (e.g., Likert scales). Iana tries to integrate the essential statistical analysis steps into a convenient interface. The application is "reactive", meaning that its output is immediately updated if any of the inputs (e.g., the selected variables) are changed. This makes it easy to compare the impact of different statistical options or of item selection on the results. Iana keeps track of the R commands it constructs as a response to user input, so the analysis steps are documented and can be replicated. Iana comes with a small built-in data set, with which the interface can be tested, however, other data can easily be read in.

### First Start

I you read this, you probably know how to start Iana. You may have noticed that the first start takes some time. This is because Iana first pulls the R packages it needs for its computations from one of the CRAN mirrors and installs them on your machine. This happens only once. Subsequent starts of Iana are much faster and should not take more than a few seconds.

### Analyzing Data

On startup, Iana checks which data frames are present in your R environment and shows them in the dropdown list located in the upper right of the GUI. Selecting a data frame makes its variables available to the functions implemented in Iana. You see these variables as labels of the check boxes located beneath the data frame dropdown list. These labels are updated whenever you select a new data frame. 

Possibly, not all variables in the data frame are shown as labels. For each variable in the selected data frame, Iana checks whether it looks like a Likert item. Such items normally have a low number of discrete values (3--9), which are usually coded as integers with an origin of 0 or 1. Iana considers variables that meet these criteria as items and hides the other variables in the data frame. You can change these criteria by specifying a different range of values in the numeric input box located above the item list. Setting this value to the maximum (20) shows all numeric variables in the data frame, including variables with non-integer or negative values. Notice, however, that not all functions are able to handle non-integer variables, so that you will get error messages if you try to analyze such variables.

The check boxes allow you to compile a set of items for analysis. For example, if you check four specific variables, the reliability of the scale composed of these four items will be reported. Instead to checking the items one after another you can also define a range of successive items to be combined using the input field below the check boxes. A variable range is specified by typing the names of two items (separated by a colon) into the input box, for example `e1:e4`. This is often faster than checking and unchecking single items. Typing only one variable name into the box unselects all variables except the specified variable. A quick way of unchecking all variables is to type any character into the field that is not the name of a variable.


### Used Packages

Revelle, W. (2012). psych: Procedures for Personality and Psychological Research.

Bernaards, Coen A. and Jennrich, Robert I. (2005) Gradient Projection Algorithms and Software for Arbitrary Rotation Criteria in Factor Analysis, Educational and Psychological Measurement: 65, 676-696. <http://www.stat.ucla.edu/research/gpa>

Yves Rosseel (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/.

Sunthud Pornprasertmanit, Patrick Miller, Alexander Schoemann and Yves Rosseel (2012). semTools: Useful tools for structural equation modeling. R package version 0.2-11. http://CRAN.R-project.org/package=semTools

Sarkar, Deepayan (2008) Lattice: Multivariate Data Visualization with R. Springer, New York. ISBN 978-0-387-75968-5

H. Wickham. ggplot2: elegant graphics for data analysis. Springer New York, 2009.

Patrick Mair, Reinhold Hatzinger and Marco J. Maier (2012). eRm: Extended Rasch Modeling. R package version 0.15-1. http://CRAN.R-project.org/package=eRm

JJ Allaire, Jeffrey Horner, Vicent Marti and Natacha Porte (2012). markdown: Markdown rendering for R. R package version 0.5.3. http://CRAN.R-project.org/package=markdown

Hadley Wickham (2007). Reshaping Data with the reshape Package. Journal of Statistical Software, 21(12), 1-20. URL http://www.jstatsoft.org/v21/i12/.

Hadley Wickham (2012). stringr: Make it easier to work with strings.. R
package version 0.6.2. http://CRAN.R-project.org/package=stringr

John Fox (2010). polycor: Polychoric and Polyserial Correlations. R package version 0.7-8. http://CRAN.R-project.org/package=polycor
