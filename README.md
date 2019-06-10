Iana
====

Iana is an [R](https://www.r-project.org/) package that implements a browser-based GUI for classical item and test analysis, factor analysis, and item response modeling with an focus on ordered-category items.

The documentation is included in the package. A short description can be found [here](https://github.com/mihock/iana/blob/master/inst/iana-shiny/help.md).

**Installation**

The following R commands install the packages "devtools" (needed to install Iana) and "iana."

```
if (!require(devtools)) install.packages("devtools")
library(devtools)
install_github("mihock/iana")
```

The first line is only needed if you haven't already installed "devtools."

To launch Iana, type

```
library(iana)
runiana()
```

into an R console.
