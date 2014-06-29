#' Mean substitution of missing values
#'
#' Replaces missing values in numeric variables of a data frame by 
#' rounded means or a specific value.
#'
#' @param x a data frame
#' @param imp.value imputed value. If NULL (default), the rounded mean is imputed. 
#'
#' @return data frame frame with imputed values
#'
#' @author Michael Hock \email{michael.hock@@uni-bamberg.de}
#'
#' @export
meansub <- function(x, imp.value=NULL) {
    if (!is.data.frame(x)) stop("Argument is not a dataframe")
    for (i in 1:ncol(x)) {
        y <- x[,i]
        if (!is.numeric(y)) {
            cat("Skipping", names(x)[i], "\n")
            next
        }
        if (is.null(imp.value))
            y[is.na(y)] <- round(mean(y, na.rm=TRUE))
        else
            y[is.na(y)] <- imp.value
        x[,i] = y
    }
    x
}

count.true <- function(x) {
  length(x[x])
}

descriptives <- function(Df, width=8, digits=2) {
  ## Prints descriptive statistics of the variables in a data frame.
  ## Non-numeric variables are ignored.

  count.false <- function(x) length(x[!x])
  count.true <- function(x) length(x[x])

  if (!is.data.frame(Df)) Df <- as.data.frame(Df)
  ##
  ## Numeric variables
  isnum <- sapply(Df, is.numeric)
  newDf <- subset(Df, select=isnum)
  if (length(newDf) == 0) {
    stop("No numeric variables encountered.")
  }
  origCol <- 1:ncol(Df)
  colwidth <- floor(log10(ncol(Df))) + 1
  Col <- formatC(origCol[isnum], format = "d", width=colwidth)
  Mean <- apply(newDf, 2, mean, na.rm=TRUE)
  SD <- apply(newDf, 2, sd, na.rm=TRUE)
  Min <- apply(newDf, 2, min, na.rm=TRUE)
  Max <- apply(newDf, 2, max, na.rm=TRUE)
  Valid <- apply(newDf, 2, complete.cases)
  Missing <- apply(Valid, 2, count.false)
  Valid <- apply(Valid, 2, count.true)
  ##
  ## Output
  M <- cbind(Mean, SD, Min, Max)
  M <- formatC(M, format= "f", width=width, digits=digits)
  Missing <- formatC(Missing, format = "d", width=width)
  Valid <- formatC(Valid, format = "d", width=width)
  M <- cbind(Col, M, Missing, Valid)
  print(M, quote=FALSE, right=TRUE)
  ##
  ## Print names of nonnumeric variables
  newDf <- subset(Df, select=!isnum)
  if (length(newDf) > 0) {
    cat("\nNonnumeric variables:\n")
    Col <- formatC(origCol[!isnum], format = "d", width=colwidth)
    Class <- sapply(newDf, function(x) paste(class(x), collapse = ", "))
    Valid <- apply(newDf, 2, complete.cases)
    Missing <- apply(Valid, 2, count.false)
    Valid <- apply(Valid, 2, count.true)
    Missing <- formatC(Missing, format = "d", width=width)
    Valid <- formatC(Valid, format = "d", width=width)
    newDf <- cbind(Col, Class, Missing, Valid)
    print(newDf, quote=FALSE, right = TRUE)
  }
  ##
  ## Valid cases
  cat("\n")
  Valid <- apply(Df, 2, complete.cases)
  Missing <- apply(Valid, 2, count.false)
  cat("Variables contain ")
  if (min(Missing) != max(Missing)) {
    cat(min(Missing), "to", max(Missing), "cases with missing values.\n")
  } else {
    cat(min(Missing), "case(s) with missing values.\n")
  }
  cat(nrow(Df), "cases in data frame.\n")
}

descriptives.groups <- function(Df, groups, width=8, digits=2) {
  if (!is.data.frame(Df)) Df <- as.data.frame(Df)
  isnum <- sapply(Df, is.numeric)
  Df <- subset(Df, select=isnum)
  groups <- as.factor(groups)
  l <- length(levels(groups))
  for (i in 1:l) {
    Group <- subset(Df, groups == levels(groups)[i])
    M <- apply(Group, 2, mean, na.rm=TRUE)
    SD <- apply(Group, 2, sd, na.rm=TRUE)
    if (i == 1) X <- cbind(M, SD)
    else X <- cbind(X, M, SD)
  }
  cat("Statistics for groups with levels: ", levels(groups), "\n")
  colnames(X) <- rep(c("M", "SD"), nlevels(groups))
  X <- formatC(X, format= "f", width=width, digits=digits)
  print(X, quote=FALSE, right=TRUE)
}

descriptives.tot.groups <- function(Df, groups, width=8, digits=2) {
  if (!is.data.frame(Df)) Df <- as.data.frame(Df)
  isnum <- sapply(Df, is.numeric)
  Df <- subset(Df, select=isnum)
  groups <- as.factor(groups)
  l <- length(levels(groups))
  M <- apply(Df, 2, mean, na.rm=TRUE)
  SD <- apply(Df, 2, sd, na.rm=TRUE)
  X <- cbind(M, SD)
  for (i in 1:l) {
    Group <- subset(Df, groups == levels(groups)[i])
    M <- apply(Group, 2, mean, na.rm=TRUE)
    SD <- apply(Group, 2, sd, na.rm=TRUE)
    X <- cbind(X, M, SD)
  }
  cat("Statistics for total sample and groups with levels: ", levels(groups), "\n")
  colnames(X) <- rep(c("M", "SD"), nlevels(groups) + 1)
  X <- formatC(X, format= "f", width=width, digits=digits)
  print(X, quote=FALSE, right=TRUE)
}


descriptivesbygroups <- function(Df, groups, width=8, digits=2) {
  if (!is.data.frame(Df)) Df <- as.data.frame(Df)
  isnum <- sapply(Df, is.numeric)
  Df <- subset(Df, select=isnum)
  M <- apply(Df, 2, mean, na.rm=TRUE)
  SD <- apply(Df, 2, sd, na.rm=TRUE)
  X <- cbind(M, SD)
  groups <- as.factor(groups)
  l <- length(levels(groups))
  for (i in 1:l) {
    Group <- subset(Df, groups == levels(groups)[i])
    M <- apply(Group, 2, mean, na.rm=TRUE)
    X <- cbind(X, M)
  }
  colnames(X) <- c("M", "SD", levels(groups))
  X <- formatC(X, format= "f", width=width, digits=digits)
  print(X, quote=FALSE, right=TRUE)
}


# descriptives <- function (X) {
#   nvars <- dim(X)[2]
#   for (i in 1:nvars) {
#     x <- X[,i]
#     if (is.numeric(x)) {
#       xname <- colnames(X)[i]
#       m <- mean(x,na.rm=T)
#       stdev <- sd(x,na.rm=T)
#       cat(formatC(xname, format = "s", width = 15),
#           formatC(m, format="f", width=8, digits=2),
#           " & (",
#           formatC(stdev, format="f", width=8, digits=2),
#           ") \\\\",
#           "\n")
#     }
#   }
# }

# descriptivesbygroups <- function (X, Groups) {
#  nvars <- dim(X)[2]
#  numGroups <- as.numeric(Groups)
#  for (i in 1:nvars) {
#     x <- X[,i]
#     xname <- colnames(X)[i]
#     m <- mean(x,na.rm=T)
#     stdev <- sd(x,na.rm=T)
#     cat(formatC(xname, format = "s", width = 15),
#         " & ",
#         formatC(m, format="f", width=6, digits=2),
#         " & (",
#         formatC(stdev, format="f", width=6, digits=2),
#         ")")
#     groupmeans <- tapply(x, Groups, mean)
#     for (j in min(numGroups):max(numGroups)) {
#       cat(" &", formatC(groupmeans[j], format="f", width=6, digits=2))
#     }
#     cat("\\\\", "\n")
#   }
# }

# The original
cor.probOrig <- function(X, dfr = nrow(X) - 2) {
  R <- cor(X)
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R
}

# The nice version
cor.prob <- function(X, Y=NULL, printprobs=FALSE, omitredundant = TRUE) {
  # Print correlation matrices with significance stars. If only X is
  # given, also print M and SD.

  # In case X or Y are vectors, remember their names
  if (is.vector(X)) {
    myname <- deparse(substitute(X))
    X <- data.frame(X)
    colnames(X) <- myname
  }
  if (is.vector(Y)) {
    myname <- deparse(substitute(Y))
    Y <- data.frame(Y)
    colnames(Y) <- myname
  }

  # Compute correlations (and descriptive stats if only x is given)
  if (is.null(Y)) {
    M <- round(colMeans(X), 2)
    SD <- round(apply(X, 2, sd), 2)
    R <- cor(X)
  } else  R <- cor(X, Y)

  # Compute p values
  dfr = nrow(X) - 2
  P <- R^2
  Fstat <- P * dfr / (1 - P)
  P <- 1 - pf(Fstat, 1, dfr)

  # Cosmetics and printout
  if (is.null(Y)) cat("\nMeans, SDs, and Correlations\n")
  else cat("\nCorrelations\n")
  rs <- round(R, 2)
  sigstars <- sigstars2 <- matrix(nrow=nrow(R), ncol=ncol(R))
  sigstars <- ifelse(P < 0.05, "*", " ")
  sigstars2 <- ifelse(P < 0.01, "*", " ")
  blanks <- ifelse(R >= 0 , " ", "")
  rs <- paste(blanks, rs, sigstars, sigstars2, sep="")
  rs <- matrix(rs, nrow=nrow(R))
  if (is.null(Y)) {
    if (omitredundant) {
      ##diag(rs) <- "  "
      rs[lower.tri(rs, diag = TRUE)] <- "   "
    } else diag(rs) <- "  1"
  }

  rownames(rs) <- rownames(R)
  colnames(rs) <- colnames(R)
  if (is.null(Y)) {
    rs <- cbind(M, SD, rs)
    if (omitredundant) {
      rs <- rs[, -3]
    }
  }
  print(rs, quote=FALSE)
  #cat("________________________________________________________\n")
  cat("\nNote: *p < .05, **p < .01 (two-tailed). N =", nrow(X), "\n")
  if (printprobs == TRUE) {
    cat("\nProbabilities (two-tailed)\n")
    P <- round(P, 3)
    print(P)
  }
}

#' GUI for Item Analysis and Scale Construction
#'
#' \code{iana} is a browser-based GUI for classical item and test analysis, factor analysis, and item response modeling with a focus on items with an ordered-category response format.
#'
#' \code{iana} is a browser-based graphical user interface to R functions for the psychometric analysis of questionnaires and tests with an ordinal response format. \code{iana} tries to integrate the essential statistical analysis steps into a convenient interface. \code{iana} covers classical item and test analysis (reliability, item discrimation), dimensionality tests (parallel analysis and MAP test), principal components and exploratory factor analysis (including factor analysis based on polychoric correlations), one-factor confirmatory analysis, and item response models (partial credit model). Graphical output includes histograms of item and test scores, empirical item characteric curves, and person-item maps, among others.
#'
#' \code{iana} is based on the \href{http://www.rstudio.com/shiny/}{Shiny Web Framework for R}, which allows a fast bidirectional communication between the browser and R. \code{iana} is "reactive", meaning that its output is instantly updated if any of the inputs (e.g., the selected variables) are changed by the user. This makes it easy to compare the impact of item selection and/or modeling options on the results. \code{iana} keeps track of the R commands it constructs as a response to user input, so the analysis steps are documented and can be replicated. \code{iana} comes with some built-in data sets, with which the interface can be tested, however, other data can easily be used.
#'
#' The basic usage is documented in \code{\link{runiana}}.
#'
#' \tabular{ll}{
#' Package: \tab iana\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2013-01-03\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' @name iana-package
#' @aliases iana
#' @importFrom psych alpha describe principal fa irt.fa
#' @import lattice ggplot2 GPArotation lavaan eRm markdown reshape2 stringr semTools polycor
#' @docType package
#' @author Michael Hock (\email{michael.hock@@uni-bamberg.de})
#' @references Shiny web framework for R: \url{http://www.rstudio.com/shiny/}
#' @keywords package
#' @examples
#' \dontrun{runiana()}
NULL

#' Data frames in global workspace
#'
#' Returns of a vector with the names of the data frames present in the
#' global environment.
#'
#' @return A character vector with the names of the data frames
#'
#' @author Michael Hock \email{michael.hock@@uni-bamberg.de}
#'
#' @export
getDataFrames <- function() {
    x <- ls(.GlobalEnv)
    ret <- x[sapply(x, function(x) is.data.frame(get(x)))]
    #### Todo
    if (length(ret) == 0) ret <- "No data frames found!"
    ret
}

#' Data frames in global workspace
#'
#' Returns of a vector with the names of the data frames present in the
#' global environment. If no data frame is found, Iana's example data
#' are loaded.
#'
#' @return A character vector with the names of the data frames
#'
#' @author Michael Hock \email{michael.hock@@uni-bamberg.de}
#'
#' @export
getDataFramesIana <- function() {
    x <- ls(.GlobalEnv)
    if (length(x) > 0) x <- x[sapply(x, function(x) is.data.frame(get(x)))]
    if (length(x) == 0) {
        load("data/ExampleData.RData", .GlobalEnv)
        x <- ls(.GlobalEnv)
        x <- x[sapply(x, function(x) is.data.frame(get(x)))]
    }
    if (length(x) == 0) ret <- "this should not happen: no data frames found"
    x
}


#' Degrees of freedom for a factor model
#'
#' Returns the degrees of freedom for a exploratory factor model, fit by maximum
#' likelihood. If the model cannot be fitted because it involves too many
#' observed variables a negative value is returned.
#'
#' @param p number of observed variables
#' @param m number of factors to be extracted
#'
#' @return degrees of freedom or a negative value.
#'
#' @author Michael Hock \email{michael.hock@@uni-bamberg.de}
#'
#' @export
dfEFA <- function(p, m) {
    # Degrees of freeddom for exploratory factor model (ML)
    # p: number of variables, m = number of factors
    # Brown 2009

    #     Dof <- function (p, m) {
    #         a <- (p*m) + ((m*(m+1)) / 2) + p -m^2 # Parameters in FA
    #         b <- (p*(p+1)) / 2 # Elements of cov matrix to use
    #         dif <- b - a
    #         list(a, b, dif)
    #
    #     }

    if (p < m) return(-1)
    dof <- 0.5 * ((p - m)^2 - p - m)
    dof
}

#' Frequencies
#'
#' Print tables with frequency counts of the unique values of the variables in a
#' data frame.
#'
#' @param x a data frame
#' @param max.unique print only variables with at most this number of unique
#'   values
#'
#' @author Michael Hock \email{michael.hock@@uni-bamberg.de}
#'
#' @export
#'
frequencies <- function (x, max.unique = 10) {
    if (!is.data.frame(x))
        stop("x must be a data frame.")
    cat("\nFrequencies\n")
    n.categories <- sapply(x, function(x) length(unique(x)))
    cols <- names(x)
    for (i in 1:ncol(x)) {
        cat("\n", cols[i])
        if (n.categories[i] <= max.unique)
            print(table(x[,i], useNA = "ifany"))
        else
            cat("\n    Variable was omitted because it has more than", max.unique, "unique values.")
    }
}

#' Velicer's MAP Test
#'
#' Perform Velicer's Minimum Average Partial test for the variables (usually items) in a data frame.
#'
#' @param x a data frame containing the variables (items) to be analyzed
#' @param n the maximum number of factors to consider (default: 20)
#'
#' @details The \code{plot} method produces a graphical representation of the squared correlations.
#'
#' @return A vector of class 'mapTest' containing the average squared (partial)
#'   correlations. The first element of the vector is the average squared
#'   correlation with no component removed.
#'
#' @author Computation: William Revelle, see \code{\link{psych}}, specifically \code{\link{VSS}}.
#' @author Plot: Michael Hock, \email{michael.hock@@uni-bamberg.de}
#'
#' @references Velicer, W. (1976) Determining the number of components from the matrix of partial correlations. \emph{Psychometrika, 41,} 321-327.
#' @export
#'
#' @examples
#' local({
#' set.seed(1234)
#' #
#' # Simulate 2 factors (f1, f2) underlying 6 items (x1 to x6).
#' #
#' n <- 200
#' f1 <- rnorm(n)
#' f2 <- rnorm(n)
#' Df <- data.frame(x1 = f1 + rnorm(n),
#'                  x2 = f1 + rnorm(n),
#'                  x3 = f1 + rnorm(n),
#'                  x4 = f2 + rnorm(n),
#'                  x5 = f2 + rnorm(n),
#'                  x6 = f2 + rnorm(n))
#' mt <- mapTest(Df)
#' print(mt)
#' plot(mt)
#'
#' #
#' # If we remove 1 item the MAP test suggests the wrong number of factors...
#' #
#' Df2 <- subset(Df, select = -x6)
#' mt2 <- mapTest(Df2)
#' print(mt2)
#' plot(mt2)
#' factanal(Df2, 2)
#' })
#'
mapTest <- function(x, n = 20) {
    x <- cor(x, use="pairwise")
    nvar <- dim(x)[2]

    mean.sqcor <- mean((x[lower.tri(x)])^2)
    mean.sqcor

    if (n >= nvar) n1 <- nvar - 1
    else n1 <- n

    min.partial <- rep(NA, n1)
    e <- eigen(x)
    evect <- e$vectors
    comp <- evect %*% diag(sqrt(e$values))
    for (i in 1:n1) {
        c11.star <- x - comp[,1:i] %*% t(comp[,1:i])
        d <- diag(1/sqrt(diag(c11.star)))
        rstar <- d %*% c11.star %*% d
        diag(rstar) <- 0
        min.partial[i] <- sum(rstar * rstar)  / (nvar*(nvar-1))
    }
    res <- c(mean.sqcor, min.partial)
    class(res) <- c("mapTest", "numeric")
    return(res)
}

### @rdname mapTest
#' @export
print.mapTest <- function(x, ...) {
    cat("Velicer's MAP test reaches its minimum with m =", which.min(x)-1, "components.\nMaximum number of components tested:", length(x) - 1, "\n\n")
    MAP <- data.frame(m = 0:(length(x) - 1), AP = round(x, 3))
    print(MAP, row.names = FALSE)
}

### @rdname mapTest
#' @export
plot.mapTest <- function(x, ...) {
    Component <- 0:(length(x) - 1)
    MAP <- data.frame(Component, x)
    # Compute ticks for x-axis
    myticks <- Component

    p <- ggplot(MAP, aes(Component, x)) +
        geom_line() +
        geom_point(size = 5,
                   shape = 21,
                   fill = "white") +
        #geom_line(alpha = c(rep(1, p), rep(0.2, p*simu))) +
        #        geom_point(size = c(rep(5, p), rep(0, p*simu)),
        #                   shape = c(rep(21, p), rep(21, p*simu)),
        #                   fill = "white") +
        geom_abline(intercept = x[1], slope = 0, colour = "darkblue",
                    linetype = "dashed") +
        theme(text = element_text(size = 20, colour = "black"),
              axis.title.x = element_text(vjust = 0.2),
              axis.title.y = element_text(vjust = 0.3),
              plot.title = element_text(vjust = 1.5)) +
        xlab("Components Removed") +
        #ylab(ylab) +
        scale_x_continuous(breaks=myticks) # + # Ticks
    #        ggtitle("MAP Test")

    print(p)
    ###p
}

#' Reliability
#'
#' Performs classical item and reliability analysis of set of items.
#'
#' @param x a numeric data frame in which rows represent persons and columns represent items to be analyzed
#' @param invert logical specifying whether items with negative loadings on the first principal component of the data should be inverted (default: TRUE)
#' @param digits number of digits to use in the output
#' @param dfname name of the data frame (only needed by \code{iana})
#'
#' @details \code{reliability} checks if items have negative loadings on the first principal component of the data and, if so, inverts these items.
#'
#' @author Michael Hock \email{michael.hock@@uni-bamberg.de}
#'
#' @export
reliability <- function(x, invert = TRUE, digits=3, dfname = NULL) {
    if (is.null(dfname)) dfname <- deparse(substitute(x))
    if (!is.data.frame(x)) stop("x must be a data frame.")
    n <- ncol(x)
    if (n < 2) stop("At least 2 items are needed.")
    cases.orig <- nrow(x)
    ##### x <- subset(x, complete.cases(x)) ### deparse won't work right
    ###x <- x[complete.cases(x),] ### same problem
    ###x <- as.data.frame(x)
    x <- na.omit(x)
    cases <- nrow(x)
    if (cases < cases.orig) {
        warning("Missing values encountered")
        cat("\nMissing values encountered.\n")
        cat("Using ", cases, " out of ", cases.orig, " cases.\n")
    }
    if (cases < 3) stop("At least 3 cases are needed.")

    loadings <- principal(x)$loadings[,1]
    if (any(loadings < 0)) {
        # Should be warning, but warnings are not shown in shiny...
        neg.items <- names(loadings[loadings < 0])
        cat("WARNING:\nThe following items have loadings < 0 on the first principal component:\n")
        cat(paste(neg.items, collapse = ", "))
        cat("\nYou may want to omit or invert these items.\n\nCode:\n")
        maxx <- max(x) + min(x)
        cat("# Assign the sum of the mininum and the maximum response to maxx.             \nmaxx <-", maxx, "\n")
        for (item in neg.items) {
            item <- paste0(dfname, "$", item)
            cat(paste(item, "<- maxx -", item, "\n"))
        }
        cat("# End of code\n")

        ## Invert items with negative loadings
        if (invert) {
            cat("\nNote: For the following analyses, these items were inverted.\n")
            ### Correct?
            x[,names(loadings[loadings < 0])] <- maxx - x[,names(loadings[loadings < 0])]
        }
    }

    # alpha using variance-covariance matrix
    M <- cov(x)
    alpha <- (n/(n - 1)) *  (1 - sum(diag(M)) / sum(M))

    # alpha using correlation matrix
    M <- cor(x)
    stand.alpha <- (n/(n - 1)) * (1 - sum(diag(M)) / sum(M))

    # item-total statistics
    row.sum <- apply(x, 1, sum)
    cov.it <- cov(x, row.sum)
    r.it <- cor(x, row.sum)

    # corrected item-total cov/cor and alpha if item removed
    r.it.c <- matrix(nrow=n, ncol=1)
    cov.it.c <- matrix(nrow=n, ncol=1)
    alpha.rm <- matrix(nrow=n, ncol=1)

    if (n > 2) {
        for (i in 1:n) {
            # corrected item-total cov/cor
            row.sum <- apply(x[,-i], 1, sum)
            cov.it.c[i,1] <- cov(x[,i], row.sum)
            r.it.c[i,1] <- cor(x[,i], row.sum)

            # alpha if item deleted
            M <- cov(x[,-i])
            alpha.rm[i,1] <- ((n-1)/(n-2)) * (1 - sum(diag(M)) / sum(M))
        }
    } else {
        warning('With only 2 items "corrected" statistics cannot be computed.')
    }

    item.means <- colMeans(x)
    item.sds <- apply(x, 2, sd)
    bad <- ifelse(alpha.rm > alpha, "*", "")
    badest <- which.max(alpha.rm)
    bad[badest] <- paste(bad[badest], "<=")
    bad <- str_pad(bad, 4, "right")
    itemstats <- data.frame(M = item.means,
                            SD = item.sds,
                            cov.it,
                            cov.it.c,
                            r.it,
                            r.it.c,
                            alpha.rm,
                            bad)

    old.ops <- options(digits=digits)
    on.exit(options(old.ops), add=TRUE)

    cat("\nCronbach's alpha is        ", alpha, "\n")
    cat("Standardized item alpha is ", stand.alpha, "\n")
    cat("\nItem statistics:\n\n")
    print(itemstats)
    cat("\nCOV/r_it: item-total covariance/correlation\nCOV/r_itc: 'corrected' item-total covariance/correlation\n    (with the respective item removed)\nalpha.rm: alpha if item removed\nbad: *item decreases alpha, <= item with mininum contribution to alpha\n")
    cat("\nCases: ", cases, "     Items: ", n, "\n")
    cat("\nStatistics for the total score (sums or means of item scores):\n\n")
    Total <- data.frame(Sum_score = rowSums(x), "Mean_score" = rowMeans(x))
    ###
###    require(psych)
    print(describe(Total))
    cat("\n")
    #invisible(alpha)
}

#' Parallel Analysis
#'
#' Performs parallel analysis of a set of items.
#'
#' @param Df a data frame containing the items
#' @param title title of the plot
#' @param use handling of missig values
#' @param simu number of simulations
#' @param xlab x-axis label of the plot
#' @param ylab y-axis label of the plot
#'
#' @details Adapted from \code{psy}-package (Bruno Falissard).
#'
#' @author Michael Hock \email{michael.hock@@uni-bamberg.de}
#'
#' @export
ggscree.plot <- function (Df, title = NULL,
                          use = "complete.obs", simu = 20,
                          xlab = "Component", ylab = "Eigenvalue") {
#     if (require(ggplot2) == FALSE) {
#         stop("Package ggplot2 must be installed.")
#     }
    mat <- Df
    if (use == "complete.obs")
        mat <- na.omit(Df)
    eigenval <- eigen(cor(mat, use = "pairwise.complete.obs"),
                      symmetric = TRUE)$values
    nev <- length(eigenval)

#     #   mik
#     eigenvaldata <- eigenval
#     eigenval
    #
    #plot(eigenval, type = "b", pch = 16, bty = "l", main = title,
    #     xlab = xlab, ylab = ylab)
    #lines(c(1, nev), c(1, 1), lty = 2)

    n <- dim(mat)[1]
    p <- dim(mat)[2]

    ev.mat <- matrix(nrow = p*(simu + 1), ncol = 2)
    ev.mat[, 1] <- rep(1:p, simu + 1)
    ev.mat[1:p, 2] <- eigenval

    matsimu <- matrix(nrow = n, ncol = p)
    int <- rep(1, n * p)
    attr(int, "dim") <- c(n, p)
    mat <- pmax(as.matrix(mat), int)
    newrow <- 1
    for (i in 1:simu) {
        matnorm <- rnorm(n * p)
        attr(matnorm, "dim") <- c(n, p)
        matsimu <- (mat/mat) * matnorm
        eigenval <- eigen(cor(matsimu, use = "pairwise.complete.obs"))$values
        newrow <- newrow + p
        ev.mat[newrow:(newrow + p - 1), 2] <- eigenval
    }

    eDf <- data.frame(ev.mat)
    ar = rep(1:(simu+1), each = p)
    ar <- factor(ar)
    eDf <- cbind(eDf, ar)
    names(eDf) <- c("Dimension", "Eigenvalue", "ar")

    # Compute ticks for x-axis
    if (ncol(Df) < 11) {
        myticks <- 1:ncol(Df)
    } else {
        incr <- round(ncol(Df)/10)
        myticks <- round(seq(from = 1, to = ncol(Df), by = incr))
    }

    pl <- ggplot(eDf, aes(Dimension, Eigenvalue, group = ar)) +
        geom_line(alpha = c(rep(1, p), rep(0.2, p*simu))) +
        geom_point(size = c(rep(5, p), rep(0, p*simu)),
                   shape = c(rep(21, p), rep(21, p*simu)),
                   fill = "white") +
        geom_abline(intercept = 1, slope = 0, colour = "darkblue",
                    linetype = "dashed") +
        theme(text = element_text(size = 20, colour = "black"),
              axis.title.x = element_text(vjust = 0.2),
              axis.title.y = element_text(vjust = 0.3),
              plot.title = element_text(vjust = 1.5)) +
        xlab(xlab) + ylab(ylab) +
        scale_x_continuous(breaks=myticks) # Ticks
    #        scale_shape_manual(values = c(21, 20))
    if (!is.null(title)) {
        pl <- pl + ggtitle(title)
    }
    print(pl)
    ###pl  ### remove?
}

#' Empirical ICCs
#'
#' Plot of empirical ICCs of the items in a data frame.
#'
#' @param x data frame in which rows are subjects and colums are test items
#' @param score total score to use
#' @param method function to use for curve fitting, e.g. loess or lm
#' @param alpha opaqueness of the points in the scatterplot
#' @param jitter should jitter be added to the points?
#'
#' @details To examine item characteristics, item scores are plotted against total scores or factor scores. To avoid overplotting, a small amount of jitter is added to overlapping points. (These are the light points in the plot.) In the upper left corner, the correlation of the total/factor score and the item score is given. The lines are locally weighted regression lines, the shaded regions represent 95\% confidence intervals around the expected item scores. If many data points are present it may be useful to decrease the opaqueness of the points and/or to deactivate jitter.
#'
#' @author Michael Hock \email{michael.hock@@uni-bamberg.de}
#'
#' @export
empICC <- function(x,
                   score = c("factor.thomson", "factor.bartlett", "mean", "sum"),
                   method = loess,
                   alpha = 0,
                   jitter = TRUE) {

#     if (!require(ggplot2)) stop("Package gglot2 must be installed to run empICC.")
#     if (!require(reshape2)) stop("Package reshape2 must be installed to run empICC.")
    #    if (!require(psych)) stop("Package psych must be installed to run empICC.")
    if (!is.data.frame(x)) stop("x must be a data frame.")

    x <- na.omit(x) ####

    score <- match.arg(score)
    if (score == "factor.thomson") {
        xlab <- ("Factor Score")
        fm <- factanal(x, 1, scores = "regression")
        scores <- fm$scores[,1]
    } else if (score == "factor.bartlett") {
        xlab <- ("Factor Score")
        fm <- factanal(x, 1, scores = "Bartlett")
        scores <- fm$scores[,1]
    } else if (score == "sum") {
        xlab <- ("Total (Sum) Score")
        scores <- rowSums(x, na.rm = TRUE) ####
    } else {
        xlab <- ("Total (Mean) Score")
        scores <- rowMeans(x, na.rm = TRUE) ####
    }

    variable <- factor(names(x))
    x <- cbind(scores, x)
    corrs <- round(cor(x)[-1,1], 2)
    corrs <- sprintf("r==%.2f", corrs)
    corrs <- data.frame(variable, corrs)

    x <- melt(x, id.vars = "scores")
    head(x)
    x.corrs <- min(x$scores)
    y.corrs <- max(x$value) + 0.25 ###
    #     require(lattice)
    #     xyplot(value ~ scores | variable, data = xx, type = c("p", "r"))
    #     xyplot(value ~ scores, group = variable, data = xx, type = c("p", "r"))

    #     qplot(scores, value, data = xx, facets = ~ variable ) +
    #         geom_smooth(method = loess)
    alpha.jitter <- alpha / 2
    p <- qplot(scores, value, data = x,
               facets = ~ variable, alpha = I(alpha),
               xlab = xlab, ylab = "Item Score")  +
        geom_text(data=corrs,
                  aes(x=x.corrs, y=y.corrs, label=corrs),
                  parse=TRUE, hjust = 0) +
        geom_smooth(method = method)
    if (jitter) p <- p + geom_jitter(alpha = alpha.jitter)
    #if (TRUE) p <- p + geom_point(alpha = 0.05)
    print(p)
}

plotICC.Rasch <- function(object, item.subset = "all",
    empICC = NULL, empCI = NULL, mplot = NULL,
    xlim = c(-4,4), ylim = c(0,1),
    xlab = "Dimension", ylab = "Probability",
    main=NULL,
    col = NULL, lty = 1, legpos = "left", ask = TRUE,
    ncols = 4, #### Mik, columns in mplot
    ...) {

    #### Mik
    plist.internal <- function(object,theta)
            # computes a list of expected probabilities for objects of class Rm
            # with 0th category included!
        {

            X <- object$X
            mt_vek <- apply(X,2,max,na.rm=TRUE)             #number of categories - 1 for each item
            mt_ind <- rep(1:length(mt_vek),mt_vek)


            #--------compute list matrix of probabilites for fixed theta)
            p.list <- tapply(object$betapar,mt_ind,function(beta.i) {
                beta.i <- c(0,beta.i)
                ind.h <- 0:(length(beta.i)-1)
                theta.h <- ind.h %*% t(theta)          #multiply category with
                #tb <- exp(theta.h-beta.i)
                tb <- exp(theta.h+beta.i)
                denom <- colSums(tb)
                pi.mat <- apply(tb,1,function(y) {y/denom})
                return(pi.mat)
            })
            return(p.list)
        }


    # plotICC.Rm from eRm 0.15-1

    X <- object$X
    if (is.null(col)) col <- 1:(max(apply(X,2,max,na.rm=TRUE))+1)
    main.arg <- main # rh added 2010-11-23 otherwise always same item in title if NULL

    # some sanity checks

    if (is.null(empICC)) {
        emp.plot <- FALSE

    } else if (!is.element(empICC[[1]], c("raw","loess","tukey","kernel"))) {
        ##empirical[[1]] <- "none"
        emp.plot <- FALSE
        warning('empICC must be one of "raw","loess","tukey","kernel"!\n')

    } else  if (object$model != "RM") {
        warning("Empirical ICCs can only be plotted for a dichotomous Rasch model!\n")
        emp.plot <- FALSE

    } else {

        th.est <- person.parameter(object)
        thetapar <- th.est$thetapar
        if (length(thetapar)!=1) {      #Too complicated with NA'groups (for each NAgroup separate plots...)
            warning("Empirical ICCs are not produced for different NA groups!\n")
            emp.plot <- FALSE
        } else {
            thetapar.u <- unique(round(unlist(thetapar),5))
            if (length(thetapar.u)<4) {
                warning("No empirical ICCs for less the 4 different person parameters!\n")
                emp.plot <- FALSE
            } else
                emp.plot <- TRUE

        }
    }

    theta <- seq(xlim[1],xlim[2],by=0.1)    #x-axis
    p.list <- plist.internal(object,theta)  #matrix of probabilities
    th.ord <- order(theta)

    if (any(item.subset=="all")) {
        textlab <- colnames(object$X)
        ivec <- 1:length(p.list)
    } else {
        if (is.character(item.subset)) {     #item names specified
            ivectemp <- t(as.matrix(1:length(p.list)))
            colnames(ivectemp) <- colnames(object$X)
            ivec <- ivectemp[,item.subset]
            textlab <- item.subset
            textlab[ivec] <- textlab
            it.legend <- item.subset
        } else {                        #numeric vector specified
            textlab <- colnames(object$X)[item.subset]
            textlab[item.subset] <- textlab
            ivec <- item.subset
        }
    }

    if (object$model=="RM") {                          #Rasch model
        p.list <- lapply(p.list,function(x) {x[,-1]})  #Delete 0-probabilites
        p.mat <- matrix(unlist(p.list),ncol=length(p.list))  #matrix with solving probabilities
        text.ylab <- p.mat[(1:length(theta))[theta==median(theta)],]
    }

    ## plot for non RMs #################
    if (object$model != "RM"){
        if (ask) par("ask"=TRUE)                                 # added rh 2007-12-01
        if (is.null(mplot)) mplot<-FALSE
        if (mplot) par(mfrow=c(2,2))
        for (j in 1:length(ivec)) {                                 # loop for items
            i <- ivec[j]

            yp <- as.matrix(p.list[[i]])
            yy <- yp[th.ord,]

            if(is.null(main.arg)) main<-paste("Item ",
                textlab[i])    # rh 2010-03-06
            matplot(sort(theta),yy,type="l",lty=lty,col=col,
                main=main, xlim=xlim,
                ylim=ylim,xlab=xlab,ylab=ylab,
                ...)
            if (is.character(legpos))
                legend(legpos,legend=paste(c("Category"),0:(dim(yp)[2]-1)), col=col,lty=lty, ...)  # added rh 2007-12-01
        }

        ## plot for  RMs #####################
    } else {
        if (is.null(mplot)) mplot <- TRUE       ### FIX MM 2012-03-18
        if (length(ivec) == 1) mplot <- FALSE   ### FIX MM 2012-03-18

        #### Mik:
        #### if (mplot) par(mfrow=c(2,2))
        nrows <- ceiling(length(ivec) / ncols)
        if (mplot) par(mfrow=c(nrows, ncols))
        ####

        if (ask) par("ask"=TRUE)                   # added rh 2007-12-01
        for (j in 1:length(ivec)) {                # runs over items
            i <- ivec[j]

            yp <- as.matrix(p.list[[i]])
            yy <- yp[th.ord,]
            if(is.null(main.arg)) main<-paste("Item ",textlab[i])    # rh 2010-03-06

            par(cex = 1.05) #### Mik
            matplot(sort(theta),yy,type="l",lty=lty,col=col,
                main=main, xlim=xlim,
                ylim=ylim,xlab=xlab,ylab=ylab,
                ...)
            ##ylim=ylim,xlab=xlab,ylab=ylab,"ask"=TRUE,...)

            ## empirical ICC
            if (emp.plot) {
                freq.table <- as.matrix(table(rowSums(X),X[,i]))
                rel.freq <- freq.table[,2]/rowSums(freq.table)
                idx <- as.numeric(rownames(freq.table))
                xy<-cbind(th.est$pred.list[[1]]$y[idx+1],rel.freq)


                if(empICC[[1]]=="loess")
                    if(!is.null(empICC$smooth)) smooth<-empICC$smooth else smooth<-0.75
                if(empICC[[1]]=="kernel")
                    if(!is.null(empICC$smooth)) smooth<-empICC$smooth else smooth<-0.5

                nn <- rowSums(freq.table)
                switch(empICC[[1]],
                    "raw"={},
                    "loess"={xy[,2]<-loess(xy[,2]~xy[,1],span=smooth)$fitted},#+;cyf<-cbind(xy[,2] * nn, nn)},
                    "tukey"={xy[,2]<-smooth(xy[,2])},#;cyf<-cbind(xy[,2] * nn, nn)}
                    "kernel"={xy[,2]<-ksmooth(xy[,1],xy[,2],bandwidth=smooth,x.points=xy[,1])[[2]]}
                )
                xy[,2] <- ifelse(xy[,2]>1,1,ifelse(xy[,2]<0,0,xy[,2])) # bounding p in [0,1]

                if(is.null(empICC$type)) empICC$type <- "p"
                if(is.null(empICC$pch)) empICC$pch <- 1
                if(is.null(empICC$col)) empICC$col <- "black"
                if(is.null(empICC$lty)) empICC$lty <- "solid"


                # confidence intervals for empirical ICC
                if(!is.null(empCI)) {
                    # functions from prop.test()
                    p.L <- function(x, n, alpha) {
                        if (x <= 0) 0 else qbeta(alpha, x, n - x + 1)}
                    p.U <- function(x, n, alpha) {
                        if (x >= n) 1 else qbeta(1 - alpha, x + 1, n - x)}
                    CINT <- function(x, n, conf.level){
                        alpha <- (1 - conf.level)/2
                        c(p.L(x,n, alpha), p.U(x,n, alpha))
                    }

                    if(is.null(empCI$clevel)) empCI$clevel <- 0.95
                    if(is.null(empCI$col)) empCI$col <- "red"
                    if(is.null(empCI$lty)) empCI$lty <- "dotted"


                    cyf<-cbind(xy[,2] * nn, nn)
                    cy<-apply(cyf,1,function(x) CINT(x[1],x[2],empCI$clevel))


                    apply(cbind(xy[,1],t(cy)),1,function(x)segments(x[1],x[2],x[1],x[3],lty=empCI$lty,col=empCI$col))
                }

                # plots the point estimates of the empirical ICC
                lines(xy[,1],xy[,2],type=empICC$type, pch=empICC$pch, col=empICC$col, lty=empICC$lty, ...)


            } # end if(emp.plot)
        }
    }
    ## reset graphics parameters
    par("ask"=FALSE) # added rh 2007-12-01
    par(mfrow=c(1,1))
    par(cex = 1)
}

############################################################################
plotICC.RM <- function(object, item.subset = "all",
    empICC = NULL, empCI = NULL,
    xlim = c(-4,4), ylim = c(0,1),
    xlab = "Dimension", ylab = "Probability") {

    #### Mik
    plist.internal <- function(object,theta)
        # computes a list of expected probabilities for objects of class Rm
        # with 0th category included!
    {

        X <- object$X
        mt_vek <- apply(X,2,max,na.rm=TRUE) # number of categories - 1 for each item
        mt_ind <- rep(1:length(mt_vek),mt_vek)


        #--------compute list matrix of probabilites for fixed theta)
        p.list <- tapply(object$betapar,mt_ind,function(beta.i) {
            beta.i <- c(0,beta.i)
            ind.h <- 0:(length(beta.i)-1)
            theta.h <- ind.h %*% t(theta)
            tb <- exp(theta.h+beta.i)
            denom <- colSums(tb)
            pi.mat <- apply(tb,1,function(y) {y/denom})
            return(pi.mat)
        })
        return(p.list)
    }


    # plotICC.Rm from eRm 0.15-1
############################################################################
    if (object$model != "RM") stop("object must be Rasch model (RM)")

    X <- object$X
#    if (is.null(col)) col <- 1:(max(apply(X,2,max,na.rm=TRUE))+1)
#    main.arg <- main

    # some sanity checks

    if (is.null(empICC)) {
        emp.plot <- FALSE
    } else if (!is.element(empICC[[1]], c("raw","loess","tukey","kernel"))) {
        emp.plot <- FALSE
        warning('empICC must be one of "raw","loess","tukey","kernel"')
    } else {
        th.est <- person.parameter(object)
        thetapar <- th.est$thetapar
        if (length(thetapar)!= 1) {
            warning("empirical ICCs are not produced for different NA groups")
            emp.plot <- FALSE
        } else {
            thetapar.u <- unique(round(unlist(thetapar),5))
            if (length(thetapar.u)<4) {
                warning("No empirical ICCs for less the 4 different person parameters")
                emp.plot <- FALSE
            } else
                emp.plot <- TRUE
        }
    }

    theta <- seq(xlim[1],xlim[2], by = 0.1) # x-axis
    p.list <- plist.internal(object,theta)  # matrix of probabilities
    th.ord <- order(theta) #### Warum das?

    if (any(item.subset=="all")) {
        textlab <- colnames(object$X)
        ivec <- 1:length(p.list)
    } else {
        if (is.character(item.subset)) {     # item names specified
            ivectemp <- t(as.matrix(1:length(p.list)))
            colnames(ivectemp) <- colnames(object$X)
            ivec <- ivectemp[,item.subset]
            textlab <- item.subset
            textlab[ivec] <- textlab
            it.legend <- item.subset
        } else {                        # numeric vector specified
            textlab <- colnames(object$X)[item.subset]
            textlab[item.subset] <- textlab
            ivec <- item.subset
        }
    }

    p.list <- lapply(p.list,function(x) {x[,-1]})  # Delete 0-probabilites
    p.mat <- matrix(unlist(p.list),ncol=length(p.list))  # matrix with solving probabilities
    text.ylab <- p.mat[(1:length(theta))[theta==median(theta)],]

    ## plot for RMs #################
    for (j in 1:length(ivec)) { # runs over items
        i <- ivec[j]
        yp <- as.matrix(p.list[[i]])
        yy <- yp[th.ord,]

        ####################################################################
        #### ICC: 'model', 'empirical'
        if (j == 1)
            mikPlotData <- data.frame(
                ICC = factor(rep("Model", length(theta)),
                    levels = c("Empirical", "Model")),
                Item = rep(textlab[i], length(theta)),
                Theta = sort(theta), Probability = yy)
        else
            mikPlotData <- rbind(mikPlotData,
                data.frame(
                    ICC = factor(rep("Model", length(theta)),
                        levels = c("Empirical", "Model")),
                    Item = rep(textlab[i], length(theta)),
                    Theta = sort(theta), Probability = yy))
        ####

        ## empirical ICC
        if (emp.plot) {
            freq.table <- as.matrix(table(rowSums(X),X[,i]))
            rel.freq <- freq.table[,2]/rowSums(freq.table)
            idx <- as.numeric(rownames(freq.table))
            xy<-cbind(th.est$pred.list[[1]]$y[idx+1],rel.freq)


            if(empICC[[1]]=="loess")
                if(!is.null(empICC$smooth)) smooth <- empICC$smooth else smooth <- 0.75
            if(empICC[[1]]=="kernel")
                if(!is.null(empICC$smooth)) smooth<-empICC$smooth else smooth<-0.5

            nn <- rowSums(freq.table)
            switch(empICC[[1]],
                "raw"={},
                "loess"={xy[,2]<-loess(xy[,2]~xy[,1],span=smooth)$fitted},
                "tukey"={xy[,2]<-smooth(xy[,2])},
                "kernel"={xy[,2]<-ksmooth(xy[,1],xy[,2],bandwidth=smooth,x.points=xy[,1])[[2]]}
            )
            xy[,2] <- ifelse(xy[,2]>1,1,ifelse(xy[,2]<0,0,xy[,2])) # bounding p in [0,1]

#            if(is.null(empICC$type)) empICC$type <- "p"
#            if(is.null(empICC$pch)) empICC$pch <- 1
#            if(is.null(empICC$col)) empICC$col <- "black"
#            if(is.null(empICC$lty)) empICC$lty <- "solid"


            # confidence intervals for empirical ICC
            if(!is.null(empCI)) {
                # functions from prop.test()
                p.L <- function(x, n, alpha) {
                    if (x <= 0) 0 else qbeta(alpha, x, n - x + 1)}
                p.U <- function(x, n, alpha) {
                    if (x >= n) 1 else qbeta(1 - alpha, x + 1, n - x)}
                CINT <- function(x, n, conf.level){
                    alpha <- (1 - conf.level)/2
                    c(p.L(x,n, alpha), p.U(x,n, alpha))
                }

                if(is.null(empCI$clevel)) empCI$clevel <- 0.95
 #               if(is.null(empCI$col)) empCI$col <- "red"
 #               if(is.null(empCI$lty)) empCI$lty <- "dotted"


                cyf<-cbind(xy[,2] * nn, nn)
                cy<-apply(cyf,1,function(x) CINT(x[1],x[2],empCI$clevel))


###                apply(cbind(xy[,1],t(cy)),1,function(x)segments(x[1],x[2],x[1],x[3],lty=empCI$lty,col=empCI$col))
            }

            #################################################################
#             # plots the point estimates of the empirical ICC
#             lines(xy[,1],xy[,2],type=empICC$type, pch=empICC$pch, col=empICC$col, lty=empICC$lty, ...)
#             ####

            len <- length(xy[,1])
            mikPlotData <- rbind(mikPlotData,
                data.frame(
                    ICC = factor(rep("Empirical", len,
                        levels = c("Empirical", "Model"))),
                    Item = rep(textlab[i], len),
                    Theta = xy[,1], Probability = xy[,2]))

#             geom_line(alpha = c(rep(1, p), rep(0.2, p*simu))) +
#                 geom_point(size = c(rep(5, p), rep(0, p*simu)),
#                     shape = c(rep(21, p), rep(21, p*simu)),
#                     fill = "white") +

            ####

        }
    }
    onlyEmp <- mikPlotData[as.character(mikPlotData$ICC) == "Empirical",]
    myplot <- qplot(Theta, Probability, colour = ICC,
        facets = ~ Item,
        data = mikPlotData,
        geom = "line",
        xlab = xlab, ylab = ylab) +
        geom_point(data = onlyEmp, size = 4,
            shape = 21,
            fill = "white")

    print(myplot)
}


# Item Classification -----------------------------------------------------

#' Subsetting Items
#'
#' Returns a subset of items in a data frame with the \code{item.text} attribute preserved.
#'
#' @param x a data frame containing the items to be subsetted
#' @param ... arguments passed to \code{\link{subset}}
#'
#' @return a data frame containing the subsetted items
#'
#' @seealso \code{\link{setItemText}} for setting and \code{\link{getItemText}} for retrieving \code{item.text} attributes.
#'
#' @author Michael Hock \email{michael.hock@@uni-bamberg.de}
#'
#' @export
#'
subsetItem <- function(x, ...) {
    ### better use standard subsetting?
    if (!is.data.frame(x)) stop("x must be a data frame.")
    ### for data frame:
    ### old.item.text <- attr(x, "item.text")
    old.item.text <- getItemText(x)
    newdf <- subset(x, ...)
    if (!is.null(old.item.text)) {
        pos <- match(names(newdf), names(x))
        new.item.text <- old.item.text[pos]
        ### attr(newdf, "item.text") <- new.item.text
        newdf <- setItemText(newdf, items = new.item.text)
    }
    newdf
}

#' Associate variables with text
#'
#' \code{setItemText} can be used to associate text (usually the item stem or a description of it) with the variables (items) in a data frame.
#'
#' @param x a data frame
#' @param items either a character vector containing the descriptions of the items (columns in the data frame) or the name of a file containing the descriptions for the items. If a file name is given, each descriptions must occupy one line and each item in the data frame must have a description.
#'
#' @details Technically, \code{setItemText} sets the \code{item.text} attribute for the variables (items) in a data frame, which corresponds to "variable labels" known form other statistical systems. This text is then displayed along with the variable names in functions such as \code{\link{classifyItems}} as a mnemonic for the content of the item.
#'
#' The text can be specified either as a character vector of the same length as the number of the columns in the data frame or in a text file that contains the descriptions of the items. The file is read via \code{\link{read.table}}, with the separator set to a newline character (i.e., \code{"\n"}) Consequently, each description must occupy exactly one physical line (which may, of course, span several display lines). The number of descriptions in the file and the number of items must be the same.
#'
#' Notice that attributes are lost when data frames are subsetted via \code{\link{subset}}. For preserving the \code{item.text} attribute, \code{\link{subsetItem}} can be used instead of \code{\link{subset}}.
#'
#' @return A data frame with the \code{item.text} attribute set for variable.
#'
#' @seealso \code{\link{getItemText}} for retrieving, and \code{\link{subsetItem}} for preserving \code{item.text} attributes.
#'
#' @author Michael Hock \email{michael.hock@@uni-bamberg.de}
#'
#' @export
#'
setItemText <- function(x, items = NULL) {
    if (!is.data.frame(x)) stop("x must be a data frame.")
    if (is.null(items)) stop("no items supplied")
    if(length(items) > 1) {
        if(!is.character(items) || length(items) != ncol(x))
            stop("'items' must be a character vector of the same length as the number of columns in x or the name of a text file")
    } else {
        if (!file.exists(items)) stop ("specified file does not exist")
        items <- read.table(items, sep="\n", as.is=TRUE)[,1]
        if (length(items) != ncol(x)) stop(paste0("number of items and columns in data frame do not match. Number of items =", length(items), ", number of columns =", ncol(x)))
    }
    for (i in 1:ncol(x))
        attr(x[,i], "item.text") <- items[i]
    invisible(x)
}

print.itemText <- function(x, ...) {
    print(data.frame(Item = x), right = FALSE)
}

#' Return the labels associated with items
#'
#' Return the labels associated with items in a data frame.
#'
#' @param x a data frame
#'
#' @return a character vector of class 'itemText' containing the labels of the items
#'
#' @author Michael Hock \email{michael.hock@@uni-bamberg.de}
#'
#' @export
getItemText <- function(x) {
    ### to do: also allow a single item
    if (!is.data.frame(x)) stop("x must be a data frame")
    itemtext <- sapply(X = x, FUN = attr, which = "item.text")
    ### For data frame:
#     itemtext <- attr(x, "item.text")
#     if (is.null(itemtext)) {
#         message("x has no 'item.txt' attribute")
#         return()
#     }
    if (is.list(itemtext)) return(NULL) ### ?
    #itemtext <- ifelse(is.null(itemtext), "No item text given", itemtext)
    #cat(itemtext, "---", colnames(x))
    names(itemtext) <- colnames(x)
    class(itemtext) <- c("itemText", "character")
    return(itemtext)
}

#' Classify items
#'
#' Automatically classify the items in a data frame.
#'
#' @param factormodel a factor model fitted by factanal of fa
#' @param Df a data frame containing the items
#' @param minloading minimum loading of an item to be considered a marker of a factor
#' @param maxloading maximum loading of an item on a secondary factor (i,e., the factor on which the items has its second highest loading) to be considered a marker for the primary factor (i,e., the factor on which the items has its highest loading)
#' @param minpurity mininum purity for a marker
#' @param itemlength trim item text to given number characters
#' @param Df.name name of data frame
#'
#' @details The purity is computed as
#' \deqn{(a_1^2 - a_2^2) / h^2,}
#' where \eqn{a_1} and \eqn{a_2} are the highest and second highest loadings of the item, respectively, and \eqn{h 2} is the communality of the item.
#'
#' @author Michael Hock \email{michael.hock@@uni-bamberg.de}
#'
#' @export
classifyItems <- function(factormodel, Df, minloading = 0.4, maxloading = .40, minpurity = 0.25, itemlength = 0, Df.name = deparse(substitute(Df))) {

    ### Todo: is Df needed? We now have the names of the items in attr
    ### itemText
    max2 <- function(x) {
        y <- sort(x, decreasing=TRUE)
        y[2]
    }

    lmat <- factormodel$loadings # for factanal()
    if (is.null(lmat)) {
        lmat <- factormodel$fa$loadings # for psych, fa.poly
        communality <- factormodel$fa$communality
        cat("\nUsing loadings from fa.poly in package psych.\n")
    } else {
        communality <- 1 - factormodel$uniquenesses
    }
    if (ncol(lmat) < 2) {
        cat("\nOnly 1 factor was extracted, so no classification table is needed.\n")
        return()
    }

    cat("\nClassification Table\n")
    cat("====================\n")
    cat("F = Factor number, M = Marker, a1/a2 = Highest loadings\n")
    cat("Pur = Purity measure\n")

    ilength = getOption("width") - 25 - max(nchar(names(Df)))
    items <- getItemText(Df)
    if (is.null(items)) {
        shortitems <- rep("-", length(communality))
        cat("\nHint: You can associate the text of the items with the columns \nof the data frame with 'setItemText()'. \nThis would allow to produce an item table.\n")
    } else {
        items <- stringr::str_trim(items)
        shortitems <- str_pad(strtrim(items, ilength), width = ilength,
                              side = "right")
    }

    fac <- apply(abs(lmat), 1, which.max)
    maxabsload <- apply(abs(lmat), 1, max)
    maxabsload2 <- apply(abs(lmat), 1, max2)
    diffsqload <- maxabsload^2 - maxabsload2^2
    purity <- diffsqload / communality
    marker <- ifelse( (maxabsload > minloading) & (maxabsload2 < maxloading) & (purity > minpurity), "*", "-")
    x <- data.frame(fac, marker, maxabsload, maxabsload2, purity, shortitems)
    names(x) <- c("F", "M", "a1", "a2", "Pur", "Item")
    cat("\n")
    print(x[order(x$F, -x$a1), ], digits = 2)
    cat("\nNote:\nThe following code may be used to create data frames of items\nassigned to the factors. Some items may need to be inverted.\n\n")

    for (i in (1:ncol(lmat))) {
        selected <- row.names(x[(x$F == i) & (x$M == "*"), ])
        selected <- paste(selected, collapse = ", ")
        cat("F", i, " <- subset(", Df.name, ", select = c(", selected, "))\n", sep = "")
    }

    # Item Table
    if (!is.null(items)) {
        if (itemlength != 0) {
            shortitems <- str_pad(strtrim(items, itemlength),
                                  width = itemlength, side = "right")
        }
    }
    cat("\nItem Table and Loadings\n")
    cat("=======================\n")
    M <- matrix(nrow=nrow(lmat), ncol = 1+ncol(lmat))
    classified <- numeric(nrow(lmat))

    for (i in 1:ncol(lmat)) {
        cat("\nFactor", i, "\n")
        k <- 0
        for (j in 1:nrow(lmat)) {
            if (fac[j] == i && marker[j] == "*") {
                k <- k+1
                #cat(k, "-", shortitems[j], round(lmat[j,],2), "\n")
                M[k,] <- c(shortitems[j], round(lmat[j,],2))
                classified[j] <- 1
            }
        }
        if (k > 0) {
            Df <- as.data.frame(M)[1:k,]
            names(Df) <- c("Item", paste0("a", 1:ncol(lmat)))
            print(Df)
        }
    }
    k <- 0
    for (j in 1:nrow(lmat)) {
        if (classified[j] == 0) {
            k <- k+1
            #cat(k, "-", shortitems[j], round(lmat[j,],2), "\n")
            M[k,] <- c(shortitems[j], round(lmat[j,],2))
        }
    }
    if (k > 0) {
        cat("\nNot classified\n")
        Df <- as.data.frame(M)[1:k,]
        names(Df) <- c("Item", paste0("a", 1:ncol(lmat)))
        print(Df)
        cat("") # Without this, we get an error!
    } else cat("\nAll items were classified\n")
}
