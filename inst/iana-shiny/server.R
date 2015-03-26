library(shiny)
require(lattice)
require(ggplot2)
# require(GPArotation)
require(lavaan)
require(eRm)
# require(markdown)
# require(reshape2)
require(stringr)
# require(semTools)
# require(polycor)
require(psych)

#load("data/ExampleData.RData", .GlobalEnv)
#load("data/ExampleData.RData")

# logFile <- "log_reliab.txt"
# cat("Log file for reliab\n", file = logFile)
# log.output <- function(procname = "No proc") {
#     t <- proc.time()[3]
#     cat(procname, t, "\n", file = logFile, append = TRUE)
# }

log.output <- function(x = "") {
    t <- proc.time()[3]
    cat("===", x, t, "\n", file = stderr())
}

cmdLogFile <- paste(tempdir(), "iana_cmd_log.R", sep = "/")
.old.mycmd <<- ""
cat("# Command Log for IANA\n\n", file = cmdLogFile)
cmdLog <- function(cmd.string = "") {
    cat(cmd.string, "\n", file = cmdLogFile, append = TRUE)
}

shinyServer(function(input, output) {

    ###load("data/ExampleData.RData", .GlobalEnv)

    # Data ####

    getSelectedDf <- reactive({
        get(input$selectedDf)
    })


    getLikertLikeVars <- function(Df, unique.values = 9) {
        # Returns the names of all numeric variables in Df
        # with less than the specified number of unique values. If this
        # number is > 19, the names of all numeric variables are returned.

        maybeLikert <- function(x, unique.values = 9) {
            is.int <- function(x) {
                if (is.numeric(x)) {
                    ceiling(x) == floor(x)
                    #round(x) == x
                } else {
                    return(FALSE)
                }
            }
            if (unique.values > 19) {
                is.numeric(x)
                #is.int(x)
            } else {
                is.int(x) &&
                    length(unique(x)) <= unique.values &&
                    min(x) >= 0 &&
                    max(x) <= unique.values
            }
        }

        log.output("getlikertlikevars")
        Df <- na.omit(Df) ### should be be done on column level (?)
        ### to do: what do we do if no values are left?
        if (nrow(Df) == 0) return(NULL)
        ###

        numvars <- logical(ncol(Df))
        for (i in 1:ncol(Df)) {
            if (maybeLikert(Df[,i], unique.values)) numvars[i] <- TRUE
        }
        varnames <- names(Df)[numvars]
    }

    getVarsInDf <- reactive({
        log.output("getVarsInDf")
        varnames <- getLikertLikeVars(getSelectedDf(), input$kUniqueValues)
        if (is.null(varnames)) {
            log.output("getVarsInDf: varnames is NULL")
            varnames <- NULL
        }
        varnames
    })

    getVarRange <- reactive({
        log.output("getVarRange")
        if (input$varrange == "") {
            varrange <- getVarsInDf()
            if (length(varrange) > 80) varrange <- varrange[1:80]
        } else {
            newDf <- try(subset(getSelectedDf(),
                                select = eval(parse(text = input$varrange))),
                         silent = TRUE)
            if (class(newDf) == "try-error") {
                log.output("Error in getVarRange")
                return()
            } else {
                varrange <- names(newDf)
            }
        }
        varrange
    })

    output$varsindf <- renderUI({
        log.output("in outputvarsindf")
        varsInDf <- getVarsInDf()
        if (is.null(varsInDf)) return()

        checkboxGroupInput(inputId = "varnamesindf",
                           label = "Variables to use:",
                           choices = varsInDf,
                           selected = getVarRange())
    })

    checkedVars <- reactive({
        # Return the checked variables. If these need to updated return NULL.
        # Intended to be called at the beginning of statistical functions
        # to make sure that the variables are already in the selected
        # data frame.

        log.output("checkedvars")
        vnames <- input$varnamesindf
        res <- vnames %in% getLikertLikeVars(getSelectedDf(), input$kUniqueValues)
        if (all(res) == TRUE) return(vnames)
        return(NULL)
    })

    getSubset <- function(vnames, .reliab.data.frame.name, minNoVars = 2,
                          return.cases.only = FALSE) {
        #### hidden .data.frame.name to avoid error when Df already exists ####
        #### Look at getVarrange for an alternative. Also: reactive function? ####
        log.output("getSubset (1)")
####        input$applyButton
        ##################################
####isolate({
        if (is.null(vnames) || length(vnames) < minNoVars) return()
        vnames <- sub(" .*", "", vnames)
        vnames <- shQuote(vnames)

        myvars <- paste(vnames, collapse = ", ")

        #### complete.cases here? or after the variables have been selected?
        Df0 <- get(.reliab.data.frame.name)
        nrow0 <- nrow(Df0)
        mycmd <- paste0("subsetItem(",
                       .reliab.data.frame.name,
                       ", \n  subset = complete.cases(",
                       .reliab.data.frame.name,
                       "), \n  select = c(",
                       myvars,
                       "))")

        Df <- try(eval(parse(text = mycmd)), silent = TRUE)
        if (class(Df) == "try-error") {
            log.output("CATCH Start ..............................................")
            log.output(Df)
            log.output(paste0("Cmd was: ", mycmd))
            log.output("CATCH End   ..............................................")
            return()
        }
        nrow1 <- nrow(Df)
        log.output(paste("getSubset (2): cases:", nrow0, "complete cases:", nrow1))
        if (return.cases.only) return(list(nrow0 = nrow0, nrow1 = nrow1))
        #####
        #### Called to often!
        if (mycmd != .old.mycmd) {
            cmdLog(paste0("\n#---------------------",
                          "\n# Data have changed...",
                          "\n#---------------------",
                          "\nmyData <- ",
                          mycmd, "\n"))
        }
        .old.mycmd <<- mycmd
        ###.currentSubsetCmd <<- mycmd
        #####
        Df
####})                
    }


    output$casesindf <- renderUI({
        log.output("in outputcasesindf")

        cases <- getSubset(checkedVars(), input$selectedDf,
                           return.cases.only = TRUE)
        ####if (is.null(cases)) return()
        if (is.null(cases)) {
            msg <- "WARNING: No cases with complete data found! Missing values should be handled before the data are analyzed."
###            helpText(msg)
###            return()
        } else {

            if (cases$nrow0 == cases$nrow1)
                msg <- paste0(cases$nrow0, " cases in data frame.")
            else
                msg <- paste0("WARNING: Data frame contains missing values. These should be handled before the data are analyzed. Using ", cases$nrow1, " out of ", cases$nrow0, " cases.")
        }
        helpText(msg)
    })


#     setPlotHeight <- function() {
#         #x <- getSubset(checkedVars(), input$selectedDf, 0)
#         #if (is.null(x)) return(500)
#         return(800)
#     }

##################################################################
#     # Plot height
#     setPlotHeightMultipanel <- reactive({
#         log.output("Plot height changed")
#         xxxplotheight <<- paste0(input$plotHeightMultipanel, "px")
#     })
#     output$phmp <- reactivePrint(function() {
#         log.output("Plot height set")
#         h <- setPlotHeightMultipanel()
#         cat(h)
#     })

    # Item text and frequencies ####

    output$itemtext <- renderPrint({
        log.output("ITEMTEXT")
        x <- getSubset(checkedVars(), input$selectedDf)
        if (is.null(x)) return()
        #cmdLog("# Reliability")
        #cmdLog("reliability(myData)\n")
        res <- getItemText(x)
        if (is.null(res)) cat("Items have no associated text. Use 'setItemText()' to attach text to items.\n")
        else {
            for (i in 1:length(res)) {
                cat("Item: ", names(x)[i])
                Text <- strwrap(str_trim(res[i]), width = 75, 3, 3)
                cat("\n", paste0(Text, "\n"), "\n")
            }
        }
        cat("")
    })

    output$frequencies <- renderPrint({
        log.output("FREQUENCIES")
        x <- getSubset(checkedVars(), input$selectedDf)
        if (is.null(x)) return()
        #cmdLog("# Reliability")
        #cmdLog("reliability(myData)\n")
        frequencies(x)
        #cat("==============\n")
    })

    # Reliability ####

    output$reliability <- renderPrint({
        log.output("REL")
        x <- getSubset(checkedVars(), input$selectedDf)
        if (is.null(x)) return()
        cmdLog("# Reliability")
        if (input$reliabDetailed){
            cmdLog("alpha(myData)\n")
            print(alpha(x)) #### dfname?
            cat("") # needed for shiny
        } else {
            cmdLog("reliability(myData)\n")
            iana::reliability(x, dfname = input$selectedDf)
        }
    })

    # Histogram ####

    output$hist <- renderPlot({
        log.output("Hist")

        x <- getSubset(checkedVars(), input$selectedDf, 1)
        if (is.null(x)) return()

        vnames <- names(x)
        myvars <- paste(vnames, collapse = " + ")
        myformula <- paste0("~ " , myvars)

        ###mydata <- getSelectedDf()
        mydata <- x ### Do we really need this copy?
        for (vn in vnames) mydata[,vn] <- as.factor(mydata[,vn])
        htype <- input$histtypeitem

#         print(do.call("histogram",
#                       args = list(x = as.formula(myformula),
#                                   xlab = "Response",
#                                   data = mydata,
#                                   as.table = TRUE #,
#                                   #layout = c(4, NA)
#                       )
#         )
#         )

        cmdLog("# Histogram")
        cmdLog(paste0("histogram(", myformula,
                      ",\n    xlab = 'Response', data = myData, type = ",
                      htype, " , as.table = TRUE",
                      ")\n")
        )
        ### trellis.par.set(canonical.theme(color = FALSE))
#         ltheme <- canonical.theme(color = FALSE) ## in-built B&W theme 
#         ltheme$strip.background$col <- "transparent" ## change strip bg 
#         lattice.options(default.theme = ltheme) ## set as default 
        print(histogram(as.formula(myformula),
                        xlab = "Response",
                        data = mydata,
                        type = htype,
                        as.table = TRUE)
              )
    })

    output$histTotal <- renderPlot({
        log.output("Hist Total")

        x <- getSubset(checkedVars(), input$selectedDf)
        log.output(names(x))
        if (is.null(x)) return()

        sumScore <- rowSums(x, na.rm = TRUE) ####
        if (input$totalscoretype == "sum") Total <- sumScore
        else Total <- rowMeans(x, na.rm = TRUE) ####
        htype <- input$histtype
        bins <- input$histbins
        if (bins < 4) bins <- round(log2(length(Total)) + 1) # Default
        # This first max is only valid for integer scores, the second
        # is not always okay.
        maxbins <- max(1 + max(sumScore) - min(sumScore),
                       length(unique(Total)))
        # take the larger of both???
        if (bins > maxbins) bins <- maxbins
        if (htype == "density") {
            print(histogram(~ Total, xlab = "Total Score",
                            type = "density",
                            nint = bins,
                            panel = function(x, ...) {
                                panel.histogram(x, ...)
                                panel.mathdensity(dmath = dnorm,
                                                  col = "red", lwd = 2,
                                                  args = list(mean=mean(x),sd=sd(x)))
                            }))
        } else {
            print(histogram(~ Total, type = htype, nint = bins,
                            xlab = "Total Score"))
        }
    })

    # ICCs ####

    output$ICCs <- renderPlot({
        log.output("ICCs")

        x <- getSubset(checkedVars(), input$selectedDf)
        if (is.null(x)) return()

        if (input$ICClinear) {
            empICC(x, input$ICCscore, method = lm, alpha = input$ICCalpha,
                   jitter = input$ICCjitter)
        } else {
            empICC(x, input$ICCscore, alpha = input$ICCalpha,
                   jitter = input$ICCjitter)
        }
    }, res = 96) ### check res


    # Parallel analysis ####

    output$parallelanalysis <- renderPlot({
        log.output("Parallel")
        x <- getSubset(checkedVars(), input$selectedDf)
        if (is.null(x)) return()
        cmdLog("# Parallel Analysis")
        cmdLog("ggscree.plot(myData)\n")
        ggscree.plot(x)
    })

    # MAP test ####

    maptest <- reactive({
        log.output("Map")
        x <- getSubset(checkedVars(), input$selectedDf)
        if (is.null(x)) return()
        mt <- mapTest(x)
        mt
    })

    output$maptest <- renderPrint({
        log.output("Map 2")
        x <- maptest()
        if (!is.null(x)) print(x)
        cat("") # Needed for Shiny (Shiny otherwise raises error)
    })

    output$maptest.plot <- renderPlot({
        log.output("Map 3")
        x <- maptest()
        if (!is.null(x)) {
            cmdLog("# MAP Test")
            cmdLog("(mymaptest <- mapTest(myData))")
            cmdLog("plot(mymaptest)\n")
            plot(x)
        }
    })


    # EFA ####

    nFactors <- reactive({
        as.numeric(input$nFactors)
    })

    faDigits <- reactive({
        as.numeric(input$faDigits)
    })

    faMethod <- reactive({
        switch(input$faMethod,
               "Minimum residuals" = "minres",
               "Principal axes" = "pa",
               "Maximum likelihood" = "ml",
               "Principal components" = "princomp")
    })

    faRotation <- reactive({
        input$faRotation
    })

    ## Why needed?
    faIRT <- reactive({
        input$faIRT
    })
    
    output$efa <- renderPrint({
        log.output("EFA")
        if (input$mainTabset != "EFA" && faIRT()) return() #####?
        vnames <- checkedVars()
        if (is.null(vnames)) return()
        
        # Check if we have enough variables for the specified number of factors
        p <- length(vnames)
        if (faMethod() == "princomp") {
            if((p < 2) || (nFactors() > p)) {
                cat("PCA needs at least two variables and\nthe number of components must be less or equal\nto the number of variables.")
                return()
            }
        } else {
            if (faMethod() == "ml")
                dof <- dfEFA(p, nFactors())
            else
                dof <- p - nFactors() -1
            if (dof < 0) {
                cat("With only", p, "variables,", nFactors(), "factor/s is/are too much.\nReduce the number of factors or include more variables.")
                return()
            }
        }
        
        Df <- getSubset(checkedVars(), input$selectedDf)
        if (is.null(Df)) return()
        
        if (faMethod() == "princomp") {
            ### Error in psych: quartimax is quatimax in doc
            possible.rots = c("none", "varimax", "quartimax", "promax", "oblimin", "simplimax", "cluster")
            if (faRotation() %in% possible.rots) {
                cmdLog("# Principal Components Analysis")
                cmdLog(paste0(
                    "principal(myData",
                    ", ", nFactors(),
                    ",\n    rotate = '", faRotation(), "'",
                    ")\n"
                ))
                
                fa.res <- principal(Df, nFactors(), rotate = faRotation())
                cat("PRINCIPAL COMPONENTS\n")
            } else {
                cat("With principal components, only the following rotations are possible: ", possible.rots)
            }
        } else {
            cmdLog("# Exploratory Factor Analysis")
            cmdLog(paste0(
                "factoranalysis(myData",
                ", ", nFactors(),
                ",\n    fm = '", faMethod(), "'",
                ",\n    rotate = '", faRotation(), "'",
                ",\n    polychor = '", faIRT(), "'",
                ")\n"
            ))
            fa.res <- factoranalysis(Df, nFactors(), fm = faMethod(), 
                rotate = faRotation(), polychor = faIRT())
        }
        log.output(class(fa.res))
        
        classifyItems(fa.res, Df, input$faMinloading, input$faMaxloading,
            input$faComplexity, input$faItemlength, input$faDigits, 
            Df.name = input$selectedDf)
    })
    
    # CFA ####

    output$cfa <- renderPrint({
        log.output("CFA")

        x <- getSubset(checkedVars(), input$selectedDf, 3)
        if (is.null(x)) return()

        maxvars <- input$cfamaxvars
        if(ncol(x) > maxvars) {
            cat("CFA was disabled because there are too many variables in the model.")
            return()
        }

        myvars <- paste(names(x), collapse = " + ")
        if (regexpr("\\$", myvars)[1] > -1) {
            cat("\nAt least on variable name contains a $-sign. This is not allowed in the model specification.\n")
            return()
        }
        modelCmd <- paste0("model <- 'Factor =~ ", myvars, "'")
        eval(parse(text = modelCmd))

        if (input$cfaEstimator == "WLSMV") {
            # Ordered argument vor WLSMV
            orderedArg <- paste0("ordered = c(",
                paste("'", names(x), "'", sep = "", collapse = ","),
                ")")
            myCmd <- paste0("cfa(model, data = ",
                input$selectedDf,
                ", ",
                orderedArg,
                ")")

        } else {
            myCmd <- paste0("cfa(model, data = ",
                input$selectedDf,
                ", estimator = '",
                input$cfaEstimator,
                "')")
        }

        fit <- try(eval(parse(text = myCmd)))
        if (class(fit) == "try-error") return()

        fitm <- fitMeasures(fit)
        if (is.na(fitm["rmsea.scaled"]))
            fitm <- fitm["rmsea"]
        else
            fitm <- fitm[c("rmsea", "rmsea.scaled")]
        cat("RMSEA:\n")
        # header <- round(c(fitm, semTools::reliability(fit)), 3)
        print(round(fitm, 3))
        cat("\nReliability estimates:\n")
        print(semTools::reliability(fit))
        cat("\n")

        cmdLog("# Confirmatory Factor Analysis")
        cmdLog(modelCmd)
        cmdLog(paste0("fit <- ", myCmd))
        cmdLog("summary(fit, fit.measures = TRUE, standardized = TRUE)\n")
        summary(fit, fit.measures = TRUE, standardized = TRUE)
    })

    # PCM ####

    computePCM <- reactive({
        log.output("RASCH")
        #### if (input$mainTabset != "Rasch") return() # not needed anymore
        #### if (input$fitrasch == 0) return()

####        isolate({
            x <- getSubset(checkedVars(), input$selectedDf, 3)
            if (is.null(x)) return()
            x <- na.omit(x) ####

            if (input$raschmodel == "pcm") res <- PCM(x)
            else if (input$raschmodel == "rsm") res <- RSM(x)
            else res <- RM(x)
            pp <- person.parameter(res)
            log.output("RASCH done")
            cases <- nrow(x)

            x <- list(res = res, pp = pp, cases = cases)
            x
####        })
    })

    output$pcm <- renderPrint({
        log.output("RASCH, Parameters")
        x <- computePCM()
        if (is.null(x)) return()

        #summary(res)
#         cat("Andersen's LR-Test:")
#         print(LRtest(x$res))
        if (input$raschmodel == "rasch") {
            ###
        } else {
            cat("Thresholds:")
            print(thresholds(x$res))
            cat("")
        }
        print(x$pp)
        cat("")
        print(itemfit(x$pp))
        cat("")

        # Summary of person fit
        cat("Unique Response Patterns and Personfit Statistics:\n")

        # Data frame for response pattern
        pers.fit <- personfit(x$pp)
        case <- row.names(x$pp$X)
        sumscore <- rowSums(x$pp$X)

        patDf <- as.data.frame(x$pp$X)
        vnames <- names(patDf)
        vnames <- paste("patDf$", vnames, sep = "", collapse = ", ")
        shortpat <- paste0("paste(", vnames, ", sep = '')")
        shortpat <- eval(parse(text = shortpat))
        #pat <- data.frame(case, x$pp$X, total = x$sumscore)
        pat <- data.frame(case, "Response pattern" = shortpat, Sum = sumscore)

        # Data frame for fit statistics
        p <- pchisq(pers.fit$p.fit, pers.fit$p.df-1, lower.tail = FALSE)
        sig <- ifelse(p < 0.01, "*", "")
        case <- names(pers.fit$p.fit)
        deviating <- case[p < 0.01]
        pfit <- data.frame(case,
                           Chisq = round(pers.fit$p.fit, 2),
                           df = pers.fit$p.df-1,
                           p = round(p, 3),
                           sig,
                           "Outfit MSQ" = round(pers.fit$p.outfitMSQ, 2),
                           "Infit MSQ" =  round(pers.fit$p.infitMSQ, 2))
        mm <- merge(pat, pfit, by = "case", all = TRUE)
        mm <- unique(mm[,-1])
        mm <- mm[order(mm[,"Sum"]),]

        cat("\nPerson fit could be computed for", length(case), "cases")
        cat("\nTotal number of cases:", x$cases)
        cat("\nCases with deviating response patterns (p < .01):", length(deviating))
        cat("\nNumber of unique response patterns:", length(mm$p))
        cat("\nNumber of deviating response patterns (p < .01):", length(mm$p[mm$p < 0.01]))
        cat("\n\n")
        print(mm, row.names = FALSE)
        cat("")
    })

    output$pcm.pimap <- renderPlot({
        log.output("RASCH, PI Map")
###        if (input$fitrasch == FALSE) return()
        x <- computePCM()
        if (is.null(x)) return()
        plotPImap(x$res, sorted=input$pcm.sortitems,
                  warn.ord.colour = "red", cex.gen = 0.8)
    }, res = 96)
    ### Check "res"

    output$rasch.icc <- renderPlot({
        log.output("RASCH, ICC")
###        if (input$fitrasch == FALSE) return()
        if (input$raschmodel != "rasch") return()

        x <- computePCM()
        if (is.null(x)) return()
        ggplotICC.RM(x$res, empICC = list(input$rasch.icctype))

    }, res = 96) ### res

    output$pcm.info <- renderPlot({
        log.output("PCM, Info")
###        if (input$fitrasch == FALSE) return()

        x <- computePCM()
        if (is.null(x)) return()

        plotINFO(x$res)

    })

    # Help ####

    output$info <- renderPrint({
        paste0("Working directory: ", getwd(), ", Temp dir: ", tempdir())
    })

})
