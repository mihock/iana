require(shiny)
require(shinyAce)
require(ggplot2)
require(lavaan) ### Why needed? Better construct a "runCFA"-command?

log.output <- function(x = "") {
    t <- proc.time()[3]
    cat("===", x, t, "\n", file = stderr())
}

#### Todo
#### cmdLogFile <- paste(tempdir(), "iana_cmd_log.R", sep = "/")
cmdLogFile <- path.expand(paste("~", "iana_cmd_log.R", sep = "/"))
.old.mycmd <<- ""
cat("# Command Log for IANA\n\n", file = cmdLogFile)
cmdLog <- function(cmd.string = "") {
    cat(cmd.string, "\n", file = cmdLogFile, append = TRUE)
}
log.output(paste("Command log is in", cmdLogFile))
###

shinyServer(function(input, output) {

    # Data #####################################################################
    
    getSelectedDf <- reactive({
        Df <- get(input$selectedDf)
        dplyr::filter_(Df, ~complete.cases(Df))
    })

    getLikertLikeVars <- function(Df, unique.values = 9) {
        # Returns the names of all numeric variables in Df
        # with less than the specified number of unique values. If this
        # number is > 19, the names of all numeric variables are returned.

        maybeLikert <- function(x, unique.values = 9) {
            is.int <- function(x) {
                if (is.numeric(x)) {
                    ceiling(x) == floor(x)
                } else {
                    return(FALSE)
                }
            }
            if (unique.values > 19) {
                is.numeric(x)
            } else {
                is.int(x) &&
                    length(unique(x)) <= unique.values &&
                    min(x) >= 0 &&
                    max(x) <= unique.values
            }
        }

        log.output("getlikertlikevars")
        ### todo: message if no values are left?
        if (nrow(Df) == 0) return()
        numvars <- logical(ncol(Df))
        for (i in 1:ncol(Df)) {
            if (maybeLikert(Df[,i], unique.values)) numvars[i] <- TRUE
        }
        varnames <- names(Df)[numvars]
    }

    getVarsInDf <- reactive({
        log.output("getVarsInDf")
        varnames <- getLikertLikeVars(getSelectedDf(), input$kUniqueValues)
        if (length(varnames) == 0) {
            log.output("getVarsInDf: No Likert-like variables found in the data frame.")
            varnames <- NULL
        }
        varnames
    })
    
    getFactorsInDf <- reactive({
        getFactors <- function(x) {
            fnames <- names(x)[sapply(x, is.factor)]
            if (length(fnames) > 0) return(fnames)
            else return()
        }
        log.output("getFactorsInDf")
        fnames <- c("median", getFactors(getSelectedDf()))
        fnames
    })
    
    getVarRange <- reactive({
        log.output("getVarRange")
        if (input$varrange == "") {
            varrange <- getVarsInDf()
            if (length(varrange) > 80) varrange <- varrange[1:80]
        } else {
            newDf <- try(dplyr::select_(getSelectedDf(),
                                            .dots = input$varrange))
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
        log.output("varsindf")
        varsInDf <- getVarsInDf()
        if (is.null(varsInDf)) return()
        checkboxGroupInput(inputId = "varnamesindf",
            label = "Variables to use:",
            choices = varsInDf,
            #inline = TRUE,
            selected = getVarRange())
    })
    
    output$factorsindf <- renderUI({
        log.output("outputfactorsindf")
        factorsInDf <- getFactorsInDf()
        if (is.null(factorsInDf)) return()
        selectInput(inputId = "factorsindf",
            label = "Split criterion for Andersen und Wald tests:",
            choices = factorsInDf)
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
        return()
    })

    getSubset <- function(vnames, minNoVars = 2) {
        log.output("getSubset")
        req(vnames, length(vnames) >= minNoVars)
        Df <- dplyr::select_(getSelectedDf(), .dots = vnames)
        Df
    }

    output$casesindf <- renderUI({
        log.output("casesindf")
        x <- getSubset(checkedVars())
        helpText(paste0(nrow(x), " cases in data frame."))
    })

    # Item text and frequencies ################################################

    output$frequencies <-  renderTable({
        log.output("frequencies")
        x <- getSubset(checkedVars())
        cmdLog("# Frequencies\n")
        cmdLog("frequencies(myData)\n")
        iana::frequencies(x)
    }, digits = 0)
    
    # Reliability ####

    output$reliability <- renderPrint({
        log.output("reliability")
        x <- getSubset(checkedVars())
        cmdLog("# Reliability")
        if (input$reliabDetailed){
            cmdLog("alpha(myData)\n")
            print(psych::alpha(x))
        } else {
            cmdLog("reliability(myData)\n")
            iana::reliability(x, dfname = input$selectedDf)
        }
    })

    # Histogram ################################################################

    output$hist <- renderPlot({
        log.output("hist")
        x <- getSubset(checkedVars(), 1)
        ### cmdLog
        d <- tidyr::gather_(x, "Item", "Score", names(x))
        if (input$histtypeitem == "count") {
            ggplot2::ggplot(d, aes_(x = ~as.factor(Score))) +
                facet_wrap(~Item) +
                geom_bar(colour = "black", fill = "white") +
                xlab("Response") + ylab("Count") +
                theme(text = element_text(size = 14))
        } else {
            ggplot2::ggplot(d, aes_(x = ~as.factor(Score))) + 
                facet_wrap(~Item) +
                geom_bar(aes_(y = ~100*(..count..) /
                        tapply(..count..,..PANEL..,sum)[..PANEL..]),
                    colour = "black", fill = "white") +
                xlab("Response") + ylab("Percent of total") +
                theme(text = element_text(size = 14))
        }
    })
    
    output$histTotal <- renderPlot({
        log.output("histTotal")

        x <- getSubset(checkedVars())
        sumScore <- rowSums(x, na.rm = TRUE)
        binw <- input$histbinwidth
        if (input$totalscoretype == "sum") Total <- sumScore
        else {
            Total <- rowMeans(x, na.rm = TRUE)
            binw <- binw/ncol(x)
        }
        d <- data.frame(Total)
        rm(Total)
        
        # Setup colors for histogram
        mycolor = "black"
        myfill = "white" ### better NA?
        
        # Plot
        p <- ggplot2::ggplot(d, aes_(x = ~Total)) +
            xlab("Total score") +
            theme(text = element_text(size = 14)) 
        if (input$histtype == "percent")  {
            p <- p + geom_histogram(aes_(y = ~100*(..count..) / sum(..count..)),
                color = mycolor, fill = myfill, 
                binwidth = binw) +
                ylab("Percent of total")
        } else if (input$histtype == "count") {
            p <- p + geom_histogram(binwidth = binw,
                color = mycolor, fill = myfill) +
                ylab("Count")
        } else { # density
            p <- p + 
                ylab("Density") +
                geom_histogram(aes_(y = ~..density..), 
                    color = mycolor, fill = myfill,
                    binwidth=binw) +
                stat_function(fun=dnorm, 
                    args=list(mean=mean(d$Total), sd=sd(d$Total)),
                    colour = "darkgreen", size = 2, alpha = 0.5
                ) +
                geom_density(color="blue")
        }
        p
    })
    
    output$descrStatsTotal <- renderTable({
        x <- getSubset(checkedVars())
        sumScore <- rowSums(x, na.rm = TRUE)
        meanScore <- rowMeans(x, na.rm = TRUE)
        rbind("Sum score" = iana::basicDescr(sumScore), "Mean score" = iana::basicDescr(meanScore))
    })

    # ICCs #####################################################################

    output$ICCs <- renderPlot({
        log.output("ICCs")
        x <- getSubset(checkedVars())
        if (input$ICClinear) method <- "lm"
        else method <- "loess"
        ### cmdLog
        iana::empICC(x, input$ICCscore, method = method, span = input$ICCloessspan,
            alpha = input$ICCalpha, jitter = input$ICCjitter)
    }, res = 96) ### check res

    # Parallel analysis ########################################################

    output$parallelanalysis <- renderPlot({
        log.output("parallelanalysis")
        x <- getSubset(checkedVars())
        cmdLog("# Parallel Analysis")
        #cmdLog("ggscree.plot(myData)\n")
        #iana::ggscree.plot(x)
        nfac <- input$nFactorsParallel
        if (nfac >= 21) nfac <- NULL
         iana::parallelAnalysis(x, 
                                fm = input$faMethodParallel,
                                cor = input$basisParallel,
                                n.factors = nfac, 
                                sim = input$simParallel,
                                onlyFA = input$onlyFAParallel)
    })

    # MAP test #################################################################

    maptest <- reactive({
        log.output("maptest")
        x <- getSubset(checkedVars())
        mt <- iana::mapTest(x)
        mt
    })

    output$maptest <- renderPrint({
        log.output("maptest (output)")
        x <- maptest()
        if (!is.null(x)) print(x)
    })

    output$maptest.plot <- renderPlot({
        log.output("maptest.plot")
        x <- maptest()
        if (!is.null(x)) {
            cmdLog("# MAP Test")
            cmdLog("(mymaptest <- mapTest(myData))")
            cmdLog("plot(mymaptest)\n")
            plot(x)
        }
    })

    # EFA ######################################################################

    computeEFA <- reactive({
        log.output("computeEFA")
        vnames <- checkedVars()
        if (is.null(vnames)) return()
        
        numfac <- input$nFactors
        famethod <- input$faMethod
        farot <- input$faRotation
        fairt <- input$faIRT
        
        # Check if we have enough variables for the specified number of factors
        p <- length(vnames)
        if (famethod == "princomp") {
            if((p < 2) || (numfac > p)) {
                cat("PCA needs at least two variables and\nthe number of components must be less or equal\nto the number of variables.")
                return()
            }
        } else {
            if (famethod == "ml")
                dof <- dfEFA(p, numfac)
            else
                dof <- p - numfac -1
            if (dof < 0) {
                cat("With only", p, "variables,", numfac, 
                    "factor/s is/are too much.\nReduce the number of factors or include more variables.")
                return()
            }
        }
        
        Df <- getSubset(checkedVars())
        if (famethod == "princomp") {
            possible.rots = c("none", "varimax", "quartimax", "promax", 
                "oblimin", "simplimax", "cluster")
            if (farot %in% possible.rots) {
                cmdLog("# Principal Components Analysis")
                cmdLog(paste0(
                    "factoranalysis(myData",
                    ", ", numfac,
                    ",\n    rotate = '", farot, "'",
                    ",\n    fm = 'principal'",
                    ")\n"
                ))
                fa.res <- factoranalysis(Df, numfac, 
                    rotate = farot, 
                    fm = "principal", 
                    return.res = TRUE)
            } else {
                cat("With principal components, only the following rotations are possible: ",
                    possible.rots)
                ### stop?
            }
        } else {
            cmdLog("# Exploratory Factor Analysis")
            cmdLog(paste0(
                "factoranalysis(myData",
                ", ", numfac,
                ",\n    rotate = '", farot, "'",
                ",\n    fm = '", famethod, "'",
                ",\n    polychor = '", fairt, "'",
                ")\n"
            ))
            fa.res <- iana::factoranalysis(Df, numfac,
                rotate = farot, 
                fm = famethod, 
                polychor = fairt, return.res = TRUE)
        }

        ### cmdLog
        classif <- iana::classifyItems(fa.res$res, Df, input$faMinloading, 
            input$faMaxloading, input$faComplexity, 
            input$faItemlength, input$faDigits, 
            Df.name = input$selectedDf, return.res = TRUE)
        ##log.output("computeEFA: DONE")
        list(fa.res = fa.res$res, 
            fit = fa.res$stats,
            factorloadings = classif$factorloadings, 
            factorvariances = classif$factorvariance, 
            factorcorrelations = classif$factorcorrelations,
            factorcode = classif$factorcode)
    })
    
    output$factorfit <- renderTable({
        log.output("factorfit")
        res <- computeEFA()
        res$fit
    }, include.rownames = FALSE)

    output$loadings <- renderTable({
        log.output("loadings")
        res <- computeEFA()
        res$factorloadings
    })
    
    output$factorvariances <- renderTable({
        log.output("factorvariances")
        res <- computeEFA()
        res$factorvariances
    })

    output$factorcorrelations <- renderTable({
        log.output("factorcorrelations")
        res <- computeEFA()
        if (is.null(res$factorcorrelations)) log.output("factor correlations are NULL")
        res$factorcorrelations
    })

    output$factorcode <- renderPrint({
        log.output("factorcode")
        res <- computeEFA()
        cat(res$factorcode)
    })
    
    # CFA ######################################################################

#     observe({
#         print(input$cfaModelEditor)
#     })
    
#     output$cfaoutput <- renderPrint({
#         input$cfaEvalModel
#         #return(isolate(eval(parse(text=input$cfaModelEditor))))
#         isolate(cat("mymodel <-", input$cfaModelEditor))
#     }) 
    
    output$cfa <- renderPrint({
        log.output("cfa")

        x <- getSubset(checkedVars(), 3)

        if (input$cfaUseModel) {
            input$cfaEvalModel
            isolate({
                modelCmd <- paste0("model <- '", 
                    str_trim(input$cfaModelEditor), "'")
            })
        } else {
            myvars <- paste(names(x), collapse = " + ")
            ### Reason for this check?
            if (regexpr("\\$", myvars)[1] > -1) {
                cat("\nAt least on variable name contains a $-sign. This is not allowed in the model specification.\n")
                return()
            }
            modelCmd <- paste0("model <- 'Factor =~ ", myvars, "'")
        }
        eval(parse(text = modelCmd))
        
        if (input$cfaEstimator == "WLSMV") {
            orderedArg <- paste0("ordered = c(",
                paste("'", names(x), "'", sep = "", collapse = ","),
                ")")
            myCmd <- paste0("lavaan::cfa(model, data = ",
                input$selectedDf,
                ", ",
                orderedArg,
                ")")
        } else {
            myCmd <- paste0("lavaan::cfa(model, data = ",
                input$selectedDf,
                ", estimator = '",
                input$cfaEstimator,
                "')")
        }
        log.output(myCmd)

        fit <- try(eval(parse(text = myCmd)))
        if (class(fit) == "try-error") return()
        log.output(paste("lavaan fit, class:", class(fit)))
        fitm <- lavaan::fitMeasures(fit)
        if (is.na(fitm["rmsea.scaled"]))
            fitm <- fitm["rmsea"]
        else
            fitm <- fitm[c("rmsea", "rmsea.scaled")]
        cat("RMSEA:\n")
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
    
    # Rasch & PCM ##############################################################

    computePCM <- reactive({
        log.output("computePCM")
        x <- getSubset(checkedVars(), 3)

        # Exit if the number of unique values in the data is too large.
        n.values <- length(unique(as.vector(as.matrix(x))))
        validate(
            need(n.values < 10, 
                 paste("Your data have", n.values, "unique values. This is too much for a PCM.\n(maximum is 9 unique values).\n"))
        )
        
        # Fit a Rasch Model if data have 2 unique values,
        # otherwise fit a Partial credit model. 
        withProgress(message = 'RM/PCM', detail = "running", max = 2, {
            if ( n.values == 2 ) {
                model <- "Rasch"
                res <- eRm::RM(x)
            } else {
                model <- "PCM"
                res <- eRm::PCM(x)
            }
            incProgress(1, detail = "person parameters")
            pp <- eRm::person.parameter(res)
            incProgress(1, detail = "done")
            cases <- nrow(x)
            x <- list(res = res, pp = pp, cases = cases, model = model)
        })
        x
    })

    output$pcm.tests <- renderPrint({
        log.output("pcm.tests")
        x <- computePCM()
        if (x$model == "Rasch") 
            cat("Fitting a Rasch Model for binary items because data\nhave 2 unique values.\n")
        else 
            cat("Fitting a Partial Credit Model because data\n have more than 2 unique values.\n")
        splitcriterion <- input$factorsindf
        if (length(splitcriterion) == 0) splitcriterion <- "median"
        cat("\nANDERSEN'S LR TEST")
        cat("\n==================\n")
        cat("Split criterion:", splitcriterion, "\n")
        if (splitcriterion == "median")
            iana::tryPrintExpr(eRm::LRtest(x$res))
        else
            iana::tryPrintExpr(eRm::LRtest(x$res, splitcr = getSelectedDf()[[splitcriterion]]))
        cat("\nWALD TEST")
        cat("\n=========\n")
        cat("Split criterion:", splitcriterion, "\n")
        if (splitcriterion == "median")
            iana::tryPrintExpr(eRm::Waldtest(x$res))
        else ### Todo: Allow only binary factors?
            iana::tryPrintExpr(eRm::Waldtest(x$res, splitcr = getSelectedDf()[[splitcriterion]]))
        cat("\nMARTIN LÖF TEST")
        cat("\n===============\n")
        iana::tryPrintExpr(eRm::MLoef(x$res))
    })
    
    output$pcm.graphmodeltest <- renderPlot({
        log.output("pcm.graphmodeltest")
        x <- computePCM()
        ### Todo: We compute LRTest twice. Catch errors?
        splitcriterion <- input$factorsindf
        if (length(splitcriterion) == 0) splitcriterion <- "median"
        if (splitcriterion == "median")
            res <- eRm::LRtest(x$res, se = TRUE)
        else
            res <- eRm::LRtest(x$res, 
                splitcr = getSelectedDf()[[splitcriterion]], se = TRUE)
        eRm::plotGOF(res, tlab = input$pcm.graphmodeltest.labels, ctrline = list())    
    }, res = 96) ### Check "res"

    output$pcm.itemfit <- renderPrint({
        log.output("pcm.itemfit")
        x <- computePCM()
        print(eRm::itemfit(x$pp))
    })
        
    output$pcm.itemstats <- renderPrint({
        log.output("pcm.itemstats")
        x <- computePCM()
        if (x$model == "Rasch") {
            summary(x$res)
        } else {
            cat("Thresholds:")
            print(eRm::thresholds(x$res))
            cat("\n")
            cat("Summary:")
            summary(x$res)
        }
    })
    
    output$pcm.personstats <- renderPrint({
        log.output("pcm.itemstats")
        x <- computePCM()
        print(x$pp)

        # Summary of person fit
        cat("\n\nUnique Response Patterns and Personfit Statistics:\n")

        # Data frame for response pattern
        pers.fit <- eRm::personfit(x$pp)
        case <- row.names(x$pp$X)
        sumscore <- rowSums(x$pp$X)

        patDf <- as.data.frame(x$pp$X)
        vnames <- names(patDf)
        vnames <- paste("patDf$", vnames, sep = "", collapse = ", ")
        shortpat <- paste0("paste(", vnames, ", sep = '')")
        shortpat <- eval(parse(text = shortpat))
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
        x <- computePCM()
        eRm::plotPImap(x$res, sorted=input$pcm.sortitems,
                  warn.ord.colour = "red", cex.gen = 0.8)
    }, res = 96) ### Check "res"

    output$rasch.icc <- renderPlot({
        log.output("RASCH, ICC")
        x <- computePCM()
        if (x$model == "Rasch") {
            iana::ggplotICC.RM(x$res, empICC = list(input$rasch.icctype))
        } else {
            # Empirical ICCs can only be plotted for a dichotomous Rasch model
            iana::ggplotICC.RM(x$res)
        }
    }, res = 96) ### res

    output$pcm.info <- renderPlot({
        log.output("PCM, Info")
        x <- computePCM()
        eRm::plotINFO(x$res)
    })

    # MIRT #####################################################################
    
    computeMirt <- reactive({
        log.output("computeMirt")
        x <- getSubset(checkedVars(), 3)

        # Exit if the number of unique values in the data is too large.
        # todo: make a function of it; also check if n.values > 1
        #       (used also in Rasch models)
        # todo: also for WLSMV, polychoric corrs.
        n.values <- length(unique(as.vector(as.matrix(x))))
        validate(
            need(n.values < 10, 
                 paste("Your data have", n.values, "unique values. This is too much for a MIRT model.\n(maximum is 9 unique values).\n"))
        )
        
        nf <- input$mirt_nfactors 
        model <- input$mirt_model
        validate(
            need(!(model == "Rasch" && nf > 1), 
                "For Rasch models, only 1 dimension is possible. Please choose another model.")
        )
        cmdLog(paste0(
            "# IRT\n",
            "res <- mirt(myData",
            ",\n    model = ", nf, 
            ",\n    itemtype = '", model, "'",
            ",\n    method = '", input$mirt_method, "'",
            ",\n    rotate = '", input$mirt_rotate, "'",
            ",\n    SE = TRUE, verbose = FALSE)"
        ))
        withProgress(message = 'MIRT', detail = "running", max = 1, {
            res <- mirt::mirt(x, 
                              model = nf, 
                              itemtype = model, 
                              method = input$mirt_method,
                              SE = TRUE, verbose = FALSE)
            incProgress(1, detail = "done")
        })
        log.output("computeMirt done")
        res
    })

    output$mirt.summary <- renderPrint({
        log.output("mirt (output)")
        x <- computeMirt()
        cat("\nBASICS\n")
        cmdLog("print(res)")
        print(x)
        cat("\nSUMMARY\n")
        cmdLog(paste0("summary(res, rotate = ", input$mirt_rotate, ")"))
        summary(x, rotate = input$mirt_rotate)
#        cat("\nCOEFFICIENTS\n")
#        print(coef(x))
        cat("\nMODEL FIT\n")
        cmdLog("print(mirt::M2(res))")
        print(mirt::M2(x), digits = 3)
#        M2(x, residmat = TRUE, suppress = 0.1)
#        cat("\nWALD TEST\n")
#        print(mirt::wald(x), digits = 2)
        cat("\nITEMFIT\n")
        cmdLog("mirt::itemfit(res)")
        mirt::itemfit(x)
        #mirt::personfit(x)
    })
    
    # Help #####################################################################

    output$info <- renderPrint({
        paste0("Working directory: ", getwd(), ", Temp dir: ", tempdir())
    })
})
