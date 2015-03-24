# TODO: import?
library(shiny)
library(shinythemes)

shinyUI(fluidPage(
    theme = shinytheme("cerulean"),
    #titlePanel("Iana - Item Analysis"),
    
    sidebarLayout(
        
        ###fluid = FALSE,
        position = "right",
        
        sidebarPanel = sidebarPanel(
            
            ### width = 4,
            
            h3("Item selection"),
            
            helpText("The dropdown list below contains the data frames present in your R environment. Please select a data frame containing the variables (items) to analyze. You can then select or deselect variables. Notice that only variables coded with integer values are shown."),
            helpText(""),
            
            ###actionButton(inputId = "stopIana", label = " Stop "),
            
            selectInput(inputId = "selectedDf", label = "Data frame to use:",
                choices = getDataFramesIana()),
            numericInput(inputId = "kUniqueValues", 
                label = "Exclude variables with values outside the range of 0 and ...:",
                min = 2, max = 20, value = 9, step = 1),
            helpText("To include all numeric (including non-integer) variables, set this to 20."),
            
            uiOutput(outputId = "varsindf"),
            
            helpText("Instead of clicking on the variable names above you can also specify a range of variables using the format 'FirstVar:LastVar'."),
            textInput(inputId = "varrange", label = "Variable range:"),
            helpText("Type Ctrl+A DELETE into the entry to clear to range."),
            
            uiOutput(outputId = "casesindf")
            
            
            #         numericInput(inputId = "plotHeightMultipanel", 
            #                      label = "Plot height for multipanel plots:",
            #                      min = 400, max = 2000, value = 800, step = 100),
            #         textOutput(outputId = "phmp")
        ),
        
        mainPanel = mainPanel(
            navbarPage(
                title = "Iana",
                id = "mainTabset",
                
                tabPanel("Distributions",
                    h3("Item scores"),
                    selectInput(inputId = "histtypeitem",
                        label = "Type of plot:",
                        choices = c("Percentages" = "percent", 
                            "Counts" = "count")),
                    plotOutput(outputId = "hist", 
                        height = getOption("iana.plotheight")),
                    
                    h3("Total score"),
                    fluidRow(
                        column(6, 
                            selectInput(inputId = "histtype",
                                label = "Type of plot:",
                                choices = c("Percentages" = "percent", 
                                    "Counts" = "count",
                                    "Density" = "density"))),
                        column(6, 
                            selectInput(inputId = "totalscoretype",
                            label = "Total score represents:",
                            choices = c("Sum of item scores" = "sum", 
                                "Average of item scores" = "ave")))
                    ),
                    sliderInput(inputId = "histbins", 
                        label = "Number of bins (3 = automatic):",
                        min = 3, max = 45, value = 3, step = 1,
                        animate = TRUE),
                    
                    helpText(""),
                    
                    plotOutput(outputId = "histTotal"),
                    
                    h3("Item stems"),
                    verbatimTextOutput(outputId = "itemtext"),
                    h3("Frequency counts"),
                    helpText("'NA' (not available) refers to missing values in the reponses."),
                    verbatimTextOutput(outputId = "frequencies")
                ),
                
                tabPanel("ICCs",
                    h3("Empirical Item Characteristic Curves"),
                    helpText("To examine item characteristics, item scores are plotted against total scores or factor scores. To avoid overplotting, a small amount of jitter is added to overlapping points. (These are the light points in the plot.) In the upper left corner, the correlation of the total/factor score and the item score is given. The lines are locally weighted regression lines, the shaded regions represent 95% confidence intervals around the expected item scores. If you have many data points you might want to decrease the opaqueness of the points and/or to deactivate jitter."),
                    
                    fluidRow(
                        column(6,
                            selectInput(inputId = "ICCscore",
                                label = "Score to use:",
                                choices = c("Factor score (Thomson)" = "factor.thomson",
                                    "Factor score (Bartlett)" = "factor.bartlett",
                                    "Total (mean) score" = "mean",
                                    "Total (sum) score" = "sum")),
                            checkboxInput(inputId = "ICClinear", 
                                label = "Fit linear regression")),
                        column(6, 
                            numericInput(inputId = "ICCalpha", 
                                label = "Opaqueness of points:",
                                min = 0, max = 1, value = 0.5, step = 0.1),
                            checkboxInput(inputId = "ICCjitter", 
                                label = "Jitter",
                                value = TRUE))
                        ),
                    
                    plotOutput(outputId = "ICCs", height = getOption("iana.plotheight")),
                    h3("Notes"),
                    helpText("The function 'loess' is used to produce locally weighted regression fits. The span (parameter alpha), which determines the degree of smoothing, is 0.75."),
                    helpText("Factor scores are based on based on maximum-likelihood factor analysis. See 'factanal' for details.")
                ),

                tabPanel("Dimensionality",
                    h3("Parallel Analysis"),
                    helpText("The parallel analysis below is based on the principal components of the data. The circles connected by the thick line show the empirical eigenvalues. The thin lines represent the eigenvalues of 20 simulations with normally distributed random data. A dimension/component is judged to be meaningful if its eigenvalue is larger than the eigenvalues obtained from random data."),
                    #plotOutput(outputId = "hist", height = "400px", width = "400px")
                    plotOutput(outputId = "parallelanalysis"),
                    
                    h3("MAP Test"),
                    helpText("The Minimum Average Partial (MAP) test works by computing the average of the squared (partial) correlations between the p variables of a data set after the first m = 1 to (p-1) principal components have been removed (partialled out). The suggestion is to retain components, for which this average reaches its minimum. The table below also shows the average squared correlation between the variables (no component removed; m = 0). If this value is the lowest in the table, there is probably no common variance to analyze. The maximum number of components removed is 20."),
                    verbatimTextOutput(outputId = "maptest"),
                    helpText("Graphical representation of the MAP test. The blue line shows the average squared correlation of the items."),
                    plotOutput(outputId = "maptest.plot")
                ),
                
                tabPanel("EFA",
                    h3("Principal Components and Exploratory Factor Analysis"),
                    helpText("By default, results of exploratory factor analysis (EFA) are shown. To obtain results of principal components analysis (PCA) choose 'Principal components' in the Method dropdown list. (For PCA, not all rotations are implemented.) The method options include 'Item Response Theory.' In this case polychoric correlations are computed by a fast (also somewhat inaccurate) two-step procedure. These correlations are then fitted by maximum likelihood factor analysis. For obtaining accurate estimates check the 'Accurate' Box. Notice that this computation is very slow."),
                    
                    h3("Factoring"),
                    fluidRow(
                        column(4, numericInput(inputId = "nFactors", 
                            label = "Number of factors:",
                            min = 1, max = 20, value = 1, step = 1)),
                        column(4, selectInput(inputId = "faMethod",
                            label = "Factoring method:",
                            choices = c("Maximum likelihood", "Minimum residuals", "Principal axes", "Principal components"))),
                        column(4, selectInput(inputId = "faRotation",
                            label = "Rotation:",
                            choices = c("varimax", "promax", "oblimin", "none", "quartimax", "bentlerT", "geominT", "bifactor", "simplimax", "bentlerQ", "geominQ","biquartimin", "cluster")))
                    ),                    
                    fluidRow(
                        column(8, checkboxInput(inputId = "faIRT", 
                            label = "Use polychoric correlations",
                            value = FALSE))
                    ),
                    
                    h3("Markers"),
                    fluidRow(
                        column(4, numericInput(inputId = "faMinloading", 
                            label = "Minimum loading:",
                            min = 0.1, max = 0.9, value = 0.4, step = 0.05)),
                        column(4, numericInput(inputId = "faMaxloading", 
                            label = "Maximum minor loading:",
                            min = 0.1, max = 0.9, value = 0.35, step = 0.05)),
                        column(4, numericInput(inputId = "faComplexity", 
                            label = "Maximum complexity:",
                            min = 0, max = 10, value = 2, step = 0.25))
                    ),
                    
                    h3("Output"),
                    fluidRow(
                        column(4, numericInput(inputId = "faDigits", 
                            label = "Digits to show:",
                            min = 2, max = 8, value = 2, step = 1)),
                        column(4, numericInput(inputId = "faItemlength", 
                            label = "Trim item text (characters, 0 = auto):",
                            min = 0, max = 250, value = 50, step = 5))
                    ),
                    
                    h3("Results"),
                    verbatimTextOutput(outputId = "efa")
                ),

                tabPanel("Reliability",
                    h3("Reliability and Item Statistics"),
                    helpText("The table shows item means, standard deviations, covariances between item scores and total score, item discriminations (i.e., item-total and item-remainder correlations), and the alpha obtained if the respective item is removed from the scale. At the bottom are statistics for the total score, i.e., the sum or the mean of the item scores for each person."),
                    checkboxInput(inputId = "reliabDetailed", 
                        label = "Show detailed output (package psych)",
                        value = FALSE),
                    helpText(""),
                    verbatimTextOutput(outputId = "reliability")
                ),
                
                
                tabPanel("CFA",
                    h3("Confirmatory Factor Analysis"),
                    helpText("Below is the fit of a confirmatory one-factor model for all of the selected variables (items). This is useful for checking fit indexes such as the SRMR or RMSEA or for obtaining unstandardized factor loadings and their standard errors, which are not given by EFAs. The omegas above the summary are reliability estimates based on the factor loadings. McDonalds (1999) version is omega3. Because CFAs for a large number of variables are slow, the computation is suppressed if the number of variables exceeds the specified threshold."),
                    helpText(""),
                    
                    fluidRow(
                        column(6, 
                            sliderInput(inputId = "cfamaxvars", 
                                label = "Threshold for computation:",
                                min = 0, max = 100, value = 20,
                                animate = TRUE)),
                        column(6,
                            selectInput(inputId = "cfaEstimator",
                                label = "Estimator:",
                                # choices = c("ML", "MLM", "WLSMV")))
                                choices = c("ML", "MLM")))
                    ),
                    
                    helpText(""),
                    verbatimTextOutput(outputId = "cfa"),
                    helpText(""),
                    strong("Reference"),
                    helpText("McDonald, R. P. (1999). Test theory: A unified treatment. Mahwah, NJ: Erlbaum.")
                ),
                
                tabPanel("Rasch",
                    h3("Rasch Models"),
                    actionButton(inputId = "fitrasch", label = " Run "),
                    helpText('Press the "Run" button to fit or refit the model. (Fitting Rasch models is computationally intensive, therefore computations are not performed automatically.)'),
                    
                    selectInput(inputId = "raschmodel", 
                        label = "Model to fit:", 
                        choices = c("Partial Credit Model" = "pcm", 
                            "Rating Scale Model" = "rsm", 
                            "Rasch Model for binary items" = "rasch")),
                    
                    conditionalPanel(
                        condition = "input.raschmodel == 'rasch'",
                        h3("ICCs"),
                        selectInput(inputId = "rasch.icctype",
                            label = "Type of empirical ICC:",
                            choices = c(
                                "Local polynomial regression" = "loess", 
                                "Tukey's (running median) smoothing" = "tukey",
                                "Kernel regression" = "kernel", 
                                "Relative frequency" = "raw")),
                        #                     numericInput(inputId = "colsRaschICC", 
                        #                         label = "Number of columns to use:",
                        #                         value = 4, min = 1, max = 10, step = 1),
                        plotOutput(outputId = "rasch.icc", 
                            height = getOption("iana.plotheight"))
                    ),
                    
                    h3("Person-item map"),
                    helpText("Items with nonordinal (disordered) threshold locations are shown in red."),
                    checkboxInput(inputId = "pcm.sortitems", label = "Sort items by location"),
                    plotOutput(outputId = "pcm.pimap", height = getOption("iana.plotheight")),
                    
                    h3("Test and item information"),
                    plotOutput(outputId = "pcm.info", height = getOption("iana.plotheight")),
                    
                    h3("Item and person parameters, fit statistics"),
                    helpText("For the PCM, items coded with an origin of 1 are recoded to have an origin of 0. Therefore, the raw (sum) score to person parameter mapping also starts also with 0."),
                    verbatimTextOutput(outputId = "pcm")
                ),
                
                tabPanel("?",
                    h3("Help"),
                    includeMarkdown("help.md"),
                    h3("Some Infos"),
                    textOutput(outputId = "info")
                )
            )
        )
    )
))
