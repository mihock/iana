### TODO: import?
library(shiny)
library(shinythemes)
library(shinyAce)

shinyUI(fluidPage(
    #theme = shinytheme("cerulean"),
    #theme = shinytheme("cosmo"),
    #theme = shinytheme("spacelab"),
    #theme = shinytheme("united"),
    theme = shinytheme("flatly"),
    #theme = shinytheme("readable"),
    
    sidebarLayout(
        position = "right",
        sidebarPanel = sidebarPanel(
            h3("Run/Apply"),
            helpText("Press this button to run an analysis or to apply changes in variables or options."),
            submitButton("Run/Apply"),
            
            h3("Item selection"),
            helpText("The dropdown list below contains the data frames present in your R environment. Please select a data frame containing the variables (items) to analyze. You can then select or deselect variables. Notice that only variables coded with integer values are shown."),
            br(),
            ###actionButton(inputId = "stopIana", label = " Stop "),
#             submitButton("Apply"),
#             br(),
            ####actionButton(inputId = "applyButton", label = "Apply"),
            ####br(),
            selectInput(inputId = "selectedDf", 
                label = "Data frame to use:",
                choices = iana::getDataFramesIana(),
                selected = getOption("iana.data")),
            numericInput(inputId = "kUniqueValues", 
                label = "Exclude variables with values outside the range of 0 and ...:",
                min = 2, max = 20, value = 9, step = 1),
            helpText("To include all numeric (including non-integer) variables, set this to 20."),
            uiOutput(outputId = "varsindf"),
            helpText("Instead of clicking on the variable names above you can also specify a range of variables using the format 'FirstVar:LastVar'."),
            textInput(inputId = "varrange", label = "Variable range:"),
            helpText("Type Ctrl+A DELETE into the entry to clear to range."),
            uiOutput(outputId = "casesindf")
        ),
        
        mainPanel = mainPanel(
            navbarPage(
                title = "Iana",
                id = "mainTabset",
                
                ### navbarMenu("Distributions", ...) # Bug in shiny (the 1st item must not be a menu)
                tabPanel("Distribution",
                         h3("Distributions of items and total score"),
                         
                         selectInput(inputId = "distrType", label = "Distribution",
                                     choices = c(
                                         "Item scores (histogram)" = "items",
                                         "Total score (histogram)" = "total", 
                                         "Frequency counts and item stems" = "freq",
                                         "Empirical item characteristic curves" = "ICC")),
                                                              
                         conditionalPanel(
                             condition = "input.distrType == 'items'",
                             h3("Item scores"),
                             selectInput(inputId = "histtypeitem",
                                         label = "Y axis represents:",
                                         choices = c("Percentages" = "percent", 
                                                     "Counts" = "count")),
                             plotOutput(outputId = "hist", 
                                        height = getOption("iana.plotheight"))),
                         
                         conditionalPanel(
                             condition = "input.distrType == 'total'",
                             h3("Total score"),
                             fluidRow(
                                 column(6, 
                                        selectInput(inputId = "histtype",
                                                    label = "Y axis represents:",
                                                    choices = c("Density" = "density",
                                                                "Percentages" = "percent", 
                                                                "Counts" = "count"
                                                    ))),
                                 column(6, 
                                        selectInput(inputId = "totalscoretype",
                                                    label = "Total score represents:",
                                                    choices = c("Sum of item scores" = "sum", 
                                                                "Average of item scores" = "ave")))
                             ),
                             sliderInput(inputId = "histbinwidth", 
                                         label = "Binwidth:",
                                         min = 1, max = 20, value = 1, step = 1,
                                         animate = TRUE),
                             br(),
                             plotOutput(outputId = "histTotal"),
                             strong("Notes"),
                             helpText("In density plots the blue line represents the empirical density, the green line represents the normal density. For item average scores, the specified binwidth is divided by the number of items."),
                             strong("Statistics"),
                             p(""),
                             tableOutput(outputId = "descrStatsTotal")
                         ),
                         conditionalPanel(
                             condition = "input.distrType == 'freq'",
                             h3("Frequency counts and item stems"),
                             fluidRow(column(12,
                                             tableOutput(outputId = "frequencies")))
                         ),
                         conditionalPanel(
                             condition = "input.distrType == 'ICC'",
                             h3("Empirical Item Characteristic Curves"),
                             helpText("To examine item characteristics, item scores are plotted against total scores or factor scores. To avoid overplotting, a small amount of jitter is added to overlapping points. In the upper left corner, the correlation of the total/factor score and the item score is given. The lines are locally weighted regression lines, the shaded regions represent 95% confidence intervals around the expected item scores. If you have many data points you might want to decrease the opaqueness of the points and/or to deactivate jitter (by setting it to 0.)"),
                             fluidRow(
                                 column(4,
                                        selectInput(inputId = "ICCscore",
                                                    label = "Score to use:",
                                                    choices = c("Factor score (Thomson)" = "factor.thomson",
                                                                "Factor score (Bartlett)" = "factor.bartlett",
                                                                "Total (mean) score" = "mean",
                                                                "Total (sum) score" = "sum")),
                                        sliderInput(inputId = "ICCalpha", 
                                                    label = "Opaqueness of points:",
                                                    min = 0, max = 1, value = 0.5, step = 0.05,
                                                    animate = TRUE)),
                                 column(4, 
                                        sliderInput(inputId = "ICCloessspan", 
                                                    label = "Span (degree of smoothing):",
                                                    min = 0, max = 1, value = 0.75, step = 0.05,
                                                    animate = TRUE),
                                        checkboxInput(inputId = "ICClinear", 
                                                      label = "Fit linear regression")),
                                 column(4, 
                                        sliderInput(inputId = "ICCjitter", 
                                                    label = "Amount of jitter:",
                                                    min = 0, max = 1, value = 0.3, step = 0.05,
                                                    animate = TRUE))
                             ),
                             plotOutput(outputId = "ICCs", height = getOption("iana.plotheight")),
                             h3("Notes"),
                             helpText("The function 'loess' is used to produce locally weighted regression fits. Refer to its help page for details."),
                             helpText("Factor scores are based on based on maximum-likelihood factor analysis. See 'factanal' for details.")
                         )
                ),
                
                # Factors -----------------------------------------------------------------
                navbarMenu("Factors",
                           
                           tabPanel("Dimensionality Tests",
                                    h3("Parallel Analysis"),
                                    helpText("The parallel analysis below is based on the principal components of the data. The circles connected by the thick line show the empirical eigenvalues. The thin lines represent the eigenvalues of 20 simulations with normally distributed random data. A dimension/component is judged to be meaningful if its eigenvalue is larger than the eigenvalues obtained from random data."),
                                    plotOutput(outputId = "parallelanalysis"),
                                    h3("MAP Test"),
                                    helpText("The Minimum Average Partial (MAP) test works by computing the average of the squared (partial) correlations between the p variables of a data set after the first m = 1 to (p-1) principal components have been removed (partialled out). The suggestion is to retain components, for which this average reaches its minimum. The table below also shows the average squared correlation between the variables (no component removed; m = 0). If this value is the lowest in the table, there is probably no common variance to analyze. The maximum number of components removed is 20."),
                                    verbatimTextOutput(outputId = "maptest"),
                                    helpText("Graphical representation of the MAP test. The blue line shows the average squared correlation of the items."),
                                    plotOutput(outputId = "maptest.plot")
                           ),
                           
                           # EFA ####
                           tabPanel("Exploratory Factor Analysis",
                                    h3("Exploratory Factor Analysis and Principal Components Analysis"),
                                    helpText("Analyses are performed with iana::factoranalysis, which uses psych::principal, psych::fa, and psych::fa.poly."),
                                    h4("Factoring"),
                                    fluidRow(
                                        column(4, numericInput(inputId = "nFactors", 
                                                               label = "Number of factors:",
                                                               min = 1, max = 20, value = 1, step = 1)),
                                        column(4, selectInput(inputId = "faMethod",
                                                              label = "Factoring method:",
                                                              choices = c("Maximum likelihood" = "ml", 
                                                                          "Minimum residuals" = "minres", 
                                                                          "Principal axes" = "pa", 
                                                                          "Principal components" = "princomp"))),
                                        column(4, selectInput(inputId = "faRotation",
                                                              label = "Rotation:",
                                                              choices = c("varimax", "promax", "oblimin", "none", 
                                                                          "quartimax", "bentlerT", "geominT", "bifactor", 
                                                                          "simplimax", "bentlerQ", "geominQ",
                                                                          "biquartimin", "cluster"), 
                                                              selected = "promax")
                                        )
                                    ),                    
                                    fluidRow(
                                        column(8, checkboxInput(inputId = "faIRT", 
                                                                label = "Use polychoric correlations",
                                                                value = FALSE))
                                    ),
                                    h4("Markers"),
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
                                    h4("Output options"),
                                    fluidRow(
                                        column(4, numericInput(inputId = "faDigits", 
                                                               label = "Digits to show:",
                                                               min = 2, max = 8, value = 2, step = 1)),
                                        column(4, numericInput(inputId = "faItemlength", 
                                                               label = "Trim item text (characters):",
                                                               min = 20, max = 500, value = 250, step = 5))
                                    ),
                                    hr(),
                                    h3("Model fit"),
                                    fluidRow(column(12, tableOutput(outputId = "factorfit"))),
                                    h3("Factor loadings"),
                                    fluidRow(column(12, tableOutput(outputId = "loadings"))),
                                    p("F = Factor, M = Marker, a_j = Factor loadings, h2 = Communality, Cmpl = Factorial complexity"),
                                    h3("Factor variances"),
                                    fluidRow(column(12, tableOutput(outputId = "factorvariances"))),
                                    conditionalPanel(condition = "input.nFactors > 1",
                                                     h3("Factor correlations"),
                                                     fluidRow(column(12, tableOutput(outputId = "factorcorrelations")))),
                                    h3("Code"),
                                    p("The following code may be used to create data frames of items assigned to the factors. (Some items may need to be inverted.)"),
                                    verbatimTextOutput(outputId = "factorcode")
                           ),
                           
                           # CFA ####
                           tabPanel("Confirmatory Factor Analysis",
                                    h3("Confirmatory Factor Analysis"),
                                    helpText("Confirmatory factor analysis is performed with lavaan::cfa."),
                                    ###                    helpText("Because CFAs for a large number of variables are slow, the computation is suppressed if the number of variables exceeds the specified threshold."),
                                    br(),
                                    fluidRow(
                                        #                         column(4, 
                                        #                             sliderInput(inputId = "cfamaxvars", 
                                        #                                 label = "Threshold for computation:",
                                        #                                 min = 0, max = 100, value = 20,
                                        #                                 animate = TRUE)),
                                        column(4,
                                               selectInput(inputId = "cfaEstimator",
                                                           label = "Estimator:",
                                                           choices = c("ML", "MLM", "WLSMV")))
                                    ),
                                    br(),
                                    h4("Model specification"),
                                    helpText('Without explicit model specification, a one-factor model for all of the selected items is fit. For computing multi-factor models, check the "Use model specification" box and describe the model using Lavaan syntax in the editor frame below. Then click on "Evaluate" to compute the model.'),
                                    checkboxInput(inputId = "cfaUseModel", 
                                                  label = "Use model specification"),
                                    aceEditor(outputId = "cfaModelEditor",
                                              value = "", 
                                              mode = "r",
                                              fontSize = 14,
                                              ### wordWrap = TRUE, ### version 0.1 (on CRAN) does not have this argument (current version is 0.2.1)
                                              height = "200px",
                                              theme = "textmate"),
                                    actionButton(inputId = "cfaEvalModel", "Evaluate"),
                                    br(), br(),
                                    helpText("Reliability estimates should not be used with WLSMV. The omegas above the summary are reliability estimates based on the factor loadings. McDonalds (1999) version is omega3."),
                                    verbatimTextOutput(outputId = "cfa"),
                                    br(),
                                    strong("Reference"),
                                    helpText("McDonald, R. P. (1999). Test theory: A unified treatment. Mahwah, NJ: Erlbaum.")
                           )
                ),
                
                # Reliability -------------------------------------------------------------
                
                tabPanel("Reliability",
                    h3("Reliability and Item Statistics"),
                    helpText("The table shows item means, standard deviations, covariances between item scores and total score, item discriminations (i.e., item-total and item-remainder correlations), and the alpha obtained if the respective item is removed from the scale. At the bottom are statistics for the total score, i.e., the sum or the mean of the item scores for each person."),
                    checkboxInput(inputId = "reliabDetailed", 
                        label = "Show detailed output (psych::alpha)",
                        value = FALSE),
                    br(),
                    verbatimTextOutput(outputId = "reliability")
                ),
                
                
                tabPanel("Rasch",
                    h3("Rasch Model and Partial Credit Model"),
###                    actionButton(inputId = "fitrasch", label = " Run "),
###                    helpText('Press the "Run" button to fit or refit the model. (Fitting Rasch models is computationally intensive, therefore computations are not performed automatically.)'),
                    helpText("Analyses are performed with eRm::Rasch and eRm:PCM. The appropriate model is automatically chosen based on the values of the items. For binary items, a Rasch Model is fitted. For items with more than two reponse categories, a Partial Credit Model is fitted. (The former model is actually a special case of the latter, however, the output is somewhat different.)"),
                    selectInput(inputId = "raschOutputOptions",
                        label = "Output:",
                        choices = c("Model tests", 
                            "ICCs (only Rasch models)" = "iccs",
                            "Person-item map",
                            "Test and item information" = "testinfo",
                            "Item fit statistics" = "itemfit",
                            "Item parameters" = "itemstats",
                            "Person parameters and person fit" = "personstats"
                        )
                    ),
                    
                    conditionalPanel(
                        condition = "input.raschOutputOptions == 'Model tests'",
                        h3("Model tests"),
                        uiOutput(outputId = "factorsindf"),
                        verbatimTextOutput(outputId = "pcm.tests"),
                        radioButtons(inputId = "pcm.graphmodeltest.labels",
                            label = "Item labels:",
                            choices = c("Name" = "item",
                                "Number" = "number")),
                        plotOutput(outputId = "pcm.graphmodeltest", 
                            height = getOption("iana.plotheight"))
                    ),
                    conditionalPanel(
                        condition = "input.raschOutputOptions == 'iccs'",
                        h3("ICCs"),
                        selectInput(inputId = "rasch.icctype",
                            label = "Type of empirical ICC:",
                            choices = c(
                                # "Local polynomial regression" = "loess", 
                                "Tukey's (running median) smoothing" = "tukey",
                                # "Kernel regression" = "kernel", 
                                "Relative frequency" = "raw")),
                        plotOutput(outputId = "rasch.icc", 
                            height = getOption("iana.plotheight"))
                    ),
                    conditionalPanel(
                        condition = "input.raschOutputOptions == 'Person-item map'",
                        h3("Person-item map"),
                        helpText("Items with nonordinal (disordered) threshold locations are shown in red."),
                        checkboxInput(inputId = "pcm.sortitems", label = "Sort items by location"),
                        plotOutput(outputId = "pcm.pimap", height = getOption("iana.plotheight"))
                    ),
                    conditionalPanel(
                        condition = "input.raschOutputOptions == 'testinfo'",
                        h3("Test and item information"),
                        plotOutput(outputId = "pcm.info", height = getOption("iana.plotheight"))
                    ),
                    conditionalPanel(
                        condition = "input.raschOutputOptions == 'itemfit'",
                        h3("Item fit statistics"),
                        verbatimTextOutput(outputId = "pcm.itemfit")
                    ),
                    conditionalPanel(
                        condition = "input.raschOutputOptions == 'itemstats'",
                        h3("Item parameters and fit statistics"),
                        helpText("For the PCM, items coded with an origin of 1 are recoded to have an origin of 0. Therefore, the raw (sum) score to person parameter mapping also starts also with 0."),
                        verbatimTextOutput(outputId = "pcm.itemstats")
                    ),
                    conditionalPanel(
                        condition = "input.raschOutputOptions == 'personstats'",
                        h3("Person parameters and fit statistics"),
                        helpText("For the PCM, items coded with an origin of 1 are recoded to have an origin of 0. Therefore, the raw (sum) score to person parameter mapping also starts also with 0."),
                        verbatimTextOutput(outputId = "pcm.personstats")
                    )
                ),
                
                tabPanel("IRT",
                    h3("Multidimensional IRT models"),
                    helpText("Analyses are performed with mirt::mirt."),
                    fluidRow(
                        column(6, 
                            numericInput(inputId = "mirt_nfactors", 
                                label = "Number of dimensions:",
                                min = 1, max =5, value = 1, step = 1)),
                        column(6, 
                            selectInput(inputId = "mirt_model",
                                label = "Model:",
                                choices = c(
                                    "Rasch or PCM (only for 1 dimension)" = "Rasch", 
                                    "2PL" = "2PL",
                                    "Graded response model" = "graded",
                                    "Rating scale graded response model" = "grsm",
                                    "Generalized partial credit model" = "gpcm"
                                )
                            ))
                    ),
                    fluidRow(
                        column(6, 
                            selectInput(inputId = "mirt_rotate",
                                label = "Rotation:",
                                choices = c(
                                    'promax', 'oblimin', 'varimax', 'quartimin', 
                                    'oblimax', 'entropy', 'quartimax', 
                                    'simplimax', 'bentlerT', 'bentlerQ', 
                                    'tandemI', 'tandemII', 'geominT', 'geominQ',
                                    'cfT', 'cfQ', 'infomaxT', 'infomaxQ', 
                                    'mccammon', 'bifactorT', 'bifactorQ')
                                #### Not included (need an additional argument): 
                                #### 'targetT', 'targetQ', 'pstT', 'pstQ', 
                            )),
                        column(6, 
                            selectInput(inputId = "mirt_method",
                                label = "Method:",
                                choices = c(
                                    "EM" = "EM", 
                                    "Quasi-Monte-Carlo EM (QMCEM)" = "QMCEM",
                                    "MH-RM" = "MHRM"
                                )
                            ))
                    ),
                    helpText("Method: EM is suggested for fitting 1 or 2 dimensions, QMCEM for fitting 3 or more."),
                    verbatimTextOutput(outputId = "mirt.summary")
                ),
                
                navbarMenu("Info",
                    tabPanel("Help",
                        includeMarkdown("help.md")),
                    tabPanel("Some Infos",
                        h3("Some Infos"),
                        textOutput(outputId = "info"))
                )
            )
        )
    )
))


