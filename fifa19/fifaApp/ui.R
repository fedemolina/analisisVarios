# Define UI for application that draws a histogram
shinyUI(
    
    fluidPage(
        HTML('<style type="text/css">
         .well { background-color: #ffffff;}
        </style>'), 
        
        # Sidebar with a slider input for number of bins
        sidebarLayout(
            sidebarPanel(width = 3,
                         br(), br(), br(),
                         # style = "position:fixed;width:inherit;",
                         style = "position:fixed;width:22%;",
                         # h5("Construido con",
                         #    img(src = "shiny.png", height = "30px"),
                         #    "por",
                         #    img(src = "logo_iesta.png", height = "30px"),
                         #    "para",
                         #    img(src = "logo-ceibal.svg", height = "30px"),
                         #    "financiado por",
                         #    img(src = "ANII.png", height = "30px")),
                         
                         conditionalPanel("(input.navbarPage == 'general'",
                                          selectInput("covariable1",
                                                      "Elegir covariable", 
                                                      choices = names(dt),
                                                      selected = "value_million"
                                          )                 
                         ),
                         conditionalPanel("input.navbarPage == 'general' && input.tabsetGeneral !=='distribucion'",
                                          selectInput("covariable2",
                                                      "Elegir covariable", 
                                                      choices = names(dt),
                                                      selected = "wage_million"
                                          )                 
                         ),
                         conditionalPanel(condition = "input.navbarPage == 'general'",
                                          selectInput(inputId = "covariable3",
                                                      label = "covariable3",
                                                      choices = c("position_summary", "position_summary2", "preferred_foot"),
                                                      selected = "position_summary")
                         ),
                         conditionalPanel(condition = "input.navbarPage == 'general'",
                                          selectInput("func",
                                                      "función", 
                                                      choices = c("sum", "mean", "max", "min", "sd"),
                                                      selected = "mean"
                                          )                 
                        ),
                        conditionalPanel(condition = "input.navbarPage == 'general'",
                                         textInput("top_n",
                                                     "top_n", 
                                                     value = "10"
                                         )                 
                        ),
                        conditionalPanel(condition = "input.navbarPage == 'general' && input.tabsetGeneral == 'distribucion' ",
                                         textInput(label = "top %", 
                                                   inputId = "top_x_dist", 
                                                   value = "1"
                                         )                 
                        )
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                width = 9,
                br(), br(), br(),
                navbarPage(
                    title = "EDA FIFA",
                    id = "navbarPage",
                    position = "fixed-top", 
                    inverse = FALSE,
                    collapsible = TRUE,
                    tabPanel(
                        title = "Inicio",
                        value = 'inicio',
                        tabsetPanel(
                            type = "pills", 
                            id   = "tabsetInicio",
                            tabPanel(
                                title = "Introducción", 
                                value = 'intro'
                            ),
                            tabPanel(
                                title = "Codebook",
                                value = 'codebook'
                            )
                        )
                    ),
                    tabPanel(
                        title = "General", 
                        value = 'general',
                        tabsetPanel(
                            type = "pills", 
                            id   = "tabsetGeneral", 
                            
                            tabPanel(
                                title = "Mapa",
                                value = 'mapa',
                                fluidRow(
                                    column(12, plotlyOutput(outputId = "mapa", height = "auto", width = "auto"))#,
                                    # column(12, plotlyOutput(outputId = "kernel", width = "auto", height = "auto"))
                                )
                            ),
                            tabPanel(
                                title = "Dispersión",
                                value = 'dispersion',
                                fluidRow(
                                    column(12, plotOutput(outputId = "scatter"))
                                )
                            ),
                            tabPanel(
                                title = "Distribución",
                                value = "distribucion",
                                fluidRow(
                                    column(12, plotlyOutput(outputId = "dispersion", width = "auto", height = "auto"))
                                )
                            )
                        )
                    ),
                    
                    tabPanel(
                        title = "Ligas", 
                        value = 'ligas',
                        tabsetPanel(
                            type = "pills",
                            id = "tabsetLigas",
                            tabPanel(
                                title = "departamento",
                                value = "dpto1",
                                fluidRow(
                                    column(12, plotlyOutput(outputId = "densidadDpto", width = "auto", height = "auto")),
                                    column(12, plotOutput(outputId = "scatterDpto"))
                                )
                            ),
                            tabPanel(
                                title = "departamento2",
                                value = 'dpto2',
                                fluidRow(
                                    # column(12, plotOutput(outputId = "facetDpto"))
                                )
                            )
                        )
                    ),
                    tabPanel(
                        title = "Clubes", 
                        value = "clubes",
                        tabsetPanel(
                            type = "pills",
                            id = "tabsetEscuela",
                            tabPanel(
                                title = "Ranking",
                                value = "ranking",
                                fluidRow(
                                    plotlyOutput(outputId =  "top_n_clubes", height = "100%")
                                )
                            ),
                            tabPanel(
                                title = "extra",
                                value = "extra",
                                fluidRow(
                                    # column(12, plotOutput(outputId = "facetDpto"))
                                )    
                            )
                        )
                    ),
                    tabPanel(
                        title = "Jugadores",
                        value = "jugadores",
                        fluidRow(
                            plotlyOutput(outputId = "top_n_jugadores", width = "auto", height = "auto")
                            
                        )
                    )
                )
            )
        )
    ))
