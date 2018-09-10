library(shiny)
library(plotly)
library(DT)
library(shinyjs)

st_choices <- c( "AK", "AL", "AR", 
                 "AS", "AZ", "CA", "CO", "CT", "DC", 
                 "DE", "FL", "GA", "HI", "IA", "ID", 
                 "IL", "IN", "KS", "KY", "LA", "MA", 
                 "MD", "ME", "MH", "MI", "MN", "MO",
                 "MP", "MS", "MT", "NC", "ND", "NE", 
                 "NH", "NJ", "NM", "NV", "NY", "OH", 
                 "OK", "OR", "PA", "PR", "RI", "SC", 
                 "SD", "TN", "TX", "UT", "VA", "VI", 
                 "VT", "WA", "WI", "WV", "WY")

yrs <- c("All", "2015", "2016", "2017", "2018")


shinyUI(# use a fluid boostrap layout
    fluidPage(
        # layout for checkboxGroupInput
        ## custom CSS for 3 column layout (used below for mechanics filter options)
        ## https://groups.google.com/forum/#!topic/shiny-discuss/DJhII5ZXJ88
        tags$head(tags$style(
            HTML(
                "
                .multicol {
                -webkit-column-count: 3; /* Chrome, Safari, Opera */
                -moz-column-count: 3; /* Firefox */
                column-count: 3;
                }
                "
            )
            )),
        # give the page a title
        titlePanel("Federal Financial Assistance for HIV/AIDS"),
        # generate a row with a sidebar
        sidebarLayout(
            # define the sidebar with inputs
            sidebarPanel(
                # select Year
                selectInput("year", "Year", choices = yrs, selected = 'All'),
                
                # sliderInput("rangeSlider", label = h3("Selected Years"), min = 2015,
                #              max = 2018, value = c(2015, 2016), dragRange = TRUE),
                
                
                # actionButton for selectAll
                shinyjs::useShinyjs(),
                actionButton("showButton", label = "Show/Hide"),

                
                # TODO: NEED TO FIGURE OUT HOW TO CONFIGURE THE LAYOUT OF CHECKBOXGROUPINPUT
                
                    hidden(
                        actionButton("clearAll", "Clear All"),
                        actionButton("selectedAll", "Select All"),
                        #actionButton("execute", "Execute"),
                        # fluidRow using css to control the layout for the checkboxGroupInput
                        checkboxGroupInput(
                            "state",
                            "State",
                            choices = st_choices,
                            selected = st_choices,
                            inline = F
                    
                        )
                        
                        # # TODO: NEED TO FIGURE OUT HOW TO CONFIGURE THE LAYOUT OF CHECKBOXGROUPINPUT
                        # fluidRow(
                        #     column(width = 3,
                        #            tags$div(class = "multicol",
                        #                     checkboxGroupInput("state", "State",
                        #                                        choices = st_choices,
                        #                                        selected = st_choices,
                        #                                        inline = F)
                        #                    )
                        #
                        #     ) # column
                        #) # fluidRow
                )
            ),
            # sidebarPanel
            mainPanel(
                tabsetPanel(
                    type = "tabs",
                    id = 'tabsetPanelID',
                    tabPanel(
                        "State-level Financial Assistance",
                        br(),
                        h4("States selected"),
                        verbatimTextOutput("Status"),
                        div(plotlyOutput("FAplot"), align = "left")
                    ),
                    # tabPanel
                    tabPanel(
                        "County-level Per State Financial Assistance",
                        br(),
                        h4("States selected"),
                        verbatimTextOutput("State"),
                        h4("Selected year:"),
                        span(textOutput("selectedYr"), style = "color:red"),
                        plotlyOutput("pieChart", width = 800,
                                     height = 500)
                        
                    ),
                    # tabPanel
                    tabPanel(
                        "County-level Median Financial Assistance",
                        br(),
                        # data table output
                        #
                        h3("County-level Median Financial Assistance:"),
                        # h3("Selected year:"),
                        # verbatimTextOutput("selectedYr"),
                        fluidPage(fluidRow(column(
                            12,
                            wellPanel(dataTableOutput("SummaryTbl"))
                        )))
                    )
                    ,
                    # tabPanel
                    tabPanel(
                        "Financial Assistance between 2 years",
                        br(),
                        helpText("You need to select two years. (Default: 2015 and 2016)"),
                        helpText("You can ignore the \'Year\' pulldown menu on the left."),
                        # data projection output
                        # fluidPage(
                        #    fluidRow(
                        #     column(12,
                        #            sliderInput("rangeSlider", label = h3("Selected Years"), min = 2015,
                        #                            max = 2018, value = c(2015, 2016)))
                        #    ),
                        #    paste0("Projection of financial assistance:"),
                        #    verbatimTextOutput("selectedRange"),
                        #    plotlyOutput("projectionChart", width = "auto", height = 'auto')
                        #    )
                        # fluidPage(
                        sliderInput(
                            "rangeSlider",
                            label = h3("Selected Years"),
                            min = 2015,
                            max = 2018,
                            value = c(2015, 2016)
                        ),
                        div(plotlyOutput("projectionChart"), height = "100%", align = "left"),
                        br(),
                        fluidPage(fluidRow(column(12,
                            wellPanel(dataTableOutput("SummaryTbl_sumOfFA")))
                        ))
                      
                        # )
                    ) # tabPanel
                ) # tabsetPanel
                
            ) # mainPanel
        ) # sidebarLayout
    ) # fluidPage
) # shinyUI
    