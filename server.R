library(shiny)
library(plotly)
library(dplyr)
library(DT)
library(shinyjs)

# need to fix state-wise aggregate sum of financial support!!!!

as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
}

# read in data file from the web as data
mydata <-
    read.csv(
        './HDWDataPortal.csv',
        sep = ',',
        header = T,
        na.strings = c("", "N/A", "NaN"),
        stringsAsFactors = F
    )
#
# remove row entries having "NA" in State.Abbreviation
delIndex <- which(mydata$State.Abbreviation == "NA")
mydata <- mydata[-delIndex,]
# remove row entries having "NA" in County.Name
delIndex <- which(is.na(mydata$County.Name))
# making sure that mydata does not contain NA in County.Name
mydata <- mydata[-delIndex,]
# #
# append "State_County" to the last column; it becomes the key for the plots because some states have the same county name.
mydata <-
    mydata %>% mutate("State_County" = paste0(State.Abbreviation, ', ', County.Name, sep = ''))
# #
# find out how many distinct County.Name in mydata[,5]
countyName <- levels(factor(mydata[, 5]))
numOfCounty <- length(countyName)

# # remove row entires having "", "N/A", "NaN" in County.Name
# mystates <- read.csv('../states.txt', sep = ',', header = F, skip = 8)
st_choices <- c(
    "AK", "AL", "AR", "AS", "AZ", "CA",
    "CO", "CT", "DC", "DE", "FL", "GA",
    "HI", "IA", "ID", "IL", "IN", "KS",
    "KY", "LA", "MA", "MD", "ME", "MH",
    "MI", "MN", "MO", "MP", "MS", "MT",
    "NC", "ND", "NE", "NH", "NJ", "NM",
    "NV", "NY", "OH", "OK", "OR", "PA",
    "PR", "RI", "SC", "SD", "TN", "TX",
    "UT", "VA", "VI", "VT", "WA", "WI",
    "WV", "WY"
)

yrs <- c("ALL", "2015", "2016", "2017", "2018")

noOfCounty <- rep(0, numOfCounty)

shinyServer(
    function(input, output, session) {
        # output selected state
        output$State <- renderText(input$state)
        
        # output status of selection
        output$Status <- renderText(input$state)
        
        # output selected year
        output$selectedYr <- renderText(input$year)
        
        # output selectedAll button
        output$SelectedAll <- renderText(input$selectedAll)
        
        # output clearAll button
        output$ClearAll <- renderText(input$clearAll)
        
        # for prediction tab
        output$selectedRange <- renderText(input$rangeSlider)
        
        # output PanelStatus: i.e. the active tabsetPanel
        output$PanelStatus <- renderText(input$tabsetPanelID)
        
        # initial plot using all the data (i.e. all years)
        output$FAplot <- renderPlotly({
            plot_ly(
                aggData(input$year, input$state),
                x = ~Award.Year,
                y = ~State.Abbreviation,
                z = ~sumOfFA,
                color = ~State.Abbreviation,
                type = 'scatter3d',
                opacity = 0.6
            ) %>% layout(
                         title = 'Sum of Financial Assistance (FA) for Each State over Time',
                         scene = list(aspectmode = "manual", 
                                      aspectratio = list(x = 1, y = 1, z = 1),
                                      xaxis = list(title = 'Year'),
                                      yaxis = list(title = 'State'),
                                      zaxis = list(title = 'Total FA')
                                      )
                         )
            
        })
        
        
        #
        # For the use in output$FAplot:
        #    aggregating financial assistance for each state and years
        aggData <- function(my_year, selectedStates) {
            if (sum(sapply(my_year, toupper) != 'ALL')) {
                
                mydata %>% filter(State.Abbreviation %in% as.vector(selectedStates) &
                                      Award.Year %in% my_year) %>% group_by(Award.Year, State.Abbreviation) %>% summarise(sumOfFA = sum(Financial.Assistance))
            } else {
                mydata %>% group_by(State.Abbreviation, Award.Year) %>% filter(State.Abbreviation %in% as.vector(selectedStates)) %>% summarise(sumOfFA = sum(Financial.Assistance))
            }
        }
        
        # For the use in output$pieChart
        #   filter mydata using my_year and selectedStates on State-County basis
        preData <- function(my_year, selectedStates) {
            if (toupper(my_year) != toupper('All')) {
                mydata %>% filter(Award.Year %in% my_year) %>% group_by(State.Abbreviation) %>% filter(State.Abbreviation %in% selectedStates)
            } else {
                mydata %>% group_by(State.Abbreviation) %>% filter(State.Abbreviation %in% selectedStates)
            }
        }
        
        # output$pieChart: pie chart that shows how the federal funding splitted among State_County
        output$pieChart <- renderPlotly(
            plot_ly(
                preData(input$year, input$state),
                labels = ~State_County,
                values = ~Financial.Assistance,
                type = 'pie'
            ) %>%
                layout(margin = list(
                    r = 100,
                    l = 70,
                    t = 20,
                    b = 70
                ))
        )
        
        # for the use in projectionChart analysis
        getData <- function(d) {
            selectedRange <- input$rangeSlider
            
            # select the data using the input year range
            tmpData <-
                d %>% filter(between(Award.Year, selectedRange[1], selectedRange[2])) %>% 
                    filter(State.Abbreviation %in% input$state) %>%
                    group_by(State_County)
            
            # find out the sum of the financial assistance per state_county
            tmpData_summary <-
                tmpData %>% summarise(sumOfFA = sum(Financial.Assistance)) %>% 
                    arrange(desc(sumOfFA))
            
            # sign adjustment;
            res <-
                tmpData %>% mutate(
                    diff_FA = ifelse(
                        Award.Year == selectedRange[1],
                        -1 * Financial.Assistance,
                        Financial.Assistance
                    )
                )
            
            # output sum of financial assistance for each state_county using datatable
            output$SummaryTbl_sumOfFA <- renderDataTable({
                datatable(tmpData_summary)
            })
            
            #print.data.frame(tmpData_summary)
            return(res)
            
        }
        
        # output projectionChart
        #  x - newer year
        #  y - older year
        output$projectionChart <- renderPlotly({
            tmpData <- getData(mydata)
            # check if nrow(tmpData) is empty or not
            if (nrow(tmpData) > 0) {
                plot_ly(
                    getData(mydata),
                    y = ~diff_FA,
                    x = ~State_County,
                    type = 'bar',
                    #marker = list(color = 'rgb(26, 118, 255)'),
                    hoverinfo = 'text',
                    text = ~paste(State_County,
                                   ',  $',
                                   Financial.Assistance,
                                   sep = '')
                ) %>%
                    layout(
                        xaxis = list(type = 'category', title = 'State County Name'),
                        yaxis = list(title = 'Difference in Financial Assistance')
                    )
                
            } else {
                # return empty plot
                plotly_empty()
            }
            
        })
        
        # for the use in output$SummaryTbl
        # given the selected years and states
        obtainInfo <- function(my_year) {
            if (toupper(my_year) == toupper("2015")) {
                #plotTitle <- paste0("Fiancial Assitance in ", my_year)
                grouped <- group_by(mydata,
                                    .dots = c("State.Abbreviation", "County.Name")) %>% filter(Award.Year == "2015") %>%
                    filter(State.Abbreviation %in% input$state)
                ans <-
                    summarise(grouped, Median = median(Financial.Assistance, na.rm = T)) %>% arrange(desc(Median))
                
            } else if (toupper(my_year) == toupper("2016")) {
                #plotTitle <- paste0("Fiancial Assitance in ", my_year)
                grouped <-
                    group_by(mydata,
                             .dots = c("State.Abbreviation", "County.Name")) %>% filter(Award.Year == "2016") %>%
                    filter(State.Abbreviation %in% input$state)
                ans <-
                    summarise(grouped, Median = median(Financial.Assistance, na.rm = T)) %>% arrange(desc(Median))
                
            } else if (toupper(my_year) == toupper("2017")) {
                #plotTitle <- paste0("Fiancial Assitance in ", my_year)
                grouped <-
                    group_by(mydata,
                             .dots = c("State.Abbreviation", "County.Name")) %>% filter(Award.Year == "2017") %>%
                    filter(State.Abbreviation %in% input$state)
                ans <-
                    summarise(grouped, Median = median(Financial.Assistance, na.rm = T)) %>% arrange(desc(Median))
                
            } else if (toupper(my_year) == toupper("2018")) {
                #plotTitle <- paste0("Fiancial Assitance in ", my_year)
                grouped <-
                    group_by(mydata,
                             .dots = c("State.Abbreviation", "County.Name")) %>% filter(Award.Year == "2018") %>%
                    filter(State.Abbreviation %in% input$state)
                ans <-
                    summarise(grouped, Median = median(Financial.Assistance, na.rm = T)) %>% arrange(desc(Median))
                
            } else {
                #plotTitle <- paste0("Fiancial Assitance from 2015 to 2018")
                grouped <-
                    group_by(mydata,
                             .dots = c("State.Abbreviation", "County.Name")) %>%
                    filter(State.Abbreviation %in% input$state)
                ans <-
                    summarise(grouped, Median = median(Financial.Assistance, na.rm = T)) %>% arrange(desc(Median))
            }
            
            names(ans)[1] <- "State"
            ans
            
            # output$SummaryTbl <- renderDataTable(ans, options = list(order = list(list(3, 'desc'))))
        }
        
        # summary of the median financial support
        output$SummaryTbl <- renderDataTable({
            DT::datatable(obtainInfo(YRS),
                          options = list(order = list(list(3, 'desc'))))
            
        })
        
        
        # hide/show buttons
        observeEvent(input$showButton, {
            toggle("state")
            toggle("clearAll")
            toggle("selectedAll")
            
        })
        
        # clear all selected states
        observeEvent(input$clearAll, {
            updateCheckboxGroupInput(
                session,
                'state',
                'State',
                choices = st_choices,
                selected = NULL,
                inline = F
            )
            
            output$FAplot <- renderPlotly({
                plotly_empty()
            })
            
            # print(
            #     paste0(
            #         "in observeEvent input$clearAll value:",
            #         input$clearAll,
            #         sep = ''
            #     )
            # )
            
        })
        
        # select all states
        observeEvent(input$selectedAll, {
            updateCheckboxGroupInput(
                session,
                'state',
                'State',
                choices = st_choices,
                selected = st_choices,
                inline = F
            )
            # print(
            #     paste0(
            #         "in observeEvent input$selectedAll value:",
            #         input$selectedAll,
            #         sep = ''
            #     )
            # )
            
            
        })
        
        observeEvent(input$state, {
            updateCheckboxGroupInput(
                session,
                'state',
                'State',
                choices = st_choices,
                selected = input$state,
                inline = F
            )
            
            output$FAplot <- renderPlotly({
                td <- aggData(input$year, input$state)
                if (nrow(td) > 0) {
                    # it is a bug because it won't display anything if there is only 1 state is selected.
                    # therefore, i add it here just to get around the problem.
                    td <- aggData(input$year, input$state)
                    plot_ly(
                        aggData(input$year, input$state),
                        x = ~Award.Year,
                        y = ~State.Abbreviation,
                        z = ~sumOfFA,
                        color = ~State.Abbreviation,
                        type = 'scatter3d',
                        opacity = 0.6
                    ) %>% layout(
                        title = 'Sum of Financial Assistance (FA) for Each State over Time',
                        scene = list(aspectmode = "manual", 
                                     aspectratio = list(x = 1, y = 1, z = 1),
                                     xaxis = list(title = 'Year'),
                                     yaxis = list(title = 'State'),
                                     zaxis = list(title = 'Total FA')
                        )
                    )
                    
                    
                    
                } else {
                    # output empty plot because there is nothing to show.
                    output$FAplot <- renderPlotly(plotly_empty())
                }
               
            })
        })
        
        
        # observing an input event from input$year
        observeEvent(input$year, {
       
            if (toupper(input$year) == toupper("2015")) {
            
                output$selectedYr <- renderText(input$year)
                output$State <- renderText(input$state)
                
                output$FAplot <- renderPlotly(
                    plot_ly(
                        aggData(input$year, input$state),
                        x = ~Award.Year,
                        y = ~State.Abbreviation,
                        z = ~sumOfFA,
                        color = ~State.Abbreviation,
                        type = 'scatter3d',
                        opacity = 0.6
                    ) %>% layout(
                        title = 'Sum of Financial Assistance (FA) for Each State over Time',
                        scene = list(aspectmode = "manual", 
                                     aspectratio = list(x = 1, y = 1, z = 1),
                                     xaxis = list(title = 'Year'),
                                     yaxis = list(title = 'State'),
                                     zaxis = list(title = 'Total FA')
                        )
                    )
                    
                    
                )
                output$SummaryTbl <-
                    renderDataTable(DT::datatable(obtainInfo('2015'),
                                                  options = list(order = list(
                                                      list(3, 'desc')
                                                      
                                                  ))))
                
                
                
            } else if (toupper(input$year) == toupper("2016")) {
                    output$selectedYr <- renderText(input$year)
                    output$State <- renderText(input$state)
                
                    
                    output$FAplot <- renderPlotly(
                            plot_ly(
                                aggData(input$year, input$state),
                                x = ~Award.Year,
                                y = ~State.Abbreviation,
                                z = ~sumOfFA,
                                color = ~State.Abbreviation,
                                type = 'scatter3d',
                                opacity = 0.6
                            ) %>% layout(
                                title = 'Sum of Financial Assistance (FA) for Each State over Time',
                                scene = list(aspectmode = "manual", 
                                             aspectratio = list(x = 1, y = 1, z = 1),
                                             xaxis = list(title = 'Year'),
                                             yaxis = list(title = 'State'),
                                             zaxis = list(title = 'Total FA')
                                )
                            )
                        )
                    output$SummaryTbl <-
                        renderDataTable(DT::datatable(obtainInfo('2016'),
                                                      options = list(order = list(
                                                          list(3, 'desc')
                                                          
                                                      ))))
                } else if (toupper(input$year) == toupper("2017")) {
                    output$selectedYr <- renderText(input$year)
                    output$State <- renderText(input$state)

                    output$FAplot <- renderPlotly(
                        plot_ly(
                            aggData(input$year, input$state), 
                            x = ~Award.Year,
                            y = ~State.Abbreviation,
                            z = ~sumOfFA,
                            color = ~State.Abbreviation,
                            type = 'scatter3d',
                            opacity = 0.6
                        ) %>% layout(
                            title = 'Sum of Financial Assistance (FA) for Each State over Time',
                            scene = list(aspectmode = "manual", 
                                         aspectratio = list(x = 1, y = 1, z = 1),
                                         xaxis = list(title = 'Year'),
                                         yaxis = list(title = 'State'),
                                         zaxis = list(title = 'Total FA')
                            )
                        )
        
                        )
                    output$SummaryTbl <-
                        renderDataTable(DT::datatable(obtainInfo('2017'),
                                                      options = list(order = list(
                                                          list(3, 'desc')
                                                          
                                                      ))))
                } else if (toupper(input$year) == toupper("2018")) {
                
                    output$selectedYr <- renderText(input$year)
                    output$State <- renderText(input$state)

                    output$FAplot <- renderPlotly(
                        plot_ly(
                            aggData(input$year, input$state),
                            x = ~Award.Year,
                            y = ~State.Abbreviation,
                            z = ~sumOfFA,
                            color = ~State.Abbreviation,
                            type = 'scatter3d',
                            opacity = 0.6
                        ) %>% layout(
                            title = 'Sum of Financial Assistance (FA) for Each State over Time',
                            scene = list(aspectmode = "manual", 
                                         aspectratio = list(x = 1, y = 1, z = 1),
                                         xaxis = list(title = 'Year'),
                                         yaxis = list(title = 'State'),
                                         zaxis = list(title = 'Total FA')
                            )
                        )
                        
                    )
                    output$SummaryTbl <-
                        renderDataTable(DT::datatable(obtainInfo('2018'),
                                                      options = list(order = list(
                                                          list(3, 'desc')
                                                          
                                                      ))))
                } else if (toupper(input$year) == toupper("All")) {
                    
                    output$selectedYr <- renderText(input$year)
                    output$State <- renderText(input$state)
    
                    output$FAplot <- renderPlotly(
                        plot_ly(
                            aggData(input$year, input$state),
                            x = ~Award.Year,
                            y = ~State.Abbreviation,
                            z = ~sumOfFA,
                            color = ~State.Abbreviation,
                            type = 'scatter3d',
                            opacity = 0.6
                        ) %>% layout(
                            title = 'Sum of Financial Assistance (FA) for Each State over Time',
                            scene = list(aspectmode = "manual", 
                                         aspectratio = list(x = 1, y = 1, z = 1),
                                         xaxis = list(title = 'Year'),
                                         yaxis = list(title = 'State'),
                                         zaxis = list(title = 'Total FA')
                            )
                        )
                        
                        
                        )
                    
                    output$SummaryTbl <-
                        renderDataTable(DT::datatable(obtainInfo(toupper(
                            'All'
                        )),
                        options = list(order = list(
                            list(3, 'desc')
                            
                        ))))
                }
                
                
            })
                
        })