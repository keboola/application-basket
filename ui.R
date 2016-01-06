
library(networkD3)
library(keboola.shiny.lib)

plotChoices <- c("support","confidence","lift","num_baskets", "avg_basket_size", "avg_basket_price",
                 "total_basket_items", "total_basket_size")

shinyUI(
    keboolaPage(
        fluidPage(
            column(4,            
                    flowLayout(
                       selectInput("priceVar", "Price Column", c("None")),
                       selectInput("countVar", "Count Column", c("None"))    
                   ),
                   h4("Filters:"),
                   wellPanel(
                       tabsetPanel(
                           tabPanel("Rules",
                                    helpText("Use these filters to change which rules are shown.",
                                             "Note: these filters will not affect the basket statistics associated with the rule"),
                                    flowLayout(
                                        uiOutput("ruleSlidersUI")
                                    )        
                           ), 
                           tabPanel("Transactions",
                                helpText("Use these filters to change which transactions are used for basket statistics calculations.",
                                         "Note: while rules not satisfying the filters will not be included, but the others will ",
                                         "keep their original support, confidence and lift values"), 
                                flowLayout(
                                     selectizeInput("rangeVar", "Data Ranges", choices = c(), multiple=TRUE),
                                     uiOutput("rangeUI")
                                ),
                                helpText("Filter transactions by date"),
                                flowLayout(
                                 selectizeInput("dateVar", "Date Ranges", choices = c(), multiple=TRUE),
                                 uiOutput("dateUI")
                                ),
                                helpText("Include only transactions that had certain values"),
                                flowLayout(
                                    selectInput("categoryVar", "Categories", c(), multiple=TRUE),
                                    uiOutput("categoryUI")
                                ) 
                            )
                            
                        ),
                        helpText("Click this button to apply your filters"),
                        actionButton("calculate", "Calculate")
                    
                   ),
                   uiOutput('filterDescriptionsUI')
                   
            ), 
            column(8, 
                tabsetPanel(
                   tabPanel("Overview", 
                            h3("Basket Analysis"),
                            uiOutput("intro")),
                   tabPanel("Plot",
                            htmlOutput('plot'),
                            fluidPage(
                                column(6,
                                       selectInput('yVar', 'Y Variable', plotChoices, selected="confidence"),
                                       selectInput('xVar', 'X Variable', plotChoices, selected="support")
                                ),
                                column(6,
                                       selectInput("sizeVar", "Size", plotChoices, selected="lift"),
                                       selectInput("colourVar", "Colour", c("lhs","rhs"), selected="lhs")
                                )    
                            )
                   ),
                   tabPanel("Rules Table", 
                            dataTableOutput(outputId="table")
                            
                   ),
                    tabPanel("Network Graph",
                        forceNetworkOutput("networkPlot"),
                        fluidPage(
                            column(6,
                                   selectInput("groupVar", "Group Nodes By:", c())),
                            column(6,
                                   selectInput("edgeVal", "Weight to be applied to network edges", plotChoices))
                                   
                        )
                    )
                )   
            )
        ), appTitle = "Basket Analysis"
    )
)
