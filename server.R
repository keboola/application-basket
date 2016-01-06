# Application for visualising association rules

#load libraries
library(jsonlite)
library(googleVis)
library(shiny)
library(data.table)
library(lubridate)
library(networkD3)

#load keboola shared library
library(keboola.shiny.lib)

# server main
shinyServer(function(input, output, session) {
    # create instance of keboola helper library
    klib <- KeboolaShiny$new()
    
    keboola <- reactive({
        # what data do we need to get from SAPI
        tables <- list(
            cleanData = list(name='CLN__1'),
            columnTypes = list(name='VAI__1',reducible=FALSE),
            rules = list(name='ARL__1',reducible=TRUE),
            trl = list(name='TRL__1',reducible=FALSE),
            rli = list(name='RLI__1',reducible=FALSE)    
        )
        
        # start it up
        ret <- klib$startup(list(appTitle = "Basket Analysis",
                                  tables = tables,
                                  cleanData = TRUE,
                                  dataToSave = calculatedData,
                                  configCallback = configCallback,
                                  description = TRUE,
                                  customElements = NULL))
        
        
        # return sourceData object [the ()() is used because the sourceData() method returns a callable, so we call that also.]
        klib$sourceData()()
    })
    
    # get source data - some minor processing is done here that probably should get done prior to this point in the pipeline.
    sourceData <- reactive({
        sd <- keboola()
        if (length(sd) == 0) {
            print("Have not yet retrieved source data")
            NULL
        } else {
            # let's remove the braces 
            sd$rules$lhs <- gsub("[{}]","",sd$rules$lhs)
            sd$rules$rhs <- gsub("[{}]","",sd$rules$rhs)
            
            # make a rule column by concatenating the lhs -> rhs values
            sd$rules$rule <- gsub("[{}]","",do.call(paste, c(sd$rules[c("lhs","rhs")],sep=" => ")))
        
            # cast our rule metrics as numeric
            sd$rules$support <- as.numeric(sd$rules$support)
            sd$rules$confidence <- as.numeric(sd$rules$confidence)
            sd$rules$lift <- as.numeric(sd$rules$lift)
            
            # return list of sourceData data.frames
            sd
        }
    })
    
    # get any parameters passed through the descriptor
    params <- reactive({
        sourceData()$descriptor$parameters
    })
    
    # update the inputs with loaded data values
    observe({
        outdat <- sourceData()
        if (is.null(outdat)) {
            print("sourceData is Null, so nothing to observe yet.")
            return(0)
        } else {
            cd <- outdat$cleanData
            
            # updata our input elements with values from our sourceData
            # rangeVar - select of numeric columns for dynamic range inputs
            updateSelectInput(session, "rangeVar", 
                              choices = names(cd[sapply(cd, is.numeric)]))
            # dateVar - select of date/datetime columns for dynamic date range inputs
            updateSelectInput(session, "dateVar", 
                              choices = names(cd[sapply(cd, function (x) is.Date(x) | is.POSIXt(x))]))
            # categoryVar - select a category to add dynamic multiselects of the category values
            updateSelectInput(session, "categoryVar",
                              choices = names(cd[sapply(cd, is.factor)]))
            # groupVar - Grouping selector for the network graph
            updateSelectInput(session, "groupVar",
                              choices = names(cd[sapply(cd, is.factor)]))
            # simple category select for the small plot in the introduction tab
            updateSelectInput(session, "descCat",
                              choices = names(cd[sapply(cd, is.factor)]))
            # the column to use as a price for basket value calculations
            updateSelectInput(session, "priceVar", 
                              choices = c("None",names(cd)),
                              selected = "price")
            # the column to use as the count for basket value calculations (if None, 1 is used)
            updateSelectInput(session, "countVar", 
                              choices = c("None", names(cd)),
                              selected = "count")
            
            # here we set up the rules filtering sliders
            output$ruleSlidersUI <- renderUI({
                list(
                    sliderInput("support", "Support", 
                            min = min(round(outdat$rules$support,3)),
                            max = max(round(outdat$rules$support,3)),
                            value = c(min(round(outdat$rules$support,3)),max(round(outdat$rules$support,3)))),
                    sliderInput("confidence", "Confidence", 
                            min = min(round(outdat$rules$confidence,3)),
                            max = max(round(outdat$rules$confidence,3)),
                            value = c(min(round(outdat$rules$confidence,3)),max(round(outdat$rules$confidence),3))),
                    sliderInput("lift","Lift", 
                            min = min(round(outdat$rules$lift,3)),
                            max = max(round(outdat$rules$lift,3)),
                            value = c(min(round(outdat$rules$lift,3)),max(round(outdat$rules$lift,3))))
                )
            })
            return(TRUE)
        }
    })
    
    # this method is used for saving our app configuration back to KBC
    # Saving our state means we can load up previous saves and share with colleagues
    # this function is set in the startup method above, and will be used by the shared library
    # when a user loads a previously saved configuration
    configCallback <- function(session, config) {
        # get the selected configuration
        config <- klib$kfig$selectedConfig()
        
        # update our inputs to the state stored in config
        updateSelectInput(session, "descCat", selected = config$descCat)
        updateSliderInput(session, "support", value = c(config$support[1],config$support[2]))
        updateSliderInput(session, "confidence", value = c(config$confidence[1],config$confidence[2]))
        updateSliderInput(session, "lift", value = c(config$lift[1],config$lift[2]))
        
        # for dynamic elements (numeric and date ranges and category selectors)
        # each value of the select is itself an element with its own values
        updateSelectInput(session, "rangeVar", selected = config$rangeVar)
        for (range in config$rangeVar) {
            updateSliderInput(session,range,value=c(config[[range]][1],config[[range]][2]))
        }
        updateSelectInput(session, "dateVar", selected = config$dateVar)
        for (date in config$dateVar) {
            updateDateRangeInput(session,date,start=config[[date]][1],end=config[[date]][2])
        }
        updateSelectInput(session, "categoryVar", selected = config$categoryVar)
        for (cat in config$categoryVar) {
            updateSelectInput(session, cat, selected = config[[cat]])
        }
        
        # continue updating the remaining inputs
        updateSelectInput(session, "groupVar", selected = config$groupVar)
        updateSelectInput(session, "priceVar", selected = config$priceVar)
        updateSelectInput(session, "countVar", selected = config$countVar)
        updateSelectInput(session, "xVar", selected = config$xVar)
        updateSelectInput(session, "yVar", selected = config$yVar)
        updateSelectInput(session, "sizeVar", selected = config$sizeVar)
        updateSelectInput(session, "coulorVar", selected = config$colourVar)
    }
    
    # Get only rules that obey our filter inputs
    selectedRules <- reactive({
        rules <- sourceData()$rules
        # input$calculate is here so that what lies inside isolate({...}) 
        # will only be evaluated if the calculate button is pressed.
        input$calculate
        isolate({
            # return those rules that reside within the selections
            rules[which(
                rules$support > input$support[1] &
                    rules$support < input$support[2] &
                    rules$confidence > input$confidence[1] &
                    rules$confidence < input$confidence[2] &
                    rules$lift > input$lift[1] &
                    rules$lift < input$lift[2]
            ),]    
        })
    })
    
    # Get the filtered transaction sets
    selectedTrans <- reactive({
        outdat <- sourceData()$cleanData
        
        # as above, we isolate our evaluation to only happen on calculate button press
        input$calculate
        isolate({
            # loop through all dynamic range elements and filter transaction sets accordingly
            if (length(input$rangeVar) > 0) {
                for (i in 1:length(input$rangeVar)) {
                    rangeElem <- input$rangeVar[i]
                    if (length(input[[rangeElem]]) > 0) {
                        outdat <- outdat[which(
                            (outdat[,rangeElem] > input[[rangeElem]][1]) &
                                (outdat[,rangeElem] < input[[rangeElem]][2])), ]    
                    }
                }
            }
            # as above, but for dynamic date ranges
            if (length(input$dateVar) > 0) {
                for (i in 1:length(input$dateVar)) {
                    dateElem <- input$dateVar[i]
                    if (length(input[[dateElem]]) > 0) {
                        # new_interval is from the lubridate package
                        timeInterval <- new_interval(input[[dateElem]][1],input[[dateElem]][2])
                        outdat <- outdat[which(
                            as.Date(outdat[,dateElem]) %within% timeInterval),]        
                    }
                }
            } 
            # and for dynamic category columns
            if (length(input$categoryVar) > 0) {
                for (i in 1:length(input$categoryVar)) {
                    catlhs <- input$categoryVar[i]
                    if (length(input[[catlhs]]) > 0) {
                        outdat <- outdat[which(outdat[,catlhs] %in% input[[catlhs]]),]    
                    }
                }
            }
        })
        # return our filtered data.frame of transactions
        outdat
    })
    
    # in this method we attempt to run some simple calculations on the selected rules and transactions.
    # Note: Some of the manipulations may look unfamiliar here, that is because we're using the data.table package
    # (see https://github.com/Rdatatable/data.table)
    calculatedData <- reactive({
        rules <- data.table(selectedRules())
        trans <- selectedTrans()
        
        # which column contains transactionId, itemId, and minSupport value are available via LuckyGuess descriptor parameters
        transCol <- params()$transactionColumn
        itemCol <- params()$itemColumn
        minSupport <- params()$minSupport
        
        orders <- trans
        
        # we check if a countVar is selected and attempt to cast to numeric if so.  If not, set column to 1.
        if (input$countVar == "None") {
            orders$count <- rep(1,nrow(orders))
        } else {
            orders$count <- orders[,input$countVar]    
        }
        orders$count <- as.numeric(orders$count)
        
        # as for count variable, so for price. Although if None, price is 0.
        if (input$priceVar == "None") {
            orders$price <- rep(0,nrow(orders))
        } else {
            orders$price <- orders[,input$priceVar]    
        }
        orders$price <- as.numeric(orders$price)
        
        orders <- data.table(orders)
        orders$id_order <- orders[[transCol]]
        
        # get the trl (transRules) table.  
        # This table contains transactionId and ruleId so we can know which transactions support which rules.
        trl <- data.table(sourceData()$trl)
        
        # now we want only the selected rules so...
        trl[ruleid %in% rules[,ruleid],]
        
        # we also only want the transactions that have satisfied our filters...
        trl[transactionid %in% orders[,transCol],]
        
        # order by ruleid
        trl[order(ruleid)]
        
        # Here we add the columns: basket_size (number of items in the transaction) and basket_price (total price of the transaction)
        orderTotals <- orders[,.(basket_size = sum(count, na.rm = TRUE), basket_price = sum(count * price, na.rm = TRUE)), by = id_order]
        
        # now, we split the transRules table into a list of transactions for each rule. 
        trans_sets <- split(trl,trl$ruleid)
        # for each set of transactions that support a rule, we aggregate the price and count columns
        stats <- lapply(
            seq_along(unique(trl$ruleid)),
            function(x) {
                if (nrow(orderTotals[id_order %in% trl[ruleid == x,transactionid],]) > 0) {
                    data.table(
                        ruleid = x, 
                        avg_basket_size = mean(orderTotals[id_order %in% trl[ruleid == x, transactionid],basket_size]),
                        avg_basket_price = mean(orderTotals[id_order %in% trl[ruleid == x, transactionid],basket_price]),
                        total_basket_items = sum(orderTotals[id_order %in% trl[ruleid == x, transactionid],basket_size]),
                        total_basket_price = sum(orderTotals[id_order %in% trl[ruleid == x, transactionid],basket_price]),
                        num_baskets = length(orderTotals[id_order %in% trl[ruleid == x, transactionid],id_order])
                    )    
                }
            }
        )
        # now, recombine our list of transaction data.tables into a single table
        stats <- rbindlist(stats, fill=TRUE)
        
        # make sure our keys are correctly typed
        stats$ruleid <- as.integer(stats$ruleid)
        rules$ruleid <- as.integer(rules$ruleid)                                
        # set the key of our new table
        stats$ruleid <- as.integer(stats$ruleid)
        setkey(stats,ruleid)
        
        # set the key of the rules table
        setkey(rules,ruleid)
        
        # merge (inner join) the transaction info into our rule table
        outdat <- rules[stats,nomatch=0]
        
        if (input$priceVar == "None") {
            outdat$total_basket_price <- NULL
            outdat$avg_basket_price <- NULL
        }
        # return the result
        outdat    
    })
    
    # The networkD3 package requires us to do a little data manipulation to get the data in the correct shape for the graph
    networkData <- reactive({
        cdat <- calculatedData()
        # this table is the "rule items" table (columns ruleid, item)  
        # It shows which items contribute to which rule
        rli <- data.table(sourceData()$rli) 
        
        cln <- sourceData()$cleanData
        # add item count 
        rli[,`:=` (count = .N), by = item]
        # one line per item
        rli <- unique(rli[,.(item,count)])
        print(paste("So we have", nrow(rli),"items"))
        # add an id column (zero indexed)
        cln$name <- cln[,params()$itemColumn]
        # get the group variable 
        cln$cat <- cln[,input$groupVar]
        
        cln <- data.table(unique(cln[,c("name","cat")]))
        setkey(cln,name)
        setkey(rli,item)
        # join the tables to get our nodes data set
        nodes <- cln[rli]
        # add an id column (zero indexed)
        nodes$id <- as.numeric(rownames(nodes)) - 1
        
        nodes <- as.data.frame(nodes)
        
        # now for the links data
        rules <- as.data.frame(cdat)
        
        # for each rule, 
        for (i in 1:nrow(rules)) {
            
            lhsID <- nodes[nodes$name == rules[i,"lhs"], "id"]
            if (length(lhsID) > 0) {
                rules[i,"source"] <- lhsID
            }
            rules[i,"target"] <- nodes[nodes$name == rules[i,"rhs"], "id"]
        }
        # we only want rules where the source column is available.  This will omit rules with lhs > 1 item
        rules <- rules[complete.cases(rules$source),]
        
        # return a list with our nodes and links
        list(
            nodes = nodes,
            links = rules
        )
    })
    
    # server side for the networkD3 plot
    output$networkPlot <- renderForceNetwork({
        links <- networkData()$links
        nodes <- networkData()$nodes
        
        forceNetwork(Links = links, Nodes = nodes,
                     Source = "source", Target = "target",
                     Value = input$edgeVal, NodeID = "name",
                     Group = "cat", opacity = 0.8, legend = TRUE,
                     Nodesize = 'count', radiusCalculation = " Math.sqrt(d.nodesize)+6"
                     )
    })
    
    # this is the little plot on the introduction tab
    # note: this uses sourceData rather than the selectedRules selectedTrans so it is filter independent
    output$descPlot <- renderGvis({
        if (input$descCat == "") {
            return(NULL)
        }
        cd <- sourceData()$cleanData
        itemCol <- params()$itemColumn
        transCol <- params()$transactionColumn
        rules <- sourceData()$rules
        ruleItems <- data.table(sourceData()$rli)
        # count the number of rules each item supports
        ruleItemCount <- ruleItems[,ruleCount := .N, by = item]
        ric <- unique(ruleItemCount[,ruleid := NULL])
        
        transDat <- data.table(id_order = cd[,transCol], cat = cd[,input$descCat], item = cd[,itemCol])
        
        transDat <- transDat[,transCount := .N, by=list(cat,item)]
        transDat <- unique(transDat[,id_order := NULL])
        
        setkey(transDat, item)
        setkey(ric, item)
        
        # join ruleItemCount and TransactionCount on item
        newDat <- transDat[ric]
        # aggregate by category
        newDat <- newDat[,.(transCount = sum(transCount), ruleCount = sum(ruleCount)), by=cat]
        # create a bar chart
        bar <- gvisBarChart(newDat,xvar = "cat", yvar = c("ruleCount","transCount"),
                            options = list(title=input$descCat,height=300))
        bar
    })
    
    # main bubble chart using the googleVis library
    output$plot <- renderGvis({
        # throw up a progress bar so the user knows we're busy
        progressBar <- shiny::Progress$new(session, min = 1, max = 100)
        progressBar$set(message = 'Preparing plot', detail = 'Working hard...')
        progressBar$set(value= 20)
        
        calcData <- calculatedData()
        progressBar$set(value=60)
        
        # a list of bubble chart options
        bubopts <- list(
            height=400,
            sizeAxis="{minSize:1,maxSize:15}",
            bubble="{textStyle:{color:'none'}}",
            legend="{position:'none'}",
            vAxis=paste0("{title:'",input$yVar,"', minValue:'0.05'}"),
            hAxis=paste0("{title:'",input$xVar,"'}"),
            chartArea="{width: '80%', height: '80%'}")
        
        progressBar$set(value=80)
        # make the plot
        plot <- gvisBubbleChart(calcData,
                        idvar="rule",
                        xvar=input$xVar,
                        yvar=input$yVar,
                        sizevar=input$sizeVar,
                        colorvar=input$colourVar,
                        options=bubopts)
        progressBar$close()
        plot
    })
    
    # the data for the table tab
    tableData <- reactive({
        progressBar <- shiny::Progress$new(session, min = 1, max = 100)
        progressBar$set(message = 'Performing Calculations', detail = 'Thinking hard...')
        progressBar$set(value= 20)
        data <- as.data.frame(calculatedData())
        progressBar$set(value=55)
        if (input$priceVar == "None") {
            out <- data[,c("ruleid", "lhs","rhs","support","confidence","lift",
                    "avg_basket_size", "total_basket_items", "num_baskets")]    
        } else {
            out <- data[,c("ruleid", "lhs","rhs","support","confidence","lift",
                    "avg_basket_size","avg_basket_price",
                    "total_basket_items", "total_basket_price",
                    "num_baskets")]
        }
        progressBar$set(value=100)
        progressBar$close()
        out
    })
    # datatable server side with options
    output$table <- renderDataTable({
        tableData()
    }, options = list(
        pageLength = 10,
        lengthMenu = c(5,10,15,20,30,50,100)
    ))
    
    # annotations that show the currently selected filters and values
    # also isolated on input$calculate
    output$filterDescriptionsUI <- renderUI({
        input$calculate
        rules <- sourceData()$rules
        sd <- sourceData()$cleanData
        ret <- list()
        ret[[1]] <- h4("Currently Applied Filters")
        if (is.null(input$support)) {
            return(ret)
        }
        isolate({
            ret[[length(ret) + 1]] <- h4("on Transactions:")
            if (length(input$rangeVar) > 0) {
                for (i in 1:length(input$rangeVar)) {
                    elem <- input$rangeVar[i]
                    ret[[length(ret) + 1]] <- helpText(paste("Where",elem,">",input[[elem]][1],"and",elem,"<",input[[elem]][[2]]))
                }
            }
            if (length(input$dateVar) > 0) {
                for (i in 1:length(input$dateVar)) {
                    elem <- input$dateVar[i]
                    ret[[length(ret) + 1]] <- helpText(paste("Where",elem,">",input[[elem]][1],"and",elem,"<",input[[elem]][[2]]))
                }
            }
            if (length(input$categoryVar) > 0) {
                for (i in 1:length(input$categoryVar)) {
                    elem <- input$categoryVar[i]
                    ret[[length(ret) + 1]] <- helpText(paste("Where",elem,"is in",paste(input[[elem]],collapse=",")))
                }
            }
            
            ret[[length(ret) + 1]] <- h4("on Rules:")
            if (input$support[1] > min(rules$support) || input$support[2] < max(rules$support)) {
                ret[[length(ret) + 1]] <- helpText(paste("Where support >",input$support[1],"AND support <",input$support[2]))
            }
            if (input$confidence[1] > min(rules$confidence) || input$confidence[2] < max(rules$confidence)) {
                ret[[length(ret) + 1]] <- helpText(paste("Where confidence >",input$confidence[1],"AND confidence <",input$confidence[2]))
            }
            if (input$lift[1] > min(rules$lift) || input$lift[2] < max(rules$lift)) {
                ret[[length(ret) + 1]] <- helpText(paste("Where lift >",input$lift[1],"AND lift <",input$lift[2]))
            }    
        })
        ret
    })
    
    # The introduction tab user interface
    output$intro <- renderUI({
        rules <- sourceData()$rules
        orders <- sourceData()$cleanData
        div(
            h4("Rules Generation"),
            div(
                span("Out of a total of "),
                strong(length(unique(orders[,params()$transactionColumn]))),
                span("transactions, there were a total of "),
                strong(nrow(rules)),
                span(" rules generated with a support (occurence frequency) of at least "),
                strong(params()$minSupport)
            ),
            div(
                span("That means that the least frequent rule that can be included must appear at least"),
                strong(length(unique(orders[,params()$transactionColumn])) * as.numeric(params()$minSupport)),
                span(" times in the transaction set")
            ),
            h4("Mini Rule Explorer"),
            div(
                helpText("Note that the filters on the side panel do not effect the results on this page."),
                span("Here we can simply visualise some of our results."),
                span("For instance, we found the following columns were potential candidates for categorical values:"),
                flowLayout(
                    selectInput("descCat", "Category", names(orders[sapply(orders, is.factor)])),
                    span("Select a category variable to view how many rules were generated by it's members")
                ),
                htmlOutput("descPlot")
            ),
            h4("Exploring Further"),
            div("There are two other tabs on this page, and filtering options on the side-panel."),
            div("Firstly, if you have a 'price'-like column in your table, please select it in the upper left."),
            div("Please do likewise for the count column (Note, if you don't have a count column, a value of 1 will be used."),
            h5("Filters"),
            div("There are two tabs for filters.  One will filter transactions.  If, as a result of applying the filter, a rule will no longer have any supporting transactions, that rule will no longer show in the plot or the table."),
            div("These filters will alter the values of the aggregated basket statistics, but not the assosiation rule values."),
            div("There are also rule filters on the other tab.  Here you can limit which rules get displayed."),
            h5("The Plot Tab"),
            div("Now if you click on the Plot tab you will see a bubble chart display of the rules."),
            div("The defaults on the plot are confidence versus support, with lift being represented as the size of the bubble."),
            h5("The Table Tab"),
            div("On the table tab you'll see a table showing associations rules and some aggregated statistics about the supporting transactions for each rule."),
            h5("Saving Data"),
            div("If you would like to save the table back to your Storage API, you may do so by clicking the 'Save to KBC' button."),
            h4(strong("Happy Exploring!"))
        )
    })
    
    # this returns a list of range selectors containing an element for each selected element in rangeVar
    output$rangeUI <- renderUI({
        if (length(input$rangeVar) > 0) {
            sd <- sourceData()$cleanData
            config <- klib$kfig$selectedConfig()
            lapply(seq_along(input$rangeVar), function(x) {
                val <- c(
                    min(as.numeric(sd[,input$rangeVar[x]]), na.rm = TRUE),
                    max(as.numeric(sd[,input$rangeVar[x]]), na.rm = TRUE)
                )
                if (input$rangeVar[x] %in% names(config)) {
                    val <- config[[input$rangeVar[x]]]    
                }
                sliderInput(input$rangeVar[x], input$rangeVar[x],
                            min = min(as.numeric(sd[,input$rangeVar[x]]), na.rm = TRUE),
                            max = max(as.numeric(sd[,input$rangeVar[x]]), na.rm = TRUE),
                            value = val)
            })
        }
    })
    # like above, but returns a list of date range input elements rather than numerical sliders
    output$dateUI <- renderUI({
        if (length(input$dateVar) > 0) {
            sd <- sourceData()$cleanData
            config <- klib$kfig$selectedConfig()
            lapply(seq_along(input$dateVar), function(x) {
                start <- min(sd[,input$dateVar[[x]]])
                end <- max(sd[,input$dateVar[[x]]])
                if (input$dateVar[[x]] %in% names(config)) {
                    start <- config[[input$dateVar[x]]][1]
                    end <- config[[input$dateVar[x]]][2]
                }
                dateRangeInput(input$dateVar[[x]], input$dateVar[[x]],
                               min = min(sd[,input$dateVar[[x]]]),
                               max = max(sd[,input$dateVar][[x]]),
                               start = start,
                               end = end)    
            })
        }
    })
    # and, returns a list of multi-selects.  one for each selected categoryVar
    output$categoryUI <- renderUI({
        if (length(input$categoryVar) > 0) {
            sd <- sourceData()$cleanData
            config <- klib$kfig$selectedConfig()
            lapply(seq_along(input$categoryVar), function(x) {
                if (input$categoryVar[x] %in% names(config)) {
                    selectInput(input$categoryVar[x], input$categoryVar[x],
                                choices = levels(as.factor(sd[,input$categoryVar[x]])),
                                selected = config[[input$categoryVar[x]]],
                                multiple = TRUE)  
                } else {
                    selectInput(input$categoryVar[x], input$categoryVar[x],
                                choices = levels(as.factor(sd[,input$categoryVar[x]])),
                                multiple = TRUE)      
                }
            })
        }
    })
})

