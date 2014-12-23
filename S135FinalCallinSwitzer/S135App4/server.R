# server.R

library(shiny)
library(ggvis)


# load data from my computer
foo <- read.table(file = "PanTHERIA_1-0_WR05_Aug2008.txt", sep = "\t", header = TRUE)
bar <- read.table(file = "PanTHERIA_1-0_WR93_Aug2008.txt", sep = "\t", header = TRUE)

colnames(foo) <- colnames(bar)
foobar <- rbind(foo, bar)
colnames(foobar)


# string manipulation for column names
stuff <- substr(colnames(foobar), start = 6, nchar(colnames(foobar)) )
beg <- gsub(pattern = "_",replacement = "", x = substr(stuff, 1,2))
colnames(foobar) <- paste(beg, substr(stuff, start = 3, stop = length(stuff)), sep = "")

# replace -999 with NA's
foobar[foobar == -999] <- NA
foobar$id <- 1:nrow(foobar)

# remove duplicates
foobar <- foobar[!duplicated(foobar$Binomial), ]

shinyServer(function(input, output, session) {
     
     # what to display while hovering     
     tooltip <- function(x) {
          if(is.null(x)) return(NULL)
          row <- df1[df1$id == x$id, ]
          paste0(names(row)[1:3], ": ", 
                 format(row)[1:3], collapse = "<br />")
          
     }
     
     # string manipulation function for creating labels
     LableMake <-function(name){
          yl <- gsub('([[:upper:]])', ' \\1', 
                     strsplit(name, "_", 
                              fixed = TRUE)[[1]][1])
          label <- ifelse(
               length(strsplit(name, "_", fixed = TRUE)[[1]]) >1, 
               yes = paste(yl," (",
                    strsplit(name, "_", fixed = TRUE)[[1]][-1], 
                    ")", sep = ""), 
               no = yl)
          label
     } 
     
     
     
     # Combine the selected variables into a new data frame
     selectedData <- reactive({
          # data for plotting
          yax <- foobar[[input$ycol]]
          xax <- foobar[[input$xcol]]
          
          # data frame for plotting    
          df <- data.frame(y = yax, x = xax, 
                           Name = foobar$Binomial)
          
         
          
          # log transform, if needed
          if(input$rdo == 2){
               df$y <- log(df$y) 
          }
          if(input$rdio == 2){
               df$x <- log(df$x) 
          }
          
          # remove rows with infinity
          is.na(df) <- sapply(df, is.infinite)
          
          # remove any incomplete rows
          df <- na.omit(df)
          
          # set id for mouse hovering
          df$id <- 1:nrow(df)
          
          # return data frame
          df1 <<- df
          df
     })
     
     
     # render plot
     # note: I'm using the renderPlot() function to be able
     # to use reactive inputs.  I'm actually plotting empty
     # space below the first plot
     output$plot1 <- renderPlot({
          
          # string manipulation for labels
          ylab <- LableMake(input$ycol)
          xlab <- LableMake(input$xcol)
          
          # change labels for log transformation
          if(input$rdo == 2){
               ylab <- paste("Log", ylab) 
          }
          if(input$rdio == 2){
               xlab <- paste("Log", xlab)
          }
          
          selectedData %>% ggvis(x = ~x, y = ~y) %>%
               layer_model_predictions(
                    model = "lm", se = TRUE, stroke := "red", 
                    stroke.hover := "red", fill := "red", 
                    fill.hover := "red", opacity.hover := .9, 
                    opacity := .5, formula = y~x) %>% 
               layer_points(
                    fill.hover:="red",size.hover := 200, 
                    opacity := 0.2, opacity.hover := 1, 
                    key := ~id) %>%
               add_axis(
                    "x", orient = "top", ticks = 0, 
                     title = paste(ylab, "vs", xlab,"for mammals"),
                     properties = axis_props(
                          axis = list(stroke = "white"),
                          labels = list(fontSize = 0))) %>%
               
               # shows data when you hover with your mouse
               add_tooltip(tooltip) %>%
               
               # axis labels
               add_axis("x", title = xlab) %>% 
               add_axis("y", title = ylab, title_offset = 60) %>%
               
               # makes it small enough for "showcase mode"
               set_options(width=400,height=300) %>% 
               
               # binds to the webpage
               bind_shiny("ggvisplot")
          
          # overall plot options
          options = list(height = 400,width=600)
          
     })
     
     # display linear model coefficients if the 
     # "straight line" option is selected
     output$mytable <- renderTable({
          if(input$checkGroup == 2){               
               # get data from above
               yax <- selectedData()[, 1]
               slope <- selectedData()[, 2]
               
               # fit a linear model
               tab <<- lm(yax~slope)
               tab
          }
          else {
               tab <<- NA # makes sure to not return the wrong model
               return()
          }
     })
     
     # the stop button -- returns a linear model to R, 
     # if user has selected the "Yes" for 
     # "Show Linear Model"
     observe({
          if (input$submit == 0)
               return()
          stopApp(summary(tab))
     }) 
})