library(shiny)
library(ggvis)

# load in data from internet
# foo <- read.table(file = "http://esapubs.org/archive/ecol/e090/184/PanTHERIA_1-0_WR05_Aug2008.txt", sep = "\t", header = TRUE)
# bar <- read.table(file = "http://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR05_Aug2008.txt", sep = "\t", header = TRUE)

# load data from my computer
foo <- read.table(file = "PanTHERIA_1-0_WR05_Aug2008.txt", sep = "\t", header = TRUE)
bar <- read.table(file = "PanTHERIA_1-0_WR93_Aug2008.txt", sep = "\t", header = TRUE)

colnames(foo) <- colnames(bar)
foobar <- rbind(foo, bar)


# string manipulation for column names
stuff <- substr(colnames(foobar), start = 6, nchar(colnames(foobar)) )
beg <- gsub(pattern = "_",replacement = "", x = substr(stuff, 1,2))
colnames(foobar) <- paste(beg, substr(stuff, start = 3, stop = length(stuff)), sep = "")

# replace -999 with NA's
foobar[foobar == -999] <- NA
foobar$id <- 1:nrow(foobar)

# remove duplicates
foobar <- foobar[!duplicated(foobar$Binomial), ]



shinyUI(pageWithSidebar(
     headerPanel('Linear Relationships in Mammals'),
     sidebarPanel(
          
          # Select data with these
          selectInput('xcol', 
                      'X Variable', 
                      names(foobar)[-c(1:5, 36:40, 56)], 
                      selected=names(foobar)[[5]]),
          selectInput('ycol', 
                      'Y Variable', 
                      names(foobar)[-c(1:5, 36:40, 56)],
                      selected=names(foobar)[[15]]),
          
          # decide if you want log- transformations
          radioButtons("rdio", 
                       label = h5("X-Axis Transformation"),
                       choices = list("none" = 1, "log" = 2),
                       selected = 2),
          
          radioButtons("rdo", 
                       label = h5("Y-Axis Transformation"),
                       choices = list("none" = 1, "log" = 2),
                       selected = 2), 
          
          # display linear model
          radioButtons("checkGroup", 
                       label = h5("Show Linear Model"), 
                       choices = list("No" = 1, "Yes" = 2),
                       selected = 1),
          
          # return linear model, if you've selected "Show Linear Model"
          actionButton("submit", 
                       label = "Stop and Return Linear Model"),
          
          # show a cute photo of a cat!
          img(src = "cat.jpg", height=300, width = 300)
     ),
     mainPanel(
          
          # show interactive plot
          ggvisOutput("ggvisplot"), 
          
          # show table with regression values
          tags$head( 
               tags$style( 
                    HTML('#mytable table {border-collapse:collapse; } 
                    #mytable table th { transform: rotate(0 deg)}'))),
          column(6,tableOutput("mytable")), 
          
          # empty plot...I used it for the reactive functions
          plotOutput('plot1')  
     )
))
