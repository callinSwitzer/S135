all_values <- function(x) {
     if(is.null(x)) return(NULL)
     paste0(names(x), ": ", format(x), collapse = "<br />")
}

base <- mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
     layer_points()
base %>% add_tooltip(all_values, "hover")
base %>% add_tooltip(all_values, "click")

# The data sent from client to the server contains only the data columns that
# are used in the plot. If you want to get other columns of data, you should
# to use a key to line up the item from the plot with a row in the data.
mtc <- df
mtc$id <- 1:nrow(mtc)  # Add an id column to use ask the key

all_values <- function(x) {
     if(is.null(x)) return(NULL)
     row <- mtc[mtc$id == x$id, ]
     paste0(names(row), ": ", format(row), collapse = "<br />")
}

head(mtc)

mtc %>% ggvis(x = ~x, y = ~y, key := ~id) %>%
     layer_points() %>%
     add_tooltip(all_values, "hover")


## Try it myself
# data for plotting
yax <- log(foobar[[input$selec]])
xax <- log(foobar[[input$select]])


# ggvis plot  
yax <- foobar$BasalMetRate_mLO2hr
xax <- foobar$AdultBodyMass_g

df=data.frame(y = yax, x = xax, Name = foobar$Binomial)
df <- df[complete.cases(df),]
df$id <- 1:nrow(df)
print(head(df))

all_values <- function(x) {
     if(is.null(x)) return(NULL)
     row <- df[df$id == x$id, ]
     paste0(names(row)[1:3], ": ", format(row)[1:3], collapse = "<br />")
}


df %>% ggvis(x = ~x, y = ~y, key:= ~id) %>%
     layer_points(fill.hover:="red",size.hover := 200) %>% 
     #layer_model_predictions(stroke="lm",model = "lm", formula=y~x) %>% 
     add_tooltip(all_values) %>% 
     scale_numeric("x") %>%
     scale_numeric("y") %>%
     set_options(width=400,height=300) %>% 
     bind_shiny("ggvisplot")
options = list(height = 400,width=600)



# data for plotting
yax <- log(foobar[[input$selec]])
xax <- log(foobar[[input$select]])


# what to display while hovering
tooltip <- function(x) {
     if(is.null(x)) return(NULL)
     row <- df[df$id == x$id, ]
     paste0(names(row)[1:3], ": ", format(row)[1:3], collapse = "<br />")
}

# ggvis plot     
df=data.frame(y = yax, x = xax, Name = foobar$Binomial)
df <- df[complete.cases(df),]
df$id <- 1:nrow(df)
#print(head(df))

xlabel = "fogog"
ylabel = "oeoeo"

head(df)
df[1:2] <- log(df[1:2])

pd <- df %>% ggvis(x = ~x, y = ~y) %>%
     layer_points(fill.hover:="red",size.hover := 200, key := ~id) %>% 
     layer_model_predictions(stroke="lm",model = "lm", formula=y~x, se = TRUE) %>% 
     add_tooltip(tooltip) %>% 
#      scale_numeric("x", trans = "log", expand = 0) %>%
#      scale_numeric("y", trans = "log", expand = 0) %>%
     set_options(width=400,height=300) %>% 
     add_axis("x", title = xlabel) %>%
     add_axis("y", title = ylabel) 
pd

pd %>% scale_numeric("y", trans = "linear", expand = 0)%>%
     scale_numeric("x", trans = "linear", expand = 0)

pd %>% layer_paths(x = ~x, y = ~x, stroke := "blue", data = dff)
pdf %>% layer_model_predictions(model = "lm", stroke := "red",  dff[,1:2])
rm(data)


df <- df[,1:2]

pd <- df %>% ggvis(x = ~x, y = ~y) %>%
     layer_points(fill.hover:="red",size.hover := 200) %>% 
     layer_model_predictions(stroke="lm",model = "lm", formula=y~x) %>% 
     add_tooltip(tooltip) %>% 
     scale_numeric("x", trans = "log", expand = 0) %>%
     scale_numeric("y", trans = "log", expand = 0) %>%
     set_options(width=400,height=300) %>% 
     add_axis("x", title = xlabel) %>%
     add_axis("y", title = ylabel) 
pd

df1 = reactive({
     a = subset(foobar, select = LitterSize)
     return(a)
})


anscombe=as.data.frame(anscombe)
anscombereorder=anscombe[,c(1,5,2,6,3,7,4,8)]
anscombelong=data.frame(x=unlist(anscombe[,1:4]),
                        y=unlist(anscombe[,5:8]),
                        datasource=rep(1:4,each=11))

head(foobar[,-c(1:5)])
