# devtools::install_github("jcheng5/crosstalk")
# devtools::install_github("jcheng5/d3scatter")
# devtools::install_github("timelyportfolio/parcoords@feature/crosstalk")


# almost all of this code is copy/pasted from
#   https://github.com/jcheng5/d3scatter/blob/master/README.md
# Joe Cheng from RStudio deserves all the credit

library(parcoords)
library(d3scatter)
library(crosstalk)

sd <- SharedData$new(mtcars)

browsable(tagList(
  d3scatter(sd, ~wt, ~mpg, ~cyl, height = 200, width = 400),
  parcoords(sd, brushMode = "1d", height = 300, width = 500)
))



library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  fluidRow(
    column(width = 6,parcoordsOutput("parcoords1", height = 400)),
    column(width = 6,plotOutput("plot1", height = 400))
  ),
  fluidRow(
    textOutput("text1")
  )
)

server <- function(input, output, session){

  sd <- crosstalk::SharedData$new(mtcars, group = "A")

  output$parcoords1 <- renderParcoords({
    parcoords(sd, brushMode = "1d-mult", brushPredicate = "OR")
  })

  output$plot1 <- renderPlot({
    df <- sd$data(TRUE)
    df$selected_ <- factor(df$selected_, levels = c(TRUE,FALSE))

    if(any(is.na(df$selected_))) {
      ggplot(df, aes(x = wt, y = mpg)) + geom_point()
    } else {
      ggplot(df, aes(x = wt, y = mpg, alpha = selected_)) + geom_point() +
        scale_alpha_manual( values = c(1.0,0.2)) +
        guides(alpha = FALSE)
    }
  })

  output$text1 <- renderText({
    jsonlite::toJSON(sd$data(TRUE) %>% filter(selected_ == TRUE))
  })

}

shinyApp(ui, server)
