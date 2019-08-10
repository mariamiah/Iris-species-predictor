# load tree model
load("Tree.RData")

# load color brewer library
library(RColorBrewer)

# create a color palette
palette <- brewer.pal(3, "Set2")

# install shiny
install.packages(("shiny"))

# load the shiny package
library(shiny)

# Install and load the tree package
install.packages("tree")
library(tree)

# Install and load library caret
install.packages("caret")
library(caret)

# Create User interface
ui <- fluidPage(
  titlePanel("Iris species predictor"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "petal.length",
        label = "Petal length(cm)",
        min = 1,
        max = 7,
        value = 4),
      sliderInput(
        inputId = "petal.width",
        label = "Petal width(cm)",
        min = 0.0,
        max = 2.5,
        step = 0.5,
        value = 1.5)),
    mainPanel(
      textOutput(
        outputId = "text"),
        plotOutput(
          outputId = "plot" )
      )
    )
)

# Create the server code
server <- function(input, output){
  output$text = renderText({
    # Create predictors
    predictors <- data.frame(
      Petal.Length = input$petal.length,
      Petal.Width = input$petal.width,
      Sepal.Length = 0,
      Sepal.Width = 0
    )
    # Make prediction
    prediction = predict(
      object = model,
      newdata = predictors,
      type = "class"
    )
    # Create prediction text
    paste(
      "The predicted species is ",
      as.character(prediction)
    )
  })
  output$plot = renderPlot({
    # create scatter plot by species
    plot(
      x = iris$Petal.Length,
      y = iris$Petal.Width,
      pch = 19,
      col = palette[as.numeric(iris$Species)],
      main = "Iris petal length vs. width",
      xlab = "Petal length(cm)",
      ylab = "Petal width(cm)"
    )
    # Plot the decision tree boundaries
    partition.tree(
      tree = model,
      label = "species",
      add = TRUE
    )
    # Draw predictor on plot
    points(
      x = input$petal.length,
      y = input$petal.width,
      col = "red",
      pch = 4,
      cex = 2,
      lwd = 2
    )
  })
}
# Create a shinny app
shinyApp(
  ui = ui,
  server = server
)
