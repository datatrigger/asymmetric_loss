# PACKAGES ----
library(shiny)
library(shinyWidgets)
library(tidyverse)

# UTILS (loss functions) ----
linex <- function(a=1, b=1, x){
    return(b*(exp(a*x)+(-a*x)-1))
}
square <- function(x){return(x^2)}

# UI ----
ui <- fluidPage(
    titlePanel(
        title = h1("Linear-exponential loss", align = "center"),
        windowTitle = 'LINEX'
    ),
    # img(src='linex_latex.png', align='center'),
    HTML('<center><img src="linex_latex.png" width="200"></center>'),
    br(),
    fluidRow(
        column(2),
        column(
            8,
            align='center',
            plotOutput("plot")
        ),
        column(2)
    ),
    br(),
    br(),
    fluidRow(
        column(2),
        column(
            8,
            align = 'center',
            wellPanel(
                sliderInput(
                    inputId = "a",
                    label = "a",
                    animate=TRUE,
                    value = 0,
                    min = 0.001,
                    max = 5,
                    step = 0.05
                )
            )
        ),
        column(2),
    )
)

# SERVER ----
server <- function(input, output){
    
    a <- reactive({
        input$a
    })
    
    # b <- reactive({
    #     input$b
    # })
    
    colors <- c("Linear Exponential Error" = "blue", "Squared Error" = "red")
    
    output$plot <- renderPlot({
        ggplot()+
            geom_function(fun = linex, args = list(a = a(), b = 2/(a()*a())), aes(color='Linear Exponential Error'), size=1.2)+
            geom_function(fun = square, aes(color='Squared Error'), size=1.2, alpha=0.8, linetype='longdash')+
            scale_x_continuous(limits = c(-1, 1), breaks=seq(-1, 1, 0.2))+
            scale_y_continuous(limits = c(0, 1), breaks=seq(0, 1, 0.1))+
            labs(x = "Error", y = "Loss", color = "Legend") +
            scale_color_manual(values = colors, name='Loss function')+
            theme_minimal()+
            theme(
                axis.text=element_text(size=12),
                axis.title=element_text(size=14),
                legend.key.size = unit(1, 'line'),
                legend.title = element_text(size=14),
                legend.text = element_text(size=12)
            )
    })
}

# APP ----
shinyApp(ui = ui, server = server)
