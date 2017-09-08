library(shiny)
library(ggplot2)
library(magrittr)
sir_open <-  function(t, state, params){
    with(as.list(c(state, params)),{
    N  <- S + I + R
    dS <- -beta * S * I/N + B - death * S
    dI <-  beta * S * I/N - (sigma + death) * I
    dR <-  sigma * I - death * R
    return(list(c(dS, dI, dR)))})
}


ui <- fluidPage(

    titlePanel("SIR Model With Birth and Death"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("beta",
                        "Contact Rate",
                        min = 0,
                        max = 50,
                        value = 3),
            sliderInput("sigma",
                        "Recovery Rate",
                        min = 0,
                        max = 50,
                        value = 3),
            sliderInput("death",
                        "Mortality Rate",
                        min = 0,
                        max = 50,
                        value = 0.02)


        ),


        mainPanel(
            plotOutput("distPlot")
        )
    )
)

                                        # Define server logic
server <- function(input, output) {

    output$distPlot <- renderPlot({
        params <- c(death = input$death, B = 10, sigma = input$sigma, beta = input$beta)
        at_t_0 <- c(S = 1e3, I = 10, R = 10)
        times  <- 0:260   ## weeks
        out    <- deSolve::ode(y = at_t_0, func = sir_open, time = times, parms = params)
        as.data.frame(out) %>%
            reshape2::melt(id.vars = "time") %>%
            ggplot(aes(time, value, color = variable)) + geom_point()

    })
}

                                        # Run the application
shinyApp(ui = ui, server = server)
