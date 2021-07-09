#install.packages("shiny")
library(shiny)
library(shinythemes)
library(ggplot2)


ui <- fluidPage(theme = shinytheme("cyborg"),
    
    headerPanel("Prédiction des prix des biens immobiliers à Paris"),
    
    sidebarPanel(

        textInput("hasYard","Dispose d'une cour : ",""),
        textInput("hasPool","Dispose d'une piscine : ",""),
        textInput("hasGuestRoom","Dispose d'une chambre d'amis : ",""),
        textInput("garage","Dispose d'un garage : ",""),
        textInput("hasStorageRoom","A une pièce de stockage : ",""),
        
        actionButton('go',"Prediction")
    ),
    
    mainPanel(
        sidebarPanel( width = 20,
          headerPanel("Prix estimé du bien immobilier: "),
          textOutput("value"),
          
          sidebarLayout(
              sidebarPanel(
                  sliderInput(inputId = "bins",
                              label = "Number of bins:",
                              min = 0,
                              max =30,
                              value = 20)
                  
              ),
              
            mainPanel(plotOutput("distPlot"))
          )
        )
          
                      ),
    
    
        
        mainPanel(
            plotOutput(outputId = "distPlot2")
            
        )
    )






server <- function(input, output) {
    
    output$distPlot2 <- renderPlot({
        
        x    <- faithful$waiting
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        hist(x, breaks = bins, col = "#75AADB", border = "white",
             xlab = "Evolution des pris sur 30 ans",
             main = "Evolution des pris des bien sur Paris")
        
    })
    

    
    output$distPlot <- renderPlot({
        hist(rnorm(input$bins))
    })
    
    data2 = reactiveValues()
    observeEvent(input$go,{
        
        data = read.csv("C:/house_price_pred/House/dataset.csv")
        View(data)
        summary(data)
        str(data)
        
        input_data = data[,c("hasYard","hasPool","garage","hasStorageRoom","hasGuestRoom","squareMeters","price")]
        
        
        data2$squareMeters <- as.numeric(input$bins)
        data2$hasYard <- as.numeric(input$hasYard)
        data2$hasPool <- as.numeric(input$hasPool)
        data2$garage <- as.numeric(input$garage)
        data2$hasStorageRoom <- as.numeric(input$hasStorageRoom)
        data2$hasGuestRoom <- as.numeric(input$hasGuestRoom)
        
        newPredict = data.frame(hasYard=data2$hasYard, hasPool=data2$hasPool, garage=data2$garage, hasStorageRoom=data2$hasStorageRoom, hasGuestRoom=data2$hasGuestRoom)

        model = lm(price ~ hasYard + hasPool + garage + hasStorageRoom + hasGuestRoom, data = input_data)
        summary(model)
        
        
        data2$op = predict(model, newPredict)
    })
    
    output$value <- renderPrint({data2$op})
}

shinyApp(ui, server)