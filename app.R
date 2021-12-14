#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

#--------------------------------------------------------

ui <- pageWithSidebar(
        
        # Application title
        headerPanel("Modelisation du cycle de mue de la femelle Gammarus fossarum"),
        
        # Sidebar with a slider input for number of observations
        sidebarPanel(
            sliderInput("t", "Temps (jours) :", min = 0, max = 70, value = 1, step = 1),
            sliderInput("kappa", "Proportion kappa de la nourriture dediee au soma :", min = 0.5, max = 1, value = 0.9, step = 0.01),
            sliderInput("JV.YVA", "Flux net de synthese des structures de mue en mg/jour:", min = 0.01, max = 0.1, value = 0.05, step=0.005),
            sliderInput("JF", "Feeding rate en mg/jour/individu :", min=0.1, max=2.5, value=1, step=0.1),
            sliderInput("WSS", "biomasse assimilee contenue dans le buffer de stockage lors du passage en stade D1 : ", min=0.1, max=20, value=5, step=0.5),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("table"),
            plotOutput("hist", height = 500),
            imageOutput("diapo", height = 300)
        )
    )


#--------------------------------------------------------

server <- function(input, output) {

    WB0 <- 0.045
    yBA <- 0.95
    yAF <- 0.3
    
   #----------------
    output$table <- renderTable({
        jM = 0.135 * input$JF*yAF
        data.frame("Duree de la phase I"=input$WSS / (input$kappa*input$JF*yAF - jM),
                   "Duree de la phase II"= input$WSS / (input$JV.YVA + jM), 
                   "Nombre d'oeufs pondus" = trunc((yBA*(1-input$kappa)*input$JF*yAF*input$WSS)/(WB0*(input$kappa*input$JF*yAF - jM)))
        )
    })
    
    
  #----------------
    output$hist <- renderPlot({
        
        jM = 0.135 * input$JF*yAF
        if(input$t<= (input$WSS / (input$kappa*input$JF*yAF - jM))){
            print("phaseI")
            data <- data.frame(c("Structure","Stockage","Ponte"),
                               c(0,(input$kappa*input$JF*yAF - jM)*input$t,(1-input$kappa)*input$JF*yAF*input$t))
            colnames(data) = c("category","amount")
            
            #---
            ggplot(data, aes(x="", y=amount, fill=category)) +
              theme_classic() +
              ggtitle("Porportion de la masse totale associee aux differents compartiments en PHASE I") +
              geom_bar(stat="identity", width=1) +
              coord_polar("y", start=0) +
              geom_text(aes(label = paste0(round(amount/sum(amount)*100, digits = 1), "%")),
                        position = position_stack(vjust=0.5), size = 5, color=c("#55007e", "#00687e", "#329d00")) +
              labs(x = NULL, y = NULL, fill = "Compartiment") +
              theme(plot.title = element_text(size=13, face="bold"),
                    legend.title = element_text(size = 15, face="bold"),
                    legend.text = element_text(size = 15),
                    axis.line = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank()) +
              scale_fill_manual(values=c("#75FF33", "#33DBFF", "#BD33FF"))

            
            
        }else{ if(input$t<= (input$WSS / (input$JV.YVA + jM))){
            print("phaseII")
          data <- data.frame(c("Structure","Stockage","Ponte"),
                             c(0 + input$JV.YVA*(input$t - (input$WSS / (input$kappa*input$JF*yAF - jM))),
                               input$WSS - (input$JV.YVA + jM)*(input$t - (input$WSS / (input$kappa*input$JF*yAF - jM))),
                               (1-input$kappa)*input$JF*yAF*(input$WSS / (input$kappa*input$JF*yAF - jM))))
          colnames(data) = c("category","amount")
          

            #---
            ggplot(data, aes(x="", y=amount, fill=category)) +
              theme_classic() +
              ggtitle("Porportion de la masse totale associee aux differents compartiments en PHASE II") +
              geom_bar(stat="identity", width=1) +
              coord_polar("y", start=0) +
              geom_text(aes(label = paste0(round(amount/sum(amount)*100, digits = 1), "%")),
                        position = position_stack(vjust=0.5), size = 5, color=c("#55007e", "#00687e", "#329d00")) +
              labs(x = NULL, y = NULL, fill = "Compartiment") +
              theme(plot.title = element_text(size=13, face="bold"),
                    legend.title = element_text(size = 15, face="bold"),
                    legend.text = element_text(size = 15),
                    axis.line = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank()) +
              scale_fill_manual(values=c("#75FF33", "#33DBFF", "#BD33FF"))
            
            
        }else{
            print("t=end of phaseII= mue")

            data <- data.frame(c("Structure","Stockage","Ponte"),
                               c(0 + input$JV.YVA*((input$WSS / (input$JV.YVA + jM)) - (input$WSS / (input$kappa*input$JF*yAF - jM))),
                                 input$WSS - (input$JV.YVA + jM)*((input$WSS / (input$JV.YVA + jM)) - (input$WSS / (input$kappa*input$JF*yAF - jM))),
                                 (1-input$kappa)*input$JF*yAF*(input$WSS / (input$kappa*input$JF*yAF - jM))))
            colnames(data) = c("category","amount")
            
            
            #---
            ggplot(data, aes(x="", y=amount, fill=category)) +
              theme_classic() +
              ggtitle("Porportion de la masse totale associee aux differents compartiments a la FIN DE LA PHASE II (mue)") +
              geom_bar(stat="identity", width=1) +
              coord_polar("y", start=0) +
              geom_text(aes(label = paste0(round(amount/sum(amount)*100, digits = 1), "%")),
                        position = position_stack(vjust=0.5), size = 5, color=c("#55007e", "#00687e", "#329d00")) +
              labs(x = NULL, y = NULL, fill = "Compartiment") +
              theme(plot.title = element_text(size=13, face="bold"),
                    legend.title = element_text(size = 15, face="bold"),
                    legend.text = element_text(size = 15),
                    axis.line = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank()) +
              scale_fill_manual(values=c("#75FF33", "#33DBFF", "#BD33FF"))
            
        }}
        
    })
    
  #----------------
    output$diapo <- renderImage({
        
        jM = 0.135 * input$JF*yAF
        
        if (input$t<= (input$WSS / (input$kappa*input$JF*yAF - jM))) {
            return(list(
                src = "Diapositive10.jpg",
                contentType = "image/jpg",
                width = 600,
                heigh = 600,
                alt = "Phase I"
            ))
        } else {
            return(list(
                src = "Diapositive11.jpg",
                filetype = "image/jpeg",
                width = 600,
                heigh = 600,
                alt = "Phase II"
            ))
        }
        
    }, deleteFile = FALSE)
    
    
}


#----------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)
