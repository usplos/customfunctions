formula_generate = function(DV, IV, Cluster){
  library(tidyverse)
  IVL = length(IV)
  IVComB = c()
  for (ll in 1:IVL) {
    ComB = combn(IV,ll) %>% t()
    if(ll == 1){IVComB = c(IVComB, ComB[,1])}else{
      for (rr in 1:nrow(ComB)) {
        IVComB = c(IVComB, paste(ComB[rr,], collapse = ':'))
      }
    }
  }

  if(IVL == 3){
    RSlope = paste(IVComB, collapse = ' + ')
    RSlope = c(RSlope, paste(IVComB[-7], collapse = ' + '))
    for (nn in 4:6) {
      RSlope = c(RSlope, paste(IVComB[-c(nn,7)], collapse = ' + '))
    }

    for (nn in 4:6) {
      RSlope = c(RSlope, paste(IVComB[c(1:3,nn)], collapse = ' + '))
    }

    RSlope = c(RSlope, paste(IVComB[1:3], collapse = ' + '))

    for (nn in 1:3) {
      RSlope = c(RSlope, paste(IVComB[-c(nn,4:7)],collapse = ' + '))
    }

    for (nn in 1:3) {
      RSlope = c(RSlope, paste(IVComB[nn],collapse = ' + '))
    }

    RSlope = paste(' + ',RSlope,'')
    RSlope = c(RSlope, '')

    Formula = c()
    for (ni in 1:length(RSlope)) {
      for (ns in 1:length(RSlope)) {
        Formula = c(Formula,
                    paste0(DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                           '(1',RSlope[ns],'|',Cluster[1],') + ',
                           '(1',RSlope[ni],'|',Cluster[2],')'))
      }
      Formula = c(Formula,
                  paste0(DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                         '(1',RSlope[ni],'|',Cluster[2],')'))
    }

    for (ns in 1:length(RSlope)) {
      Formula = c(Formula,
                  paste0(DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                         '(1',RSlope[ns],'|',Cluster[1],')'))
    }
  }

  if(IVL == 2){
    RSlope = paste(IVComB, collapse = ' + ')
    RSlope = c(RSlope, paste(IVComB[-3], collapse = ' + '))
    for (nn in 1:2) {
      RSlope = c(RSlope, paste(IVComB[-c(nn,3)], collapse = ' + '))
    }

    RSlope = paste(' + ',RSlope,'')
    RSlope = c(RSlope, '')

    Formula = c()
    for (ni in 1:length(RSlope)) {
      for (ns in 1:length(RSlope)) {
        Formula = c(Formula,
                    paste0(DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                           '(1',RSlope[ns],'|',Cluster[1],') + ',
                           '(1',RSlope[ni],'|',Cluster[2],')'))
      }
      Formula = c(Formula,
                  paste0(DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                         '(1',RSlope[ni],'|',Cluster[2],')'))
    }

    for (ns in 1:length(RSlope)) {
      Formula = c(Formula,
                  paste0(DV,' ~ ',paste(IV, collapse = ' * '),' + ',
                         '(1',RSlope[ns],'|',Cluster[1],')'))
    }
  }
  # write_csv(tibble(Formula),paste0(DV, 'Formulas.csv'))
  return(Formula)
}
formula_generate_shiny = function(){
  ui <- fluidPage(
    titlePanel('SHINY formulas generator'),
    sidebarLayout(

      sidebarPanel(
        textInput('DV','Input the dependent variable:','Y'),

        selectInput('IVNumber','Select the number of fixed factors',choices = c(2,3)),

        textInput('IV1','Input the 1st factor:', 'A'),

        textInput('IV2','Input the 2nd factor:', 'B'),

        textInput('IV3','Input the 3rd factor:', 'C'),

        textInput('Cluster1','Input the 1st cluster variable:', 'Sub'),

        textInput('Cluster2','Input the 2nd cluster variable:', 'Item'),

        downloadButton("downloadData", "Download the formulas")
      )

      ,
      mainPanel(
        tableOutput("contents")
      )
    ))


  server <- function(input, output) {
    DV = reactive(input$DV)

    IVNumber = reactive(input$IVNumber)

    IV1 = reactive(input$IV1)

    IV2 = reactive(input$IV2)

    IV3 = reactive(input$IV3)

    Cluster1 = reactive(input$Cluster1)

    Cluster2 = reactive(input$Cluster2)


    Output = reactive(input$Output)

    output$contents <- renderTable({
      if (IVNumber() == 2) {
        formula_generate(DV = DV(),
                         IV = c(IV1(), IV2()),
                         Cluster = c(Cluster1(), Cluster2()))
      }else{
        formula_generate(DV = DV(),
                         IV = c(IV1(), IV2(),IV3()),
                         Cluster = c(Cluster1(), Cluster2()))
      }

    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$DV,'Formulas', ".csv", sep = "")
      },
      content = function(file) {
        write.csv(formula_generate(DV = DV(),
                                   IV = c(IV1(), IV2()),
                                   Cluster = c(Cluster1(), Cluster2())) %>% as_tibble(), file, row.names = FALSE)
      }
    )


  }

  print(shinyApp(ui, server))
}
