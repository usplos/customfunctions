LMMRun_Parallel = function(df, DV=NULL, IV=NULL, Cluster=NULL, Ifrun = F, output = NULL,
                           Manual = F, Manualcodefilename = NULL, Ncore = 4, Family = NULL){

  library(lmerTest)
  library(tidyverse)

  if(!isTRUE(Manual)){
    Formulas = formula_generate(DV = DV, IV = IV, Cluster = Cluster)
  }else{
    Formulas = read_csv(Manualcodefilename) %>% .[[1]]
  }

  Model_RunOneCore = function(formula.id){

    library(lmerTest)
    if(is.null(Family)){
      M = lmer(data = df, as.formula(Formulas[formula.id]))
      MAIC = AIC(M)
      MBIC = BIC(M)
      MConverge = ifelse(length(M@optinfo$conv$lme4$messages[grep(pattern = 'Model failed to converge',
                                                                  x = M@optinfo$conv$lme4$message)]) > 0,
                         F,T)
      M.Singular = isSingular(M)
      try({
        R2.C = MuMIn::r.squaredGLMM(M)[[2]]
        R2.M = MuMIn::r.squaredGLMM(M)[[1]]
      },silent = F)
      resulttable = data.frame(formula = Formulas[formula.id],
                               R2.M = ifelse(exists('R2.M'),R2.M,NA),
                               R2.C = ifelse(exists('R2.C'),R2.C,NA),
                               AIC = MAIC,
                               BIC = MBIC,
                               Converge = MConverge,
                               Singular = M.Singular)
      return(resulttable)
    }else{
      M = glmer(data = df, as.formula(Formulas[formula.id]), family = Family)
      MAIC = AIC(M)
      MBIC = BIC(M)
      M.Singular = isSingular(M)
      MConverge = ifelse(length(M@optinfo$conv$lme4$messages[grep(pattern = 'Model failed to converge',
                                                                  x = M@optinfo$conv$lme4$message)]) > 0,
                         F,T)
      try({
        R2.C = MuMIn::r.squaredGLMM(M)[[2]]
        R2.M = MuMIn::r.squaredGLMM(M)[[1]]
      },silent = F)
      resulttable = data.frame(formula = Formulas[formula.id],
                               R2.M = ifelse(exists('R2.M'),R2.M,NA),
                               R2.C = ifelse(exists('R2.C'),R2.C,NA),
                               AIC = MAIC,
                               BIC = MBIC,
                               Converge = MConverge,
                               Singular = M.Singular)
      return(resulttable)
    }
  }

  if (isTRUE(Ifrun)) {
    tic = Sys.time()
    formula.ids = sample(1:length(Formulas), length(Formulas))
    cat(length(Formulas), 'LMM models are running with', Ncore, ' parallel cores..............\n\n')
    library(parallel)
    cl <- makeCluster(Ncore)
    clusterExport(cl, c('df','DV','IV','Cluster',
                        'Ifrun','Manual','Manualcodefilename',
                        'Family','Formulas'), envir = environment())
    Results.DF <- do.call('rbind',parLapply(cl,formula.ids, Model_RunOneCore))
    stopCluster(cl)
    write_csv(Results.DF,paste0('ModelInfo',output,'.csv'))
    print(Sys.time() - tic)
    return(Results.DF)
  }
}
LMMRun_Parallel_shiny = function(){
  ui <- fluidPage(
    titlePanel('SHINY select the best fitted linear mixed model'),
    sidebarLayout(

      sidebarPanel(
        textInput('DV','Input the dependent variable:','Y'),

        selectInput('IVNumber','Select the number of fixed factors',choices = c(2,3)),

        textInput('IV1','Input the 1st factor:', 'A'),

        textInput('IV2','Input the 2nd factor:', 'B'),

        textInput('IV3','Input the 3rd factor:', 'C'),

        textInput('Cluster1','Input the 1st cluster variable:', 'Sub'),

        textInput('Cluster2','Input the 2nd cluster variable:', 'Item'),

        checkboxInput('Ifrun','Whether to run the models',T),

        checkboxInput('Manual','Whether to run models based on existing formulas',F),

        textInput('mfile', 'Input file name containing existing formulas:',NULL),

        selectInput('Family', 'Select the distribution family of dependent variable:',
                    choices = c('gaussian','binomial','poisson')),

        sliderInput('Ncore','Set the number of parallel cores', min = 1, max = 20, value = 4, step = 1),

        textInput('Output', 'input the prefix name of ouput file:','Y'),

        fileInput("file1", "Choose CSV File of your data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),

        actionButton("update", "Update View")

      ),
      mainPanel(
        tableOutput("contents")
      )
    )
  )

  server <- function(input, output) {
    DV = reactive(input$DV)

    IVNumber = reactive(input$IVNumber)

    IV1 = reactive(input$IV1)

    IV2 = reactive(input$IV2)

    IV3 = reactive(input$IV3)

    Cluster1 = reactive(input$Cluster1)

    Cluster2 = reactive(input$Cluster2)

    Manual = reactive(input$Manual)

    Ifrun = reactive(input$Ifrun)

    FamilyD = reactive({
      switch(input$Family,
             "gaussian" = NULL,
             "binomial" = 'binomial',
             "poisson" = 'poisson')
    })


    Output = reactive(input$Output)

    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      mfile = reactive(input$mfile)

      if(is.null(mfile)){
        m = NULL
      }else{m = mfile()}

      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      d = read.csv(inFile$datapath, header = T)


      anovatable = eventReactive(input$update, {
        if(IVNumber() == 2){
          LMMRun_Parallel(df = d,
                          DV = DV(),
                          IV = c(IV1(), IV2()),
                          Cluster = c(Cluster1(), Cluster2()),
                          Manual = Manual(),Manualcodefilename = m, Family = FamilyD(),
                          Ifrun = Ifrun(),
                          Ncore = input$Ncore,
                          output = Output()) %>% arrange(Singular,-Converge,BIC)
        }else{
          LMMRun_Parallel(df = d,
                          DV = DV(),
                          IV = c(IV1(), IV2(),IV3()),
                          Cluster = c(Cluster1(), Cluster2()),
                          Manual = Manual(),Manualcodefilename = m, Family = FamilyD(),
                          Ifrun = Ifrun(),
                          Ncore = input$Ncore,
                          output = Output()) %>% arrange(Singular,-Converge,BIC)
        }}, ignoreNULL = FALSE)
      anovatable()
    })
  }

  print(shinyApp(ui, server))
}
