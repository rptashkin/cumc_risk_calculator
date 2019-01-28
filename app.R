library(shinyjs)

PAGE_TITLE <- "calculator"
# Define UI for application
ui <- fluidPage(
   #header image
    titlePanel(windowTitle = PAGE_TITLE,
    title =
    tags$a(href='https://www.columbiaobgyn.org/',
    tags$img(src='columbia_obgyn_logo.png',width = "50%", height = "50%" ))),

    h6(div(HTML("<em>\nSelect the model parameters below to calculate risk estimate:\n\n</em>"))),

    useShinyjs(),
    #create client side input form
    sidebarLayout(
        sidebarPanel(width = 5,
        div(
            id="form",
            selectInput(inputId = "pscore",label = "Procedure Score", choices = c(0,1,2,'3+')),
            sliderInput(inputId = 'age', label = 'Age', min=25, max=100,value=50, step=1, round=0),
            selectInput(inputId = "pralbum",label = "Albumin", choices = c('<3.5','3.5-4','>4'), selected = '>4'),
            selectInput(inputId = "asclas2",label = "ASA", choices = c(1,2,3,'4+'),selected = '1'),
            checkboxInput(inputId = 'esurg',label = "Elective Surgery"),
            selectInput(inputId = "ascites",label = "Ascites", choices = c('Yes','No'), selected = "No"),
            selectInput(inputId = "bleed",label = "Bleeding disorder", choices = c('Yes','No'), selected = "No"),
            actionButton("calc", "Calculate"),
            verbatimTextOutput("final_val")
            #div(img(src="nomogram.png",height = 200,wigth=400), style="text-align: center;")
            )),
        mainPanel()),

    #footer
    hr(),
    tags$a(href='https://github.com/rptashkin/cumc_risk_calculator',
        tags$img(src='github_logo.jpg',width = "5%", height = "5%" )),
    tags$a(href='mailto:r.ptashkin@gmail.com',
        tags$img(src='gmail_logo.png',width = "2.5%", height = "2.5%" , align="middle"))
  )

# Define server logic
server <- function(input, output) {
    paramValues <- reactive({

    # Compose data frame
    data.frame(
        Parameter = c("pscore", "age","pralbum","asclas2","esurg","ascites","bleed"),
        Selection = c(input$pscore,
            input$age,
            input$pralbum,
            input$asclas2,
            input$esurg,
            input$ascites,
            input$bleed),
        stringsAsFactors=FALSE)
    })

    #estimate risk score based on input model parameters:
    calculate <- function(pscore,age, pralbum, asclas, esurg, ascites, bleed) {
    pointsPerLP = 66.21
    intercept = -4.53792
    LP= intercept
    if (pscore==0){
        pscoreBeta = 0
        pscorePoints=0
    }
    else if(pscore==1){
        pscoreBeta = 0.3459
        pscorePoints=23
    }
    else if(pscore==2){
        pscoreBeta = 0.8133
        pscorePoints=54
    }
    else{
        pscoreBeta = 1.5103
        pscorePoints=100
    }
    if (age<50){
        ageBeta = 0.2753
        agePoints=18
    }
    else if(age>=50 & age<60){
        ageBeta = 0
        agePoints=0
    }
    else if(age>=60 & age<70){
        ageBeta = 0.3320
        agePoints=22
    }
    else if(age>=70 & age<80){
        ageBeta = 0.5909
        agePoints=39
    }
    else{
        ageBeta = 0.8990
        agePoints=60
    }
    if(esurg==TRUE){
        esurgBeta=0
        esurgPoints=0
    }
    else{
        esurgBeta=0.5442
        esurgPoints=36
    }
    if(ascites=="Yes"){
        ascitesBeta=0.4593
        ascitesPoints=30
    }
    else{
        ascitesBeta=0
        ascitesPoints=0
    }
    if(bleed=="Yes"){
        bleedBeta=1.0031
        bleedPoints=66
    }
    else{
        bleedBeta=0
        bleedPoints=0
    }
    if(pralbum=='<3.5'){
        pralbumBeta = 0.6595
        pralbumPoints=44
    }
    else if(pralbum=='3.5-4'){
        pralbumBeta = 0.3501
        pralbumPoints=23
    }
    else{
        pralbumBeta = 0
        pralbumPoints=0
    }
    if(asclas==1){
        asclasBeta = 0
        asclasPoints=0
    }
    else if(asclas==2){
        asclasBeta = 0.2503
        asclasPoints=17
    }
    else if(asclas==3){
        asclasBeta = 0.4755
        asclasPoints=31
    }
    else{
        asclasBeta = 1.0624
        asclasPoints=70
    }
    #calculate total points
        totalPoints = (pscorePoints + agePoints + esurgPoints
    + ascitesPoints + bleedPoints + pralbumPoints + asclasPoints)

    if(totalPoints==0){
        LP= intercept
    }
    else{
        LP = totalPoints /pointsPerLP+intercept
    }
    risk = exp(LP)/(1+exp(LP))
    return(risk)
  }

  output$final_tbl <- renderTable({
    if (input$calc == 0)
    return()
    isolate(paramValues())
  })

  output$final_val <- renderText({
    if (input$calc == 0){
        return()
    }
    else{
        isolate(paste("Estimated Risk: ", round(calculate(input$pscore,input$age,
        input$pralbum, input$asclas2, input$esurg, input$ascites, input$bleed),digits = 4)))
    }
  })

}
# Run the application
shinyApp(ui = ui, server = server)
