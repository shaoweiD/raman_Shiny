shinyUI(fluidPage(
  titlePanel("Raman Spectra Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      tags$hr(),
      numericInput("Number",
                  "Number of samples in each group", 20),
      numericInput("Length",
                  "Length of Wavenumber", value = 3000),
      
      selectInput("panel1", "Plot:",
                  list("Average" = "1", 
                       "Plot with p-value < 0.05" = "2",
                       "Principal Component Analysis" = "3")),
      submitButton("Update View"),
      downloadButton('downloadData', 'Download PC scores')
    ),
    mainPanel(
      h4('Plot'),
      conditionalPanel(condition = "inputId == 'panel1'",plotOutput('contents')),
      h4("Summary"),
      verbatimTextOutput("summary")
    )
  )
))