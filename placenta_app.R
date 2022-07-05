library(shiny)
library(quantreg)
library(quantregGrowth)
library(ggplot2)
library(plotly)
library(rhandsontable)
data= read.csv('dados_placenta_08mar2021.csv')

o <- gcrq(data$Diameter1 ~ ps(data$GA, monotone = 1), tau = c(.03,.10,0.25,.50,0.75,.90,.97))

o2 <- gcrq(data$Diameter2 ~ ps(data$GA, monotone = 1), tau = c(.03,.10,0.25,.50,0.75,.90,.97))

ui <- fluidPage(headerPanel(title = "APP2"),
                sidebarLayout(
                  sidebarPanel (
                    shiny::fileInput(inputId = "file_id1", label = "Upload d1 file"),
                    shiny::fileInput(inputId = "file_id2", label = "Upload d2 file"),
                    downloadButton("download_button1", "Download file d1"),
                    downloadButton("download_button2", "Download file d2")
                  ),
                  mainPanel(
                    tabsetPanel(
                      type = "tab",
                      tabPanel("D1table", rHandsontableOutput('d1tbl')),
                      tabPanel("D1plot", plotOutput('plt1')),
                      tabPanel("D2table", rHandsontableOutput('d2tbl')),
                      tabPanel("D2plot", plotOutput('plt2'))
                    )
                  )
                ))
server <- function(input, output, session) {
  d1 = reactive({
    file = input$file_id1
    if (is.null(file)) {
      return()
    }
    else {
      read.csv(file = file$datapath)
    }
  })
  d2 = reactive({
    file = input$file_id2
    if (is.null(file)) {
      return()
    }
    else {
      read.csv(file = file$datapath)
    }
  })

  values1 <- reactiveValues()
  values2 <- reactiveValues()
  observeEvent(input$do, {
    values1$data <-  hot_to_r(input$d1tbl)
    values2$data <-  hot_to_r(input$d2tbl)
    if (is.null(values1$data)) {
      return()
    }

  })
  output$d1tbl = renderRHandsontable(rhandsontable(
    d1(),
    rowHeaders = NULL,
    rownames = FALSE,
    editable = TRUE
  ))
  output$d2tbl = renderRHandsontable(rhandsontable(
    d2(),
    rowHeaders = NULL,
    rownames = FALSE,
    editable = TRUE
  ))
  output$NewIris <- DT::renderDataTable({
    values$data
  })
  output$plt1 <- renderPlot({
    
    
    plot.gcrq(o, xlab='GA (weeks)', ylab='Diameter 1 (cm)',legend = TRUE, grid=list(x=45,y=10),xaxt='n', yaxt='n')
    axis(1, at = seq(0, 42, by = 1), las=2)
    axis(2, at = seq(0, 35, by = 1), las=2)
    x1 = d1()[[1]]
    y1 = d1()[[2]]
    
    points(x1, y1, col='blaCK', pch=20)

    observe(str(input$tbl1_cell_edit))
    observeEvent(input$tbl1_cell_edit, {
      info = input$tbl1_cell_edit
      i = info$row
      j = info$col = info$col + 1  # column index offset by 1
      v = info$value
      info$value1d <-
        as.numeric(info$value)
      write.csv(x = d1(), file = "data_final_d2.csv")
    })
  })
  output$plt2 <- renderPlot({
    plot.gcrq(o2, xlab='GA (weeks)', ylab='Diameter 2 (cm)',legend = TRUE, grid=list(x=45,y=10),xaxt='n', yaxt='n')
    axis(1, at = seq(0, 42, by = 1), las=2)
    axis(2, at = seq(0, 35, by = 1), las=2)
    x2 = d2()[[1]]
    y2 = d2()[[2]]
    
    points(x2, y2, col='blaCK', pch=20)

  })
  output$download_button1 <- downloadHandler(
    filename = function() {
      paste('d1', 'csv', sep = '.')
    },
    content = function(con) {
      write.csv(hot_to_r(input$d1tbl), con, row.names = FALSE)
    }
  )
  output$download_button2 <- downloadHandler(
    filename = function() {
      paste('d2', 'csv', sep = '.')
    },
    content = function(con) {
      write.csv(hot_to_r(input$d2tbl), con, row.names = FALSE)
    }
  )
}
shinyApp(ui=ui, server=server)
