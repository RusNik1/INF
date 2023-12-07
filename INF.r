library(shiny)
library(DT)
library(plotly)
library(leaflet)
library(data.table)
library(shinythemes)

# UI
ui <- fluidPage(
  titlePanel("Сайт публикаций в Scopus"),
  theme = shinytheme("cosmo"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Выберите файл CSV", accept = ".csv", multiple = FALSE),
      radioButtons("filter_by", "Фильтровать по:", choices = c("Страна", "Регион"), selected = "Страна"),
      selectInput("selected_region", "Выберите регион", choices = NULL),
      selectInput("selected_country", "Выберите страну", choices = NULL),
      selectInput("selected_type", "Выберите тип", choices = NULL),
      textInput("search_title", "Поиск по названию:"),
      downloadButton("download_csv", "Выгрузить в CSV"),
      downloadButton("download_pdf", "Выгрузить в PDF"),
          ),
    mainPanel(
      tabsetPanel(
        tabPanel("Таблица", DTOutput("table")),
        tabPanel("Карта", leafletOutput("map")),
        tabPanel("График", plotlyOutput("plot_cites")),
        tabPanel("Загруженные данные", verbatimTextOutput("uploaded_data_output"))
      ),
      fluidRow(
        column(12, tags$footer(""))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Загрузка данных из файла
  data <- reactiveVal(NULL)
  
  observe({
    req(input$file)
    loaded_data <- fread(input$file$datapath)
    
    # Создаем списки уникальных значений для фильтров
    countries <- sort(unique(loaded_data$Country))
    types <- sort(unique(loaded_data$Type))
    regions <- sort(unique(loaded_data$Region))
    updateSelectInput(session, "selected_country", choices = countries)
    updateSelectInput(session, "selected_type", choices = types)
    updateSelectInput(session, "selected_region", choices = regions)
    
    data(loaded_data)
  })
  
  # Табличное представление данных с фильтрами
  output$table <- renderDT({
    req(data())
    
    if (input$filter_by == "Страна") {
      filtered_data <- data() %>%
        filter(Country %in% input$selected_country,
               Type %in% input$selected_type,
               grepl(input$search_title, Title, ignore.case = TRUE))
    } else {
      filtered_data <- data() %>%
        filter(Region %in% input$selected_region,
               Type %in% input$selected_type,
               grepl(input$search_title, Title, ignore.case = TRUE))
    }
    
    datatable(filtered_data, options = list(lengthMenu = c(5, 10, 15), pageLength = 10))
  })
  
  # Графическое представление данных (Total Cites)
  output$plot_cites <- renderPlotly({
    req(data())
    plot_ly(data(), x = ~`Total Cites (3years)`, y = ~`Total Docs. (3years)`, type = "scatter", mode = "markers")
  })
  
  # Карта с градиентной заливкой
  output$map <- renderLeaflet({
    leaflet() %>% addTiles()
  })
  
  # Отображение загруженных данных
  output$uploaded_data_output <- renderPrint({
    if (!is.null(data())) {
      cat("Загруженные данные:\n")
      print(head(data()))
    } else {
      "Загрузите файл CSV, чтобы увидеть данные."
    }
  })
  
  # Сохранение данных в CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  # Выгрузка данных в PDF
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("data", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 8, height = 5)  
      
      # Табличное представление данных
      if (input$filter_by == "Страна") {
        filtered_data <- data() %>%
          filter(Country %in% input$selected_country,
                 Type %in% input$selected_type,
                 grepl(input$search_title, Title, ignore.case = TRUE))
      } else {
        filtered_data <- data() %>%
          filter(Region %in% input$selected_region,
                 Type %in% input$selected_type,
                 grepl(input$search_title, Title, ignore.case = TRUE))
      }
      
      # Проверка, что есть данные для отображения
      if (nrow(filtered_data) > 0) {
        # Статическое изображение таблицы
        print(DT::datatable(filtered_data, options = list(lengthMenu = c(5, 10, 15), pageLength = 10)))
      } else {
        cat("Нет данных для отображения.")
      }
      
      dev.off()
    }
  )
}

# Установка максимального размера загружаемого файла (5 ГБ)
options(shiny.maxRequestSize = 5 * 1024^3)

# Запуск приложения Shiny
shinyApp(ui, server)
