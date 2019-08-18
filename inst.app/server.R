library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)

server <- function(input, output, session) {
   
   #### CTV LEVEL ####
   time_series_monthly_ctv <- reactive({
      selected_ctvs <- input$ctvs_select
      selected_from <- input$year[1]
      selected_to <- input$year[2]
      
      filtered_data <- tutti_time_monthly_ctv %>%
         filter (ctv == selected_ctvs) %>%
         filter (month >= selected_from, month <= selected_to)
   })
   
   output$ctv_plot <- renderPlotly({
      data = time_series_monthly_ctv()
      p2<- ggplot(data) +
         geom_line(aes (month, total, color= data$ctv)) +
         theme(axis.text.x = element_text(angle = 90, hjust = 1))+
         scale_x_date(
            date_breaks = "1 month",
            date_labels = "%Y - %m")
            #showticklabels = TRUE,
            #tickangle = 45
            #+ scale_x_date(date_breaks = "1 year", date_labels = "%Y")
            
   
      
      ggplotly (p2)
   })
   
   #psycho specific
   time_series_monthly_sub <- reactive({
      selected_ctvs <- input$ctvs_select
      selected_sub <- input$subcategory_select
      selected_from <- input$year[1]
      selected_to <- input$year[2]
      
      merged_data <- 
         inner_join(tutti_time_monthly_package, packages_per_psy_sub, by = c("package" = "package")) %>%
         filter (month >= selected_from, month <= selected_to, category == selected_sub) %>%
         group_by(category, month) %>% 
         summarise(total = sum(total))
   })
   
   output$sub_plot <- renderPlotly({
      data = time_series_monthly_sub()
      sub_plot <- ggplot(data) +
         geom_line(aes (month, total, color= data$category)) +
         scale_x_date(
            date_breaks = "1 month",
            date_labels = "%Y - %m"
         )
      
      ggplotly (sub_plot)
   })
   
   
   downloads_per_ctv <- reactive({
      selected_ctvs <- input$ctvs_select
      selected_from <- input$year[1]
      selected_to <- input$year[2]
      
      monthly_avg_per_ctv <- tutti_time_monthly_ctv %>%
         filter (ctv == selected_ctvs) %>%
         filter (month >= selected_from, month <= selected_to) %>%
         select (ctv, total) %>%
         group_by(ctv) %>% # group by ctv
         summarise(avg = sum(total))
      
      count_pkg_per_ctv <- packages_per_ctv %>%
         select(ctv, package) %>%
         filter(ctv == selected_ctvs) %>%
         group_by(ctv) %>%
         summarise(count = n_distinct(package))
      
      merged_data <- inner_join(monthly_avg_per_ctv, count_pkg_per_ctv,  c("ctv" = "ctv")) %>%
         mutate(avg_pkg = avg / count)
      
   })

   
   output$downloads_per_ctv <- renderPlotly({
      data = downloads_per_ctv()
      p<- ggplot(data) +
         geom_col(aes (ctv, avg_pkg, fill= data$ctv))
      
      ggplotly(p)
   })
   
   pkg_per_ctv <- reactive({
   
      selected_ctvs <- input$ctvs_select
      selected_from <- input$year[1]
      selected_to <- input$year[2]
      
         count_pkg <-packages_per_ctv %>%
         select(ctv, package) %>%
         filter(ctv == selected_ctvs) %>%
         group_by(ctv) %>%
         summarise(count = n_distinct(package))
      })
   
   
   
   output$pkg_per_ctv <-renderTable({pkg_per_ctv()})
   
   
   #### PACKAGE LEVEL ####
   time_series_monthly_pkg <- reactive({
      selected_pkgs <- input$packages_select
      selected_from <- input$date_selection_package[1]
      selected_to <- input$date_selection_package[2]
      
      filtered_data <- tutti_time_monthly_package %>%
         filter (package == selected_pkgs) %>%
         filter (month >= selected_from, month <= selected_to)
   })
   
   output$pkg_plot <- renderPlot({
      data = time_series_monthly_pkg()
      ggplot(data) +
         geom_line(aes (month, total, color= data$package)) +
         scale_x_date(
            date_breaks = "1 month",
            date_minor_breaks = "1 month",
            date_labels = "%Y - %m"
         )
      
   })
   
   output$value <- renderPrint({
      input$checkboxGroup
   })
   
   
}

