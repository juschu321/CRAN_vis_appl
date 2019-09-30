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
      filter (ctv %in% selected_ctvs) %>%
      filter (month >= selected_from, month <= selected_to)
  })
  
  output$ctv_plot <- renderPlotly({
    data = time_series_monthly_ctv()
    p2 <- ggplot(data) +
      geom_line(aes (month, total, color = data$ctv)) +
      scale_x_date(date_labels = "%Y - %m")
    
    
    ggplotly (p2)
  })
  
  #psycho specific
  #
  time_series_monthly_sub <- reactive({
    selected_ctvs <- input$ctvs_select
    selected_sub <- input$subcategory_select
    selected_from <- input$year[1]
    selected_to <- input$year[2]
    
    merged_data <-
      inner_join(tutti_time_monthly_package,
                 packages_per_psy_sub,
                 by = c("package" = "package")) %>%
      filter (month >= selected_from,
              month <= selected_to,
              category == selected_sub) %>%
      group_by(category, month) %>%
      summarise(total = sum(total))
  })
  
  # plot of the psycho categories
  output$sub_plot <- renderPlotly({
    data = time_series_monthly_sub()
    sub_plot <- ggplot(data) +
      geom_line(aes (month, total, color = data$category)) +
      theme(legend.position = "bottom") +
      theme(legend.text = element_text(size = 5)) +
      scale_x_date(date_labels = "%Y - %m")
    
    ggplotly (sub_plot)
  })
  
  
  downloads_per_ctv <- reactive({
    selected_ctvs <- input$ctvs_select
    selected_from <- input$year[1]
    selected_to <- input$year[2]
    
    monthly_avg_per_ctv <- tutti_time_monthly_ctv %>%
      filter (ctv %in% selected_ctvs) %>%
      filter (month >= selected_from, month <= selected_to) %>%
      select (ctv, total) %>%
      group_by(ctv) %>% # group by ctv
      summarise(avg = sum(total))
    
    count_pkg_per_ctv <- packages_per_ctv %>%
      select(ctv, package) %>%
      filter(ctv %in% selected_ctvs) %>%
      group_by(ctv) %>%
      summarise(count = n_distinct(package))
    
    merged_data <-
      inner_join(monthly_avg_per_ctv, count_pkg_per_ctv,  c("ctv" = "ctv")) %>%
      mutate(avg_pkg = avg / count)
    
  })
  
  
  output$downloads_per_ctv <- renderPlotly({
    data = downloads_per_ctv()
    p <- ggplot(data) +
      geom_col(aes (ctv, avg_pkg, fill = data$ctv)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    ggplotly(p)
  })
  
  pkg_per_ctv <- reactive({
    selected_ctvs <- input$ctvs_select
    selected_from <- input$year[1]
    selected_to <- input$year[2]
    
    count_pkg <- packages_per_ctv %>%
      select(ctv, package) %>%
      filter(ctv %in% selected_ctvs) %>%
      group_by(ctv) %>%
      summarise(count = n())
  })
  
  
  
  output$pkg_per_ctv <- renderTable({
    pkg_per_ctv()
  })
  
  
  #### PACKAGE LEVEL ####
  time_series_monthly_pkg <- reactive({
    selected_pkgs <- input$packages_select
    selected_from <- input$date_selection_package[1]
    selected_to <- input$date_selection_package[2]
    
    filtered_data <- tutti_time_monthly_package %>%
      filter (package %in% selected_pkgs) %>%
      filter (month >= selected_from, month <= selected_to)
  })
  
  #line chart R-packages
  output$pkg_plot <- renderPlotly({
    shiny::validate(need(dim(time_series_monthly_pkg())[1] != 0, 'No Data Available'))
    
    data = time_series_monthly_pkg()
    ggplot(data) +
      geom_line(aes (month, total, color = data$package)) +
      theme(legend.position = "bottom") +
      theme(legend.text = element_text(size = 5)) +
      scale_x_date(date_labels = "%Y - %m")
    
  })
  
  
  filtered_packages_imp <- reactive({
    selected_min_importance <- input$importance_range[1]
    selected_max_importance <- input$importance_range[2]
    selected_dep <- input$dep_select
    
    hist_data <- importance_data %>%
      filter(count >= selected_min_importance,
             count <= selected_max_importance) %>%
      filter(type %in% selected_dep)
  })
  
  output$package_selection <- renderUI({
    filtered_packages <- filtered_packages_imp()
    
    selectizeInput(
      'packages_select',
      'packages to select',
      choices = filtered_packages$to,
      
      multiple = TRUE,
      options = list(maxItems = 10),
      selected = input$packages_select
    )
    
  })
  
  
  # importance histogram (preparation + selection)
  output$importance_hist <- renderPlotly({
    data <- filtered_packages_imp()
    data$type <- as.factor(data$type)
    
    selected_min_importance <- input$importance_range[1]
    selected_max_importance <- input$importance_range[2]
    
    if ((selected_max_importance - selected_min_importance) > 100) {
      ggplot(data, aes (data$count, fill = data$type)) +
        geom_histogram(bins = 50) +
        scale_fill_manual(values  = c(
          "depends" = "slategrey",
          "imports" = "tomato",
          "suggests" = "gold"
        ))
    } else {
      ggplot(data, aes (data$count, fill = data$type)) +
        geom_histogram(binwidth = 1) +
        scale_fill_manual(values  = c(
          "depends" = "slategrey",
          "imports" = "tomato",
          "suggests" = "gold"
        ))
    }
  })
  
  #select type of dependency
  output$value <- renderPrint({
    input$checkboxGroup
  })
  
  # dependency network
  output$dep_plot <- renderVisNetwork ({
    selected_pkgs <- input$packages_select
    
    selected_dep <- input$dep_select
    
    #vis.edges$type <- edgelist%>%
    #filter(type %in% selected_dep)
    
    vis.edges <- edgelist %>%
      filter(from %in% selected_pkgs, type %in% selected_dep) %>%
      distinct(from, to, type)
    
    # to avoid red error message in shiny
    shiny::validate(need(dim(vis.edges)[1] != 0, 'No Data Available'))
    
    unique_nodes_1 = data.frame(id = unique(vis.edges$from))
    unique_nodes_2 = data.frame(id = unique(vis.edges$to))
    
    vis.nodes <- unique(bind_rows(unique_nodes_1, unique_nodes_2))
    vis.nodes$title <- vis.nodes$id # Text on click
    vis.nodes$borderWidth <- 1
    vis.nodes$borderWidthSelected <- 3
    vis.nodes$selected <- vis.nodes$id %in% selected_pkgs
    vis.nodes$group <-
      ifelse(vis.nodes$selected == TRUE, "selected", "dependent")
    #vis.nodes$dep <- sample(c("depends", "imports", "suggests"), nrow(vis.nodes), replace = TRUE)
    
    
    #defining visualization of the dependency network
    vis.edges$color <-
      c("slategrey", "tomato", "gold")[vis.edges$type]
    vis.edges$width <- 4
    vis.edges$hoverWidth <- 7
    # vis.edges$arrows = c("to", "from")
    vis.edges$selfReferenceSize = 15
    
    ledges <- data.frame(
      color = c("slategrey", "tomato", "gold"),
      label = c("depends", "imports", "suggests")
    )
    #,selected_dep = c(1,2,3))
    
    
    visNetwork(vis.nodes, vis.edges, width = "100%") %>%
      visEdges(arrows = list (to = list(enabled = TRUE))) %>%
      visGroups(groupname = "selected",
                color = list(background = "gray", border = "black")) %>%
      visOptions(highlightNearest = list(
        enabled = T,
        degree = 1,
        hover = T
      )) %>%
      visLegend(main = "Legend",
                addEdges = ledges,
                useGroups = TRUE) %>%
      #visOptions(selectedBy = "dep")%>%
      visInteraction(keyboard = TRUE, tooltipDelay = 50)
  })
  
}
