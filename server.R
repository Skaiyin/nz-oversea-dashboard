# 加载必要的库
library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(tidyverse)
library(plotly)
library(treemapify)

imports_raw <- readRDS("./all_years_imports.rds")
exports_raw <- readRDS("./all_years_exports.rds")
hs_map <- readRDS("./hs_code_map.rds")
hs2_map <- readRDS("./hs2_group_map.rds")
truncate_text <- function(text, max_chars = 10) {
  ifelse(
    nchar(text) > max_chars,
    paste0(substr(text, 1, max_chars), "..."),
    text
  )
}

# 数据聚合函数
monthly_imports_agg <- function(data, ...) {
  data %>%
    group_by(...) %>%
    filter(imports_qty != 0) %>%
    summarise(
      Total_Imports_vfd = sum(imports_nzd_vfd, na.rm = TRUE),
      Total_Imports_cif = sum(imports_nzd_cif, na.rm = TRUE),
      Total_Imports_qty = sum(imports_qty, na.rm = TRUE)
    ) %>%
    mutate(
      Transport_Insurance_Cost = Total_Imports_cif - Total_Imports_vfd,
      unit_vfd = Total_Imports_vfd / Total_Imports_qty,
      unit_cif = Total_Imports_cif / Total_Imports_qty,
      unit_transport_insurance_cost = Transport_Insurance_Cost / Total_Imports_qty
    )
}

imports_overview_plot <- function(data, ...) {
  agg_data <- data %>%
    monthly_imports_agg(...)
  
  p <- ggplot(agg_data) +
    # 绘制 VFD (以百万为单位)
    geom_line(aes(x = display_period, y = Total_Imports_vfd / 1e6, color = "VFD"), linewidth = 1) +
    # 绘制 CIF (以百万为单位)
    geom_line(aes(x = display_period, y = Total_Imports_cif / 1e6, color = "CIF"), linewidth = 1) +
    labs(title = "Monthly Imports ($NZD vfd, $NZD cif) and Transport/Insurance Costs", 
         x = "Month", 
         y = "Total Imports ($NZD, in millions)") +
    theme_minimal() +
    scale_color_manual(values = c("blue", "red"))
}

monthly_exports_agg <- function(data, ...) {
  data %>%
    group_by(...) %>%
    filter((exports_nzd_fob != 0) | (reexports_nzd_fob != 0)) %>%
    summarise(
      Total_Only_Exports_nzd_fob = sum(exports_nzd_fob, na.rm = T),
      Total_Only_Exports_qty = sum(exports_qty, na.rm = T),           
      Total_Reexports_nzd_fob = sum(reexports_nzd_fob, na.rm = T),
      Total_Reexports_qty = sum(reexports_qty, na.rm = T),        
      Total_Exports_nzd_fob = sum(total_exports_nzd_fob, na.rm = T),  
      Total_Exports_qty = sum(total_exports_qty, na.rm = T)        
    ) %>%
    mutate(
      unit_fob = Total_Exports_nzd_fob / Total_Exports_qty,  
      unit_reexports_fob = Total_Reexports_nzd_fob / Total_Reexports_qty, 
      unit_only_exports_fob = Total_Only_Exports_nzd_fob / Total_Only_Exports_qty, 
      export_qty_proportion = Total_Only_Exports_nzd_fob / Total_Exports_nzd_fob, 
    )
}

exports_overview_plot <- function(data, ...){
  agg_data <- data %>%
    monthly_exports_agg(...)
  
  p1 <- ggplot(agg_data) +
    geom_line(aes(x = display_period, y = Total_Only_Exports_nzd_fob / 1e6, color = "Only_Exports"),  size = 1) +
    geom_line(aes(x = display_period, y = Total_Reexports_nzd_fob / 1e6, color = "Reexports"), size = 1) +
    geom_line(aes(x = display_period, y = Total_Exports_nzd_fob / 1e6, color = "Exports"), size = 1) +
    labs(title = "Monthly Imports fob($NZD)", 
         x = "Month", 
         y = "Total Imports ($NZD, in millions)") +
    theme_minimal() +
    scale_color_manual(values = c("blue", "red", "green"))
}


# # 数据准备
# geo_data <- bind_rows(
#   imports_raw %>% 
#     group_by(iso3) %>%
#     summarise(Value = sum(imports_nzd_cif)) %>%
#     mutate(Type = "Import"),
#   exports_raw %>%
#     group_by(iso3) %>% 
#     summarise(Value = sum(total_exports_nzd_fob)) %>%
#     mutate(Type = "Export")
# )

all_years <- unique(imports_raw$year)
default_year <- all_years[1]

# UI部分
ui <- dashboardPage(
  dashboardHeader(title = "New Zealand OverSea Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      # style = "position: fixed; overflow: visible;",
      menuItem("Overview", tabName = "tab_overview", icon = icon("tachometer-alt")),
      menuItem("data analysis", tabName = "tab_analysis", icon = icon("chart-line")),
      menuItem("Set up", tabName = "tab_settings", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")),
    fluidRow(
      box(title = "Select the time and display granularity", status = "primary", solidHeader = TRUE, width = 12,
          selectInput("time_range", "Select a time range:", 
                      choices = c("All Time" = "all_time", "Specific Year" = "specific_year", "Custom Range" = "custom_range"),
                      selected = "all_time"),
          uiOutput("year_range_ui"),  # 动态显示年份选择或日期范围选择
          uiOutput("time_period_ui"),
      ),
    ),
    tabItems(
      # 首页 Tab
      tabItem(tabName = "tab_overview",
              fluidRow(
                box(status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("overview_plot")
                ),
              ),
              fluidRow(
                box(status = "primary", solidHeader = TRUE, width = 12,
                    tabsetPanel(
                      tabPanel("Import", 
                               fluidRow(
                                 column(12,
                                        plotlyOutput("geo_imports")
                                 )
                               ),
                               fluidRow(
                                 column(12,
                                        plotlyOutput("top10_import_countries_value")
                                 )
                               ),
                               fluidRow(
                                 column(12,
                                        plotlyOutput("top10_import_commodities_combined")
                                 )
                               ),
                               fluidRow(
                                 column(12, 
                                        plotlyOutput("import_yoy_growth")
                                 )
                               ),
                               fluidRow(
                                 column(12, 
                                        plotlyOutput("import_mom_growth")
                                 )
                               )
                      ),
                      tabPanel("Export", 
                               fluidRow(
                                 column(12,
                                        plotlyOutput("geo_exports")
                                 )
                               ),
                               fluidRow(
                                 column(12,
                                        plotlyOutput("top10_export_countries_value")
                                 )
                               ),
                               fluidRow(
                                 column(12,
                                        plotlyOutput("top10_export_commodities_combined")
                                 )
                               ),
                               fluidRow(
                                 column(12, 
                                        plotlyOutput("export_yoy_growth")
                                 )
                               ),
                               fluidRow(
                                 column(12, 
                                        plotlyOutput("export_mom_growth")
                                 )
                               )
                      )
                    )
                )
              )
      ),
      
      # 数据分析 Tab
      tabItem(tabName = "tab_analysis",
              fluidRow(
                box(title = "Product category selection", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(3,
                             selectInput("hs2_group_select", "HS2 group:", 
                                         choices = c("ALL"), 
                                         selected = "ALL")
                      ),
                      column(3,
                             selectInput("hs2_select", "HS2 code:", 
                                         choices = c("ALL"), 
                                         selected = "ALL")
                      ),
                      column(3,
                             selectInput("hs4_select", "HS4 code:", 
                                         choices = c("ALL"), 
                                         selected = "ALL")
                      ),
                      column(3,
                             selectInput("hs6_select", "HS6 code:", 
                                         choices = c("ALL"), 
                                         selected = "ALL")
                      ),
                      column(4,
                             selectInput("hscode_select", "Full HS Code:", 
                                         choices = c("ALL"), 
                                         selected = "ALL")
                      ),
                      # column(4,
                      #        div(
                      #          textInput("hscode_input", "Directly enter HS Code:", ""),
                      #          actionButton("search_hscode", "Query", icon = icon("search"), 
                      #                       style = "margin-top: 5px; width: 100%;")
                      #        )
                      # )
                    ),
                    fluidRow(
                      column(12,
                             selectInput("country_select", "Country:", 
                                         choices = c("ALL"), 
                                         selected = "ALL")
                      ),
                    )
                  ),
                  
              ),
              fluidRow(
                column(12,
                       box(title = "Import and export trends of currently selected HS code", status = "info", solidHeader = TRUE, width = 12,
                           plotlyOutput("current_hs_trend")
                       )
                ),
              ),
              fluidRow(
                column(12,
                       box(status = "primary", solidHeader = TRUE, width = 12,
                           tabsetPanel(
                             # 进口标签页
                             tabPanel("Import", 
                                      fluidRow(
                                        column(12,
                                                plotlyOutput("current_hs_top10_import_countries")
                                        )
                                      ),
                                      fluidRow(
                                        column(12,
                                               DT::dataTableOutput("sublevel_import_table")
                                        )
                                      ),
                                      fluidRow(
                                        column(12,
                                               plotlyOutput("sublevel_import_trend")
                                        )
                                      ),
                                      fluidRow(
                                        column(12,
                                               plotlyOutput("hs_subcategory_countries_import_sankey", height = 500)
                                        )
                                      ),
                                      fluidRow(
                                        column(12,
                                               plotlyOutput("continent_import_sankey", height = 500)
                                        )
                                      ),
                                      # fluidRow(
                                      #   column(12,
                                      #          plotOutput("import_treemap", height = 500)
                                      #   )
                                      # )
                             ),
                             
                             # 出口标签页
                             tabPanel("Export",
                                      fluidRow(
                                        column(12,
                                               plotlyOutput("current_hs_top10_export_countries")
                                        )
                                      ),
                                      fluidRow(
                                        column(12,
                                                DT::dataTableOutput("sublevel_export_table")
                                        )
                                      ),
                                      fluidRow(
                                        column(12,
                                               plotlyOutput("sublevel_export_trend")
                                        )
                                      ),
                                      fluidRow(
                                        column(12,
                                               plotlyOutput("hs_subcategory_countries_export_sankey", height = 500)
                                        )
                                      ),
                                      fluidRow(
                                        column(12,
                                               plotlyOutput("continent_export_sankey", height = 500)
                                        )
                                      ),
                                      # fluidRow(
                                      #   column(12,
                                      #          plotOutput("export_treemap", height = 500)
                                      #   )
                                      # )
                             )
                           )
                       )
                )
              ),
              fluidRow(
                box(title = "Import and export comparison", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("import_export_comparison")
                ),
              ),
      ),
      
      
      # 设置 Tab
      tabItem(tabName = "tab_settings",
              fluidRow(
                box(title = "仪表盘说明", status = "primary", solidHeader = TRUE, width = 12,
                    h3("新西兰海外贸易仪表盘使用说明"),
                    p("本仪表盘提供了新西兰进出口贸易数据的可视化分析工具，通过多维度的图表展示贸易流向、结构和趋势。"),
                    
                    h4("1. 概览页面 (Overview)"),
                    tags$ul(
                      tags$li(strong("时间和显示粒度选择"), " - 可以选择查看全部时间、特定年份或自定义时间范围的数据，并可设置按年或按月显示。"),
                      tags$li(strong("进出口概览"), " - 显示进口额、出口额及贸易平衡的整体趋势。蓝色区域表示出口，红色区域表示进口，黑线表示贸易平衡。"),
                      tags$li(strong("进口概览"), " - 展示每月进口金额 (VFD和CIF) 以及运输保险成本的变化趋势。"),
                      tags$li(strong("出口概览"), " - 展示每月出口金额 (FOB) 的变化趋势，区分为国内产品出口和再出口。"),
                      tags$li(strong("地理分布"), " - 通过世界地图展示进出口的地理分布情况，颜色深浅表示贸易额大小。"),
                      tags$li(strong("前十国家贸易情况"), " - 展示与新西兰贸易额最大的十个国家的进口和出口趋势及占比变化。采用年份的聚合"),
                      tags$li(strong("前十商品贸易情况"), " - 展示新西兰进出口额最大的十类商品的贸易趋势及占比变化。采用年份的聚合"),
                      tags$li(strong("进出口同比环比分析"), " - 通过同比增长率和环比增长率展示进出口贸易的短期和长期变化趋势。"),
                      tags$li(strong("待做"), " - 一个拖拽的时间进度，查看地理的变化。")
                    ),
                    
                    h4("2. 数据分析页面 (数据分析)"),
                    tags$ul(
                      tags$li(strong("商品分类选择"), " - 可通过HS码分类层级（HS2分组、HS2码、HS4码、HS6码、详细HSCode）筛选特定商品。"),
                      tags$li(strong("当前选中HS码进出口走势"), " - 展示所选商品分类的进出口金额随时间的变化趋势。"),
                      tags$li(strong("当前HS码前十交易国家"), " - 展示与所选商品分类有关的前十大贸易国家。"),
                      tags$li(strong("下级分类趋势"), " - 分别展示所选商品分类下一级分类的进口和出口趋势，采用年份的聚合"),
                      tags$li(strong("商品与大洲贸易流向"), " - 通过桑基图(Sankey)展示不同商品类别与不同大洲之间的贸易流向和规模。"),
                      tags$li(strong("贸易结构树形图"), " - 通过矩形树图展示进出口贸易的分类结构，矩形面积表示贸易额大小。"),
                      tags$li(strong("进出口比较"), " - 直观对比不同商品类别的进出口情况，横轴上方为出口，下方为进口。"),
                      tags$li(strong("待做"), " - 自己输入的 hscode 的解析和选择。以及随着年份的增多是否需要调整坐标")
                    ),
                    
                    h4("数据指标说明"),
                    tags$ul(
                      tags$li(strong("VFD (Value for Duty)"), " - 海关完税价格，不包括运费和保险费。"),
                      tags$li(strong("CIF (Cost, Insurance, and Freight)"), " - 成本、保险费加运费价格，是进口商品的总成本。"),
                      tags$li(strong("FOB (Free On Board)"), " - 离岸价格，指商品装船后的价格，不包括运费和保险费。"),
                      tags$li(strong("运输保险成本"), " - CIF与VFD之间的差额，表示运输和保险的成本。"),
                      tags$li(strong("同比增长率"), " - 与去年同期相比的增长百分比。"),
                      tags$li(strong("环比增长率"), " - 与上一个月相比的增长百分比。")
                    ),
                    
                    h4("HS码分类系统"),
                    p("本仪表盘使用协调系统编码(HS Code)对商品进行分类："),
                    tags$ul(
                      tags$li(strong("HS2分组"), " - 协调系统的大类别分组。"),
                      tags$li(strong("HS2码"), " - 两位数编码，代表主要商品类别，如'01'表示活动物。"),
                      tags$li(strong("HS4码"), " - 四位数编码，代表更细分的商品类别。"),
                      tags$li(strong("HS6码"), " - 五位数编码，进一步细分商品类别。"),
                      tags$li(strong("详细HSCode"), " - 最详细的商品分类编码，可精确到特定商品类型。")
                    ),
                    
                    
                    h4("HS2分组对照表"),
                    p("以下是主要HS2分组及其代表的商品类别："),
                    div(style = "max-height: 400px; overflow-y: auto;",
                        DT::dataTableOutput("hs2_group_table")
                    ),
                )
              )
      )
    )
  )
)


# 服务器部分
server <- function(input, output, session) {
  create_overview_subtitle <- function() {
    # 收集当前筛选条件
    filters <- c()
    
    # 时间范围
    if (input$time_range == "specific_year") {
      filters <- c(filters, paste0(input$year_select_range, "year"))
    } else if (input$time_range == "custom_range") {
      filters <- c(filters, paste0(input$year_range[1], "to", 
                                   input$year_range[2]))
    } else {
      filters <- c(filters, "all_time")
    }
    
    # 时间粒度
    filters <- c(filters, ifelse(input$time_period == "month", "month", "year"))
    
    # 返回副标题
    return(paste(filters, collapse = ", "))
  }
  
  data_reactive <- reactive({
    # 根据时间范围进行数据筛选
    if (input$time_range == "all_time") {
      imports_data <- imports_raw
      exports_data <- exports_raw
    } else if (input$time_range == "specific_year") {
      req(input$year_select_range)
      imports_data <- filter(imports_raw, year == input$year_select_range)
      exports_data <- filter(exports_raw, year == input$year_select_range)
    } else if (input$time_range == "custom_range") {
      req(input$year_range[1])
      imports_data <- filter(imports_raw, year >= input$year_range[1] & year <= input$year_range[2])
      exports_data <- filter(exports_raw, year >= input$year_range[1] & year <= input$year_range[2])
    }

    # print(imports_data)
    req(input$time_period)
    if (input$time_period == "year") {
      imports_data <- imports_data %>% mutate(display_period = year)
      exports_data <- exports_data %>% mutate(display_period = year)
    } else if (input$time_period == "month") {
      imports_data <- imports_data %>% mutate(display_period = month)
      exports_data <- exports_data %>% mutate(display_period = month)
    }
    
    trade_balance_data <- imports_data %>%
      group_by(display_period) %>%
      summarise(Imports = sum(imports_nzd_cif, na.rm = TRUE)) %>%
      full_join(
        exports_data %>% 
          group_by(display_period) %>% 
          summarise(Exports = sum(total_exports_nzd_fob, na.rm = TRUE)),
        by = "display_period"
      ) %>%
      mutate(Balance = Exports - Imports)
    
    geo_imports <- imports_data %>% 
      group_by(iso3) %>%
      summarise(Value = sum(imports_nzd_vfd)) %>%
      mutate(Type = "Import")
    geo_exports <- exports_data %>%
        group_by(iso3) %>% 
        summarise(Value = sum(total_exports_nzd_fob, na.rm = T)) %>%
        mutate(Type = "Export")
    
    continent_import_sankey <- imports_data %>%
      group_by(hs2_group, continent) %>%
      summarise(Value = sum(imports_nzd_vfd, na.rm = T)) 
    
    continent_export_sankey <- exports_data %>%
      group_by(hs2_group, continent) %>%
      summarise(Value = sum(total_exports_nzd_fob, na.rm = T)) 
    
    
    list(
      imports_data = imports_data,
      exports_data = exports_data,
      trade_balance_data = trade_balance_data,
      geo_imports = geo_imports,
      geo_exports = geo_exports,
      continent_import_sankey = continent_import_sankey,
      continent_export_sankey = continent_export_sankey
    )
  })
  
 
  output$year_range_ui <- renderUI({
    if (input$time_range == "specific_year") {
      # 显示年份选择框
      selectInput("year_select_range", "select year:", choices = unique(imports_raw$year), selected = unique(imports_raw$year)[1])
    } else if (input$time_range == "custom_range") {
      choices = as.integer(unique(imports_raw$year))
      # 显示日期范围选择
      sliderInput("year_range", "Select a year range:",
                min = min(choices),
                max = max(choices),
                value = c(min(choices), max(choices)),
                step = 1,
                sep = "")
    } else {
      return(NULL)  # 全部时不显示任何选择
    }
  })
  
  output$time_period_ui <- renderUI({
    # 如果选择了"具体年份"，则只能按月显示
    if(input$time_range == "specific_year") {
      # 固定为"按月"且禁用选择
      selectInput("time_period", "Displays granularity:", 
                  choices = c("month"), 
                  selected = "month",)
    } else {
      # 其他情况下允许选择"按年"或"按月"
      selectInput("time_period", "Displays granularity:", 
                  choices = c("year", "month"), 
                  selected = "month")
    }
  })
  
  output$overview_plot <- renderPlotly({
    
    p <- data_reactive()$trade_balance_data %>% ggplot() +
      geom_line(aes(x=display_period, y=Imports/1e6, color="Imports", group=1), linewidth=1) + 
      geom_line(aes(x=display_period, y=Exports/1e6, color="Exports", group=1), linewidth=1) +
      geom_line(aes(x=display_period, y=Balance/1e6, color="Balance", group=1), linewidth=1.2) +
      geom_label(
        data = data_reactive()$trade_balance_data %>% 
          filter(Balance == max(Balance) | Balance == min(Balance)),
        aes(x=display_period, y=Balance/1e6, 
            label=paste(scales::dollar(Balance/1e6),"M")),
        color="#FFFFFF", fill="#2D3047"
      ) +
      scale_y_continuous(
        name = "Trade Value (Millions NZD)",
        sec.axis = sec_axis(~.*1, name="Trade Balance (Millions $NZD)")
      ) +
      scale_color_manual(values = c("Imports" = "#FF6B6B", "Exports" = "#4ECDC4", "Balance" = "#2D3047"),
                         name = "Type of trade") +
      labs(title = "Trade Flow Overview", x = NULL) + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
      
    
    ggplotly(p)
  })
  
  # 第二部分是地理，加入单独的时间进度条，同步变化, 
  output$geo_imports <- renderPlotly({
    title <- paste("Geographic Distribution of Imports - ",get_time_title())
    plot_geo(data_reactive()$geo_imports) %>%
      add_trace(
        z = ~Value,
        locations = ~iso3,
        colorscale = "Viridis"
      ) %>%
      layout(
        title = title
      )
  })
  
  output$geo_exports <- renderPlotly({
    title <- paste("Geographic Distribution of Exports - ",get_time_title())
    plot_geo(data_reactive()$geo_exports) %>%
      add_trace(
        z = ~Value,
        locations = ~iso3,
        colorscale = "Viridis"
      ) %>%
      layout(
        title = title
      )
  })
  
 
  
  # 第三部分是前十的国家，前十的商品，加入 hover 信息， 前十的商品
  # 准备前十进口国家数据
  output$top10_import_countries_value <- renderPlotly({
    # 获取前10名国家（按总额排名）
    top10_countries <- data_reactive()$imports_data %>%
      group_by(country) %>%
      summarise(Total = sum(imports_nzd_vfd, na.rm = T)) %>%
      arrange(desc(Total)) %>%
      slice_head(n = 10) %>%
      pull(country)
    
    # 按年整理这10个国家的数据
    top10_data <- data_reactive()$imports_data %>%
      filter(country %in% top10_countries) %>%
      group_by(display_period, country) %>%
      summarise(Value = sum(imports_nzd_vfd, na.rm = T)) %>%
      ungroup() %>%
      mutate(hover_text = paste0(
        'time: ', display_period,
        '<br>country: ', country, 
        "<br>Value: $", Value/1e6,"million"
      ))
    
    p1 <- ggplot(top10_data, aes(x = display_period, y = Value/1e6, color = country, 
                                 group = country, text=hover_text)) +
      geom_line(linewidth = 1) +
      labs(
        x = NULL, 
        y = "Value (million NZD)",
        color = "country",
        title = "Top Ten Importing Countries"
      ) +
      theme_minimal() +
      theme(legend.position = "none") 
    
    fig1 <- ggplotly(p1,tooltip = 'text')
    
    
    # 计算每年总进口额
    monthly_total <- data_reactive()$imports_data %>%
      group_by(display_period) %>%
      summarise(Total = sum(imports_nzd_vfd, na.rm = T))
    
    # 按月份整理这10个国家的数据并计算比例
    top10_data <- data_reactive()$imports_data %>%
      filter(country %in% top10_countries) %>%
      group_by(display_period, country) %>%
      summarise(Value = sum(imports_nzd_vfd, na.rm = T)) %>%
      ungroup() %>%
      left_join(monthly_total, by = "display_period") %>%
      mutate(Percentage = Value / Total * 100,
             hover_text = paste0(
               'time: ', display_period,
               '<br>country: ', country, 
               "<br>Percentage: ", Percentage
             ))
    
    p2 <- ggplot(top10_data, aes(x = display_period, y = Percentage, color = country, 
                                 group = country,text = hover_text)) +
      geom_line(linewidth = 1) +
      labs(
        x = NULL, 
        y = "Share of Total (%)",
        color = "Country"
      ) +
      theme_minimal()
    
    fig2 <- ggplotly(p2, tooltip = 'text')
    subplot(style(fig1, showlegend = F), fig2 ,nrows = 2, shareY = F, titleX = T, titleY=T, shareX = T) %>%
      layout(showlegend = TRUE,xaxis = list(tickangle = -30)) 
  })
  
  
  # 准备前十出口国家数据
  output$top10_export_countries_value <- renderPlotly({
    # 获取前10名国家（按总额排名）
    top10_countries <- data_reactive()$exports_data %>%
      group_by(country) %>%
      summarise(Total = sum(total_exports_nzd_fob, na.rm = TRUE)) %>%
      arrange(desc(Total)) %>%
      slice_head(n = 10) %>%
      pull(country)
    
    # 按月份整理这10个国家的数据
    top10_data <- data_reactive()$exports_data %>%
      filter(country %in% top10_countries) %>%
      group_by(display_period, country) %>%
      summarise(Value = sum(total_exports_nzd_fob, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(hover_text = paste0(
        'time: ', display_period,
        '<br>country: ', country, 
        "<br>Value: $", Value/1e6,"million"
      ))
    
    p1 <- ggplot(top10_data, aes(x = display_period, y = Value/1e6, color = country, 
                                 group = country, text = hover_text)) +
      geom_line(linewidth = 1) +
      labs(
        x = NULL, 
        y = "Value (million NZD)",
        color = "Country"
      ) +
      theme_minimal()
    
   fig1 <- ggplotly(p1, tooltip = 'text')
    
    # 计算每月总出口额
    monthly_total <- data_reactive()$exports_data %>%
      group_by(display_period) %>%
      summarise(Total = sum(total_exports_nzd_fob, na.rm = T))
    
    # 按月份整理这10个国家的数据并计算比例
    top10_data <- data_reactive()$exports_data %>%
      filter(country %in% top10_countries) %>%
      group_by(display_period, country) %>%
      summarise(Value = sum(total_exports_nzd_fob, na.rm = T)) %>%
      ungroup() %>%
      left_join(monthly_total, by = "display_period") %>%
      mutate(Percentage = Value / Total * 100,
             hover_text = paste0(
               'time: ', display_period,
               '<br>country: ', country, 
               "<br>Percentage: ", Percentage
             ))
    
    p2 <- ggplot(top10_data, aes(x = display_period, y = Percentage, color = country, 
                                 group = country,text = hover_text)) +
      geom_line(linewidth = 1) +
      labs(
        x = NULL, 
        y = "Share of Total (%)",
        color = "Country",
        title = "Top Ten Exporting Countries"
      ) +
      theme_minimal()
    
    fig2 <- ggplotly(p2, tooltip = 'text')
    subplot(style(fig1, showlegend = F), fig2 ,nrows = 2, shareY = F, titleX = T, titleY=T, shareX = T) %>%
      layout(showlegend = TRUE,xaxis = list(tickangle = -30)) 
  })
  
  # 前十进出口商品
  # 前十进口商品
  output$top10_import_commodities_combined <- renderPlotly({
    if(input$time_period == "month") {
      # 返回一个提示信息的空白图表
      return(plot_ly() %>% 
               layout(title = "Top Ten Import Commodities",
                      annotations = list(
                        x = 0.5,
                        y = 0.5,
                        text = "For individual products, monthly aggregation is not suitable because some products may only be used in certain months. Please adjust the granularity to yearly.",
                        showarrow = FALSE,
                        font = list(size = 14)
                      ),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
    }
      
    # 获取前10名商品（按总额排名）- 使用VFD
    top10_commodities <- data_reactive()$imports_data %>%
      group_by(harmonised_system_code) %>%
      summarise(Total = sum(imports_nzd_vfd, na.rm = T)) %>%
      arrange(desc(Total)) %>%
      slice_head(n = 10) %>%
      pull(harmonised_system_code)
    
    top10_descriptions <- hs_map %>%
      filter(HS_codes %in% top10_commodities) %>%
      select(HS_codes, HS_description)
    
    # 按月份整理这10个商品的数据 - 金额 (VFD)
    top10_data_value <- data_reactive()$imports_data %>%
      filter(harmonised_system_code %in% top10_commodities) %>%
      group_by(year, harmonised_system_code) %>%
      summarise(Value = sum(imports_nzd_vfd, na.rm = T)) %>%
      ungroup() %>%
      # Join with descriptions
      left_join(top10_descriptions, by = c("harmonised_system_code" = "HS_codes")) %>%
      # Create hover text with description
      mutate(hover_text = paste0(
        'Year: ', year,
        '<br>HS Code: ', harmonised_system_code, 
        '<br>Description: ', truncate_text(HS_description),
        '<br>Value: $', round(Value/1e6, 2), " million"
      ))
    
    # 计算每月总进口额 (VFD)
    monthly_total <- data_reactive()$imports_data %>%
      group_by(year) %>%
      summarise(Total = sum(imports_nzd_vfd, na.rm = TRUE))
    
    # 按月份整理这10个商品的数据 - 比例
    top10_data_percent <- data_reactive()$imports_data %>%
      filter(harmonised_system_code %in% top10_commodities) %>%
      group_by(year, harmonised_system_code) %>%
      summarise(Value = sum(imports_nzd_vfd, na.rm = TRUE)) %>%
      ungroup() %>%
      left_join(monthly_total, by = "year") %>%
      mutate(Percentage = Value / Total * 100) %>%
      # Join with descriptions
      left_join(top10_descriptions, by = c("harmonised_system_code" = "HS_codes")) %>%
      # Create hover text with description
      mutate(hover_text = paste0(
        'Year: ', year,
        '<br>HS Code: ', harmonised_system_code, 
        '<br>Description: ',  truncate_text(HS_description),
        '<br>Percentage: ', round(Percentage, 2), '%'
      ))
    
    
    # 创建金额图
    p1 <- ggplot(top10_data_value, aes(x = year, y = Value/1e6, color = harmonised_system_code, 
                                       group = harmonised_system_code, text = hover_text)) +
      geom_line(linewidth = 1) +
      labs(
        x = NULL, 
        y = "Value (million NZD)",
        title = "Top Ten Import Commodities"
      ) +
      theme_minimal() +
      theme(legend.position = "none")  # 移除图例
    
    # 创建比例图
    p2 <- ggplot(top10_data_percent, aes(x = year, y = Percentage, color = harmonised_system_code, 
                                         group = harmonised_system_code, text = hover_text)) +
      geom_line(linewidth = 1) +
      labs(
        x = NULL, 
        y = "Share of Total (%)"
      ) +
      theme_minimal() 
    
    # 转换为plotly对象
    fig1 <- ggplotly(p1, tooltip = 'text') 
    fig2 <- ggplotly(p2, tooltip = 'text')
    
    # 创建子图并排列，使用共享的X轴
    subplot(style(fig1, showlegend = F), fig2 ,nrows = 2, shareY = F, titleX = T, titleY=T, shareX = T) %>%
      layout(showlegend = TRUE,xaxis = list(tickangle = -30))
  })
  
  output$top10_export_commodities_combined <- renderPlotly({
    if(input$time_period == "month") {
      # 返回一个提示信息的空白图表
      return(plot_ly() %>% 
               layout(title = "Top Ten Export Commodities",
                      annotations = list(
                        x = 0.5,
                        y = 0.5,
                        text = "For individual products, monthly aggregation is not suitable because some products may only be used in certain months. Please adjust the granularity to yearly.",
                        showarrow = FALSE,
                        font = list(size = 14)
                      ),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
    }
    
    # 获取前10名商品（按总额排名）- 使用VFD
    top10_commodities <- data_reactive()$exports_data %>%
      group_by(harmonised_system_code) %>%
      summarise(Total = sum(total_exports_nzd_fob, na.rm = T)) %>%
      arrange(desc(Total)) %>%
      slice_head(n = 10) %>%
      pull(harmonised_system_code)
    top10_descriptions <- hs_map %>%
      filter(HS_codes %in% top10_commodities) %>%
      select(HS_codes, HS_description)
    
    # 按月份整理这10个商品的数据 - 金额 (VFD)
    top10_data_value <- data_reactive()$exports_data %>%
      filter(harmonised_system_code %in% top10_commodities) %>%
      group_by(year, harmonised_system_code) %>%
      summarise(Value = sum(total_exports_nzd_fob, na.rm = T)) %>%
      ungroup() %>%
      # Join with descriptions
      left_join(top10_descriptions, by = c("harmonised_system_code" = "HS_codes")) %>%
      # Create hover text with description
      mutate(hover_text = paste0(
        'Year: ', year,
        '<br>HS Code: ', harmonised_system_code, 
        '<br>Description: ', truncate_text(HS_description),
        '<br>Value: $', round(Value/1e6, 2), " million"
      ))
    
    # 计算每月总进口额 (VFD)
    monthly_total <- data_reactive()$exports_data %>%
      group_by(year) %>%
      summarise(Total = sum(total_exports_nzd_fob, na.rm = TRUE))
    
    # 按月份整理这10个商品的数据 - 比例
    top10_data_percent <- data_reactive()$exports_data %>%
      filter(harmonised_system_code %in% top10_commodities) %>%
      group_by(year, harmonised_system_code) %>%
      summarise(Value = sum(total_exports_nzd_fob, na.rm = TRUE)) %>%
      ungroup() %>%
      left_join(monthly_total, by = "year") %>%
      mutate(Percentage = Value / Total * 100) %>%
      # Join with descriptions
      left_join(top10_descriptions, by = c("harmonised_system_code" = "HS_codes")) %>%
      # Create hover text with description
      mutate(hover_text = paste0(
        'Year: ', year,
        '<br>HS Code: ', harmonised_system_code, 
        '<br>Description: ', truncate_text(HS_description),
        '<br>Percentage: ', round(Percentage, 2), '%'
      ))
      
    # 创建金额图
    p1 <- ggplot(top10_data_value, aes(x = year, y = Value/1e6, color = harmonised_system_code, 
                                       group = harmonised_system_code, text = hover_text)) +
      geom_line(linewidth = 1) +
      labs(
        x = NULL, 
        y = "Value (million NZD)",
        title = "Top Ten Export Commodities"
      ) +
      theme_minimal() +
      theme(legend.position = "none")  # 移除图例
    
    # 创建比例图
    p2 <- ggplot(top10_data_percent, aes(x = year, y = Percentage, color = harmonised_system_code, 
                                         group = harmonised_system_code, text = hover_text)) +
      geom_line(linewidth = 1) +
      labs(
        x = NULL, 
        y = "Share of Total (%)"
      ) +
      theme_minimal() 
    
    # 转换为plotly对象
    fig1 <- ggplotly(p1, tooltip = "text") 
    fig2 <- ggplotly(p2, tooltip = "text")
    
    # 创建子图并排列，使用共享的X轴
    subplot(style(fig1, showlegend = F), fig2 ,nrows = 2, shareY = F, titleX = T, titleY=T, shareX = T) %>%
      layout(showlegend = TRUE,xaxis = list(tickangle = -30))
  })
  
  # 第四部分是同比，环比，加入 hover 信息
  # 计算进口同比增长率
  output$import_yoy_growth <- renderPlotly({
    
    if(input$time_range == "specific_year") {
      # 返回一个提示信息的空白图表
      return(plot_ly() %>% 
               layout(title = "选择单一年份时无法计算同比增长率",
                      annotations = list(
                        x = 0.5,
                        y = 0.5,
                        text = "同比增长率需要至少两年的数据进行对比，请选择'全部时间'或'自定义范围'",
                        showarrow = FALSE,
                        font = list(size = 14)
                      ),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
    }
    
    # 准备计算同比数据
    yoy_data <- data_reactive()$imports_data %>%
      # 按年月汇总
      mutate(year = year(month), month_num = month(month)) %>%
      group_by(year, month_num) %>%
      summarise(
        total_value = sum(imports_nzd_vfd, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      # 创建年月标识符用于排序和匹配
      mutate(year_month = paste(year, sprintf("%02d", month_num), sep = "-")) %>%
      arrange(year, month_num)
    
    # 计算同比增长率
    yoy_growth <- yoy_data %>%
      group_by(month_num) %>%
      arrange(year) %>%
      mutate(
        prev_year_value = lag(total_value, 1),
        yoy_growth_rate = (total_value - prev_year_value) / prev_year_value * 100
      ) %>%
      ungroup() %>%
      filter(!is.na(yoy_growth_rate)) %>%
      # 重新创建完整日期用于绘图
      mutate(date = as.Date(paste(year_month, "01", sep = "-")))
    
    # 绘制图表
    p <- ggplot(yoy_growth, aes(x = date, y = yoy_growth_rate)) +
      geom_line(color = "#1E88E5", linewidth = 1) +
      geom_point(color = "#1E88E5", size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_text(
        data = subset(yoy_growth, abs(yoy_growth_rate) > 20 | yoy_growth_rate == max(yoy_growth_rate) | yoy_growth_rate == min(yoy_growth_rate)),
        aes(label = sprintf("%.1f%%", yoy_growth_rate)),
        vjust = -0.5, hjust = 0.5, size = 3
      ) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(
        x = NULL,
        y = "Year-over-year growth rate (%)",
        title = "Year-on-Year Growth Rate of Imports (%)"
      ) +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
    ggplotly(p)
  })
  
  # 计算进口环比增长率
  output$import_mom_growth <- renderPlotly({
    # 准备月度数据
    mom_data <- data_reactive()$imports_data %>%
      group_by(month) %>%
      summarise(
        total_value = sum(imports_nzd_vfd, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      arrange(month)
    
    # 计算环比增长率
    mom_growth <- mom_data %>%
      mutate(
        prev_month_value = lag(total_value, 1),
        mom_growth_rate = (total_value - prev_month_value) / prev_month_value * 100
      ) %>%
      filter(!is.na(mom_growth_rate))
    
    # 绘制图表
    p <- ggplot(mom_growth, aes(x = month, y = mom_growth_rate)) +
      geom_line(color = "#FFA000", linewidth = 1) +
      geom_point(color = "#FFA000", size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_text(
        data = subset(mom_growth, abs(mom_growth_rate) > 10 | mom_growth_rate == max(mom_growth_rate) | mom_growth_rate == min(mom_growth_rate)),
        aes(label = sprintf("%.1f%%", mom_growth_rate)),
        vjust = -0.5, hjust = 0.5, size = 3
      ) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(
        x = NULL,
        y = "Sequential Growth Rate (%)",
        title = "Month-on-Month Growth Rate of Imports (%)"
      ) +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
    ggplotly(p)
  })
  
  # 计算出口同比增长率
  output$export_yoy_growth <- renderPlotly({
    
    if(input$time_range == "specific_year") {
      # 返回一个提示信息的空白图表
      return(plot_ly() %>% 
               layout(title = "",
                      annotations = list(
                        x = 0.5,
                        y = 0.5,
                        text = "The year-over-year growth rate needs to be compared with at least two years of data, please select 'All Time' or 'Custom Range'",
                        showarrow = FALSE,
                        font = list(size = 14)
                      ),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
    }
    
    # 准备计算同比数据
    yoy_data <- data_reactive()$exports_data %>%
      # 按年月汇总
      mutate(year = year(month), month_num = month(month)) %>%
      group_by(year, month_num) %>%
      summarise(
        total_value = sum(total_exports_nzd_fob, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      # 创建年月标识符用于排序和匹配
      mutate(year_month = paste(year, sprintf("%02d", month_num), sep = "-")) %>%
      arrange(year, month_num)
    
    # 计算同比增长率
    yoy_growth <- yoy_data %>%
      group_by(month_num) %>%
      arrange(year) %>%
      mutate(
        prev_year_value = lag(total_value, 1),
        yoy_growth_rate = (total_value - prev_year_value) / prev_year_value * 100
      ) %>%
      ungroup() %>%
      filter(!is.na(yoy_growth_rate)) %>%
      # 重新创建完整日期用于绘图
      mutate(date = as.Date(paste(year_month, "01", sep = "-")))
    
    # 绘制图表
    p <- ggplot(yoy_growth, aes(x = date, y = yoy_growth_rate)) +
      geom_line(color = "#43A047", linewidth = 1) +
      geom_point(color = "#43A047", size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_text(
        data = subset(yoy_growth, abs(yoy_growth_rate) > 20 | yoy_growth_rate == max(yoy_growth_rate) | yoy_growth_rate == min(yoy_growth_rate)),
        aes(label = sprintf("%.1f%%", yoy_growth_rate)),
        vjust = -0.5, hjust = 0.5, size = 3
      ) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(
        x = NULL,
        y = "Year-over-year growth rate (%)",
        title = "Year-on-Year Growth Rate of Exports (%)"
      ) +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
    ggplotly(p)
  })
  
  # 计算出口环比增长率
  output$export_mom_growth <- renderPlotly({
    # 准备月度数据
    mom_data <- data_reactive()$exports_data %>%
      group_by(month) %>%
      summarise(
        total_value = sum(total_exports_nzd_fob, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      arrange(month)
    
    # 计算环比增长率
    mom_growth <- mom_data %>%
      mutate(
        prev_month_value = lag(total_value, 1),
        mom_growth_rate = (total_value - prev_month_value) / prev_month_value * 100
      ) %>%
      filter(!is.na(mom_growth_rate))
    
    # 绘制图表
    p <- ggplot(mom_growth, aes(x = month, y = mom_growth_rate)) +
      geom_line(color = "#D81B60", linewidth = 1) +
      geom_point(color = "#D81B60", size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_text(
        data = subset(mom_growth, abs(mom_growth_rate) > 10 | mom_growth_rate == max(mom_growth_rate) | mom_growth_rate == min(mom_growth_rate)),
        aes(label = sprintf("%.1f%%", mom_growth_rate)),
        vjust = -0.5, hjust = 0.5, size = 3
      ) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(
        x = NULL,
        y = "Sequential Growth Rate (%)",
        title = "Month-on-Month Growth Rate of Exports (%)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
    ggplotly(p)
  })
  # 第二个 tab 是商品分析，先整体的树形图可以选择hscode，按照0 2 4 5，按照时间分布的占进口的比例，有 sankey plot，地图展示, 前十国家的进出口和比例，
  # sankey plot
  unique_hs2code <- sort(unique(c(imports_raw$hs2_group, exports_raw$hs2_group)))
  unique_continent <- sort(unique(c(imports_raw$continent, exports_raw$continent)))

  # 准备进口桑基图数据
  output$continent_import_sankey <- renderPlotly({
    sankey_data <- data_reactive()$continent_import_sankey
    sankey_data <- sankey_data %>%
      mutate(hs2_group = as.character(hs2_group))
    
    # 确保hs2_map中的HS_codes也是字符串
    hs2_map <- hs2_map %>%
      mutate(HS_codes = as.character(HS_codes))
    sankey_data <- sankey_data %>%
      left_join(hs2_map, by = c("hs2_group" = "HS_codes")) %>%
      mutate(
        hover_text = paste0(
          ifelse(!is.na(HS_description), paste0(truncate_text(HS_description)), "")
        )
      )
    sankey_data %>%
      plot_ly(
        type = "sankey",
        orientation = "h",
        node = list(
          label = c(unique_hs2code, unique_continent)
        ),
        link = list(
          label = .$hover_text,
          source = as.numeric(factor(.$continent,level = unique_continent)) - 1 + length(unique_hs2code),
          target = as.numeric(factor(.$hs2_group,level = unique_hs2code)) - 1,
          value = .$Value
        )
      )%>%
        layout(title = "Imports Flow: HS Group to Continent")
  })

  
  # 准备出口桑基图数据
  output$continent_export_sankey <- renderPlotly({
    sankey_data <- data_reactive()$continent_export_sankey
    sankey_data <- sankey_data %>%
      mutate(hs2_group = as.character(hs2_group))
    
    # 确保hs2_map中的HS_codes也是字符串
    hs2_map <- hs2_map %>%
      mutate(HS_codes = as.character(HS_codes))
    sankey_data <- sankey_data %>%
      left_join(hs2_map, by = c("hs2_group" = "HS_codes")) %>%
      mutate(
        hover_text = paste0(
          ifelse(!is.na(HS_description), paste0(truncate_text(HS_description)), "")
        )
      )
    sankey_data %>%
      plot_ly(
        type = "sankey",
        orientation = "h",
        node = list(
          label = c(unique_hs2code, unique_continent)
        ),
        link = list(
          source = as.numeric(factor(.$hs2_group,level = unique_hs2code)) - 1,
          target = as.numeric(factor(.$continent,level = unique_continent)) - 1 + length(unique_hs2code),
          value = .$Value,
          label = .$hover_text
        )
      )%>%
      layout(title = "Exports Flow: HS Group to Continent")
  })
  
  # # 第二部分是树形框，展示比例，加入 hover 信息，
  # output$export_treemap <- renderPlot({
  #   data_reactive()$exports_data %>%
  #     group_by(hs2_group, hs2) %>%
  #     summarise(total = sum(total_exports_nzd_fob)) %>%
  #     ggplot(aes(area = total, fill = hs2_group,
  #              label = hs2, subgroup = hs2_group)) +
  #     geom_treemap() +
  #     geom_treemap_subgroup_border() +
  #     geom_treemap_text(colour = "white") +
  #     scale_fill_viridis_d()
  # })
  # 
  # output$import_treemap <- renderPlot({
  #   data_reactive()$imports_data %>%
  #     group_by(hs2_group, hs2) %>%
  #     summarise(total = sum(imports_nzd_vfd)) %>%
  #     ggplot(aes(area = total, fill = hs2_group,
  #                label = hs2, subgroup = hs2_group)) +
  #     geom_treemap() +
  #     geom_treemap_subgroup_border() +
  #     geom_treemap_text(colour = "white") +
  #     scale_fill_viridis_d()
  # })
  # # TODO: 加入 hover，加入多层级 
  
  # 进出口比较
  
  output$import_export_comparison <- renderPlotly({
    hs_comparison <- bind_rows(
      data_reactive()$imports_data %>% 
        group_by(hs2_group) %>%
        summarise(Value = sum(imports_nzd_cif)) %>%
        mutate(Type = "Import"),
      data_reactive()$exports_data  %>% 
        group_by(hs2_group) %>%
        summarise(Value = sum(total_exports_nzd_fob)) %>%
        mutate(Type = "Export")
    ) 
    p1 <- ggplot(hs_comparison) +
      geom_bar(
        data = filter(hs_comparison, Type=="Import"),
        aes(x=hs2_group, y=-Value/1e9, fill=Type), 
        stat="identity",
      ) +
      geom_bar(
        data = filter(hs_comparison, Type=="Export"),
        aes(x=hs2_group, y=Value/1e9, fill = Type), 
        stat="identity",
      ) +
      geom_hline(yintercept=0, color="#2D3047") +
      coord_flip() +
      scale_y_continuous(
        breaks = seq(-200, 300, 10),
        labels = function(x) abs(x)
      ) +
      labs(title = "HS Code Trade Structure Comparison cif vs fob (billion)",
           y = "Value (billion NZD)",
           x = 'HS2 Group',)
    ggplotly(p1)
  })

  # TODO: x-axis 适应不同大小的数据
  
  # 选择层级后，每年的钱，增长比例，占比，前十的贡献城市排名
  
  # 初始化HS2分组选择框
  observe({
    # 合并进出口数据的HS2分组，获取所有可能值
    hs2_groups <- sort(unique(c(
      unique(data_reactive()$imports_data$hs2_group),
      unique(data_reactive()$exports_data$hs2_group)
    )))
    # 过滤掉NA值
    hs2_groups <- hs2_groups[!is.na(hs2_groups)]
    
    matched_codes <- hs2_map %>%
      filter(HS_codes %in% hs2_groups)
    hs2_choices <- setNames(matched_codes$HS_codes, matched_codes$HS_description)
    
    # 添加"全部"选项
    hs2_groups <- c("ALL", hs2_choices)
    
    # 更新选择框
    updateSelectInput(session, "hs2_group_select", 
                      choices = hs2_groups,
                      selected = "ALL")
  })
  
  # 当HS2分组改变时，更新HS2选择框
  observeEvent(input$hs2_group_select, {
    # 如果选择了"全部"，则显示所有HS2码
    if(input$hs2_group_select == "ALL") {
      hs2_codes <- sort(unique(c(
        unique(data_reactive()$imports_data$hs2),
        unique(data_reactive()$exports_data$hs2)
      )))
    } else {
      # 筛选当前分组下的HS2码
      hs2_codes <- sort(unique(c(
        data_reactive()$imports_data %>% 
          filter(hs2_group == input$hs2_group_select) %>% 
          pull(hs2),
        data_reactive()$exports_data %>% 
          filter(hs2_group == input$hs2_group_select) %>% 
          pull(hs2)
      )))
    }
    
    # 过滤掉NA值
    hs2_codes <- hs2_codes[!is.na(hs2_codes)]
    matched_codes <- hs_map %>%
      filter(HS_codes %in% hs2_codes)
    
    hs2_choices <- setNames(matched_codes$HS_codes, matched_codes$HS_description)
    # 添加"全部"选项
    hs2_codes <- c("ALL", hs2_choices)
    
    # 更新选择框
    updateSelectInput(session, "hs2_select", 
                      choices = hs2_codes,
                      selected = "ALL")
  })
  
  # 当HS2码改变时，更新HS4选择框
  observeEvent(input$hs2_select, {
    # 如果选择了"全部"并且HS2分组也是"全部"，则显示所有HS4码
    if(input$hs2_select == "ALL" && input$hs2_group_select == "ALL") {
      hs4_codes <- sort(unique(c(
        unique(data_reactive()$imports_data$hs4),
        unique(data_reactive()$exports_data$hs4)
      )))
    } else if(input$hs2_select == "ALL") {
      # 如果只有HS2是"全部"，但HS2分组有选择，则筛选该分组下的所有HS4
      hs4_codes <- sort(unique(c(
        data_reactive()$imports_data %>% 
          filter(hs2_group == input$hs2_group_select) %>% 
          pull(hs4),
        data_reactive()$exports_data %>% 
          filter(hs2_group == input$hs2_group_select) %>% 
          pull(hs4)
      )))
    } else {
      # 筛选当前HS2码下的HS4码
      hs4_codes <- sort(unique(c(
        data_reactive()$imports_data %>% 
          filter(hs2 == input$hs2_select) %>% 
          pull(hs4),
        data_reactive()$exports_data %>% 
          filter(hs2 == input$hs2_select) %>% 
          pull(hs4)
      )))
    }
    
    # 过滤掉NA值
    hs4_codes <- hs4_codes[!is.na(hs4_codes)]
    
    matched_codes <- hs_map %>%
      filter(HS_codes %in% hs4_codes)
    hs4_choices <- setNames(matched_codes$HS_codes, matched_codes$HS_description)
    
    # 添加"全部"选项
    hs4_codes <- c("ALL", hs4_choices)
    
    # 更新选择框
    updateSelectInput(session, "hs4_select", 
                      choices = hs4_codes,
                      selected = "ALL")
  })
  
  # 当HS4码改变时，更新HS6选择框
  observeEvent(input$hs4_select, {
    # 根据当前的选择链构建筛选条件
    imports_filtered <- data_reactive()$imports_data
    exports_filtered <- data_reactive()$exports_data
    
    # 应用HS2分组筛选
    if(input$hs2_group_select != "ALL") {
      imports_filtered <- imports_filtered %>% filter(hs2_group == input$hs2_group_select)
      exports_filtered <- exports_filtered %>% filter(hs2_group == input$hs2_group_select)
    }
    
    # 应用HS2筛选
    if(input$hs2_select != "ALL") {
      imports_filtered <- imports_filtered %>% filter(hs2 == input$hs2_select)
      exports_filtered <- exports_filtered %>% filter(hs2 == input$hs2_select)
    }
    
    # 应用HS4筛选
    if(input$hs4_select != "ALL") {
      hs6_codes <- sort(unique(c(
        imports_filtered %>% filter(hs4 == input$hs4_select) %>% pull(hs6),
        exports_filtered %>% filter(hs4 == input$hs4_select) %>% pull(hs6)
      )))
    } else {
      # 如果HS4是"全部"，则显示所有符合前面筛选条件的HS6
      hs6_codes <- sort(unique(c(
        imports_filtered %>% pull(hs6),
        exports_filtered %>% pull(hs6)
      )))
    }
    
    if(input$hs6_select != "ALL") {
      hscodes <- sort(unique(c(
        imports_filtered %>% filter(hs6 == input$hs6_select) %>% pull(hs6),
        exports_filtered %>% filter(hs6 == input$hs6_select) %>% pull(hs6)
      )))
    } else {
      # 如果HS6是"全部"，则显示所有符合前面筛选条件的HSCode
      hscodes <- sort(unique(c(
        imports_filtered %>% pull(hs6),
        exports_filtered %>% pull(hs6)
      )))
    }
    
    # 过滤掉NA值
    hs6_codes <- hs6_codes[!is.na(hs6_codes)]
    
    # 创建一个包含所有HS6代码的数据框
    all_codes_df <- data.frame(HS_codes = hs6_codes)
    
    # 与hs_map合并，保留所有hs6_codes中的代码
    matched_codes <- all_codes_df %>%
      left_join(hs_map, by = "HS_codes")
    
    # 对于没有匹配到描述的代码，使用原始代码作为描述
    matched_codes <- matched_codes %>%
      mutate(HS_description = ifelse(is.na(HS_description), 
                                     HS_codes, 
                                     HS_description))
    
    # 创建选择列表
    hs6_choices <- setNames(matched_codes$HS_codes, matched_codes$HS_description)
    # 添加"全部"选项
    hs6_codes <- c("ALL", hs6_choices)
    
    # 更新选择框
    updateSelectInput(session, "hs6_select", 
                      choices = hs6_codes,
                      selected = "ALL")
  })
  
  # 当HS6码改变时，更新HSCode选择框
  observeEvent(input$hs6_select, {
    # 根据当前的选择链构建筛选条件
    imports_filtered <- data_reactive()$imports_data
    exports_filtered <- data_reactive()$exports_data
    
    # 应用HS2分组筛选
    if(input$hs2_group_select != "ALL") {
      imports_filtered <- imports_filtered %>% filter(hs2_group == input$hs2_group_select)
      exports_filtered <- exports_filtered %>% filter(hs2_group == input$hs2_group_select)
    }
    
    # 应用HS2筛选
    if(input$hs2_select != "ALL") {
      imports_filtered <- imports_filtered %>% filter(hs2 == input$hs2_select)
      exports_filtered <- exports_filtered %>% filter(hs2 == input$hs2_select)
    }
    
    # 应用HS4筛选
    if(input$hs4_select != "ALL") {
      imports_filtered <- imports_filtered %>% filter(hs4 == input$hs4_select)
      exports_filtered <- exports_filtered %>% filter(hs4 == input$hs4_select)
    }
    
    # 应用HS6筛选
    if(input$hs6_select != "ALL") {
      imports_filtered <- imports_filtered %>% filter(hs6 == input$hs6_select)
      exports_filtered <- exports_filtered %>% filter(hs6 == input$hs6_select)
     
    }
    
    if(input$hscode_select != "ALL") {
      hscodes <- sort(unique(c(
        imports_filtered %>% filter(harmonised_system_code == input$hscode_select) %>% pull(harmonised_system_code),
        exports_filtered %>% filter(harmonised_system_code == input$hscode_select) %>% pull(harmonised_system_code)
      )))
    } else {
      # 如果HS6是"全部"，则显示所有符合前面筛选条件的HSCode
      hscodes <- sort(unique(c(
        imports_filtered %>% pull(harmonised_system_code),
        exports_filtered %>% pull(harmonised_system_code)
      )))
    }
    # print(hscodes)
    # 过滤掉NA值
    hscodes <- hscodes[!is.na(hscodes)]
    matched_codes <- hs_map %>%
      filter(HS_codes %in% hscodes)
    
    hs_choices <- setNames(matched_codes$HS_codes, matched_codes$HS_description)
    
    # 添加"全部"选项
    hscodes <- c("ALL", hs_choices)
    
    # 更新选择框
    updateSelectInput(session, "hscode_select", 
                      choices = hscodes,
                      selected = "ALL")
  })
  
  # 当国家选择变化时的响应
  observeEvent(input$country_select, {
    # 更新已筛选的数据
    filtered_data <- reactive({
      # 首先获取基于HS码筛选的数据
      hs_filtered <- build_hs_filter(data_reactive()$imports_data, data_reactive()$exports_data)
      
      # 如果选择了特定国家（不是"ALL"）
      if(input$country_select != "ALL") {
        # 进一步按国家筛选进口数据
        imports_filtered <- hs_filtered$imports %>% 
          filter(country == input$country_select)
        
        # 进一步按国家筛选出口数据
        exports_filtered <- hs_filtered$exports %>% 
          filter(country == input$country_select)
        
        return(list(imports = imports_filtered, exports = exports_filtered))
      } else {
        # 如果选择了"ALL"，则返回仅按HS码筛选的数据
        return(hs_filtered)
      }
    })
    
  })
  
  # 创建一个函数来构建筛选条件
  build_hs_filter <- function(imports_data, exports_data) {
    # 初始化筛选条件
    imports_filtered <- imports_data
    exports_filtered <- exports_data
    
    # 根据选择的HS2分组筛选
    if(input$hs2_group_select != "ALL") {
      imports_filtered <- imports_filtered %>% filter(hs2_group == input$hs2_group_select)
      exports_filtered <- exports_filtered %>% filter(hs2_group == input$hs2_group_select)
    }
    
    # 根据选择的HS2码筛选
    if(input$hs2_select != "ALL") {
      imports_filtered <- imports_filtered %>% filter(hs2 == input$hs2_select)
      exports_filtered <- exports_filtered %>% filter(hs2 == input$hs2_select)
    }
    
    # 根据选择的HS4码筛选
    if(input$hs4_select != "ALL") {
      imports_filtered <- imports_filtered %>% filter(hs4 == input$hs4_select)
      exports_filtered <- exports_filtered %>% filter(hs4 == input$hs4_select)
    }
    
    # 根据选择的HS6码筛选
    if(input$hs6_select != "ALL") {
      imports_filtered <- imports_filtered %>% filter(hs6 == input$hs6_select)
      exports_filtered <- exports_filtered %>% filter(hs6 == input$hs6_select)
    }
    
    # 根据选择的HSCode筛选
    if(input$hscode_select != "ALL") {
      imports_filtered <- imports_filtered %>% filter(harmonised_system_code == input$hscode_select)
      exports_filtered <- exports_filtered %>% filter(harmonised_system_code == input$hscode_select)
    }
    
    if(input$country_select != "ALL") {
      imports_filtered <- imports_filtered %>% filter(country == input$country_select)
      exports_filtered <- exports_filtered %>% filter(country == input$country_select)
    }
    
    return(list(imports = imports_filtered, exports = exports_filtered))
  }
  
  filtered_hs_reactive <- reactive({
    # 调用build_hs_filter函数获取过滤后的数据
    build_hs_filter(data_reactive()$imports_data, data_reactive()$exports_data)
  })
  
  observe({
    # Get all unique continents from imports and exports data
    countrys <- sort(unique(c(
      unique(data_reactive()$imports_data$country),
      unique(data_reactive()$exports_data$country)
    )))
    
    # Filter out NA values
    countrys <- countrys[!is.na(countrys)]
    
    # Add "ALL" option
    countrys <- c("ALL", countrys)
    
    # Update select input
    updateSelectInput(session, "country_select", 
                      choices = countrys,
                      selected = "ALL")
  })
  
  get_analysis_title <- function(){
    title_parts <- c()
    if(input$hs2_group_select != "ALL") {
      description <- hs2_map %>%
        filter(HS_codes == input$hs2_group_select) %>%
        pull(HS_description)
      # 如果找不到描述，则使用原始值
      if(length(description) == 0 || is.na(description)) description <- input$hs2_group_select
      
      title_parts <- c(paste0("HS2 group: ", description))
    }
    
    if(input$hs2_select != "ALL") {
      description <- hs_map %>%
        filter(HS_codes == input$hs2_select) %>%
        pull(HS_description)
      # 如果找不到描述，则使用原始值
      if(length(description) == 0 || is.na(description)) description <- input$hs2_select
      
      title_parts <- c(paste0("HS2: ", description))
    }
    
    if(input$hs4_select != "ALL") {
      description <- hs_map %>%
        filter(HS_codes == input$hs4_select) %>%
        pull(HS_description)
      # 如果找不到描述，则使用原始值
      if(length(description) == 0 || is.na(description)) description <- input$hs4_select
      
      title_parts <- c(paste0("HS4: ", description))
    }
    
    if(input$hs6_select != "ALL") {
      description <- hs_map %>%
        filter(HS_codes == input$hs6_select) %>%
        pull(HS_description)
      # 如果找不到描述，则使用原始值
      if(length(description) == 0 || is.na(description)) description <- input$hs6_select
      
      title_parts <- c(paste0("HS6: ", description))
    }
    
    if(input$hscode_select != "ALL") {
      description <- hs_map %>%
        filter(HS_codes == input$hscode_select) %>%
        pull(HS_description)
      # 如果找不到描述，则使用原始值
      if(length(description) == 0 || is.na(description)) description <- input$hscode_select
      
      title_parts <- c(paste0("HSCode: ", description))
    }
    
    if(input$country_select != "ALL") title_parts <- c(title_parts, paste0("Country: ", input$country_select))
    
    title <- if(length(title_parts) > 0) paste(title_parts, collapse = ", ") else "All merchandise"
    
    return(title)
  }
  
  output$current_hs_trend <- renderPlotly({
    # 获取筛选后的数据
    filtered_data <- build_hs_filter(data_reactive()$imports_data, data_reactive()$exports_data)

    title <- get_analysis_title()
    # 按月份汇总进出口数据
    import_data <- filtered_data$imports %>%
      group_by(display_period) %>%
      summarise(value = sum(imports_nzd_vfd, na.rm = TRUE)) %>%
      mutate(type = "Import")
    
    export_data <- filtered_data$exports %>%
      group_by(display_period) %>%
      summarise(value = sum(total_exports_nzd_fob, na.rm = TRUE)) %>%
      mutate(type = "Export")
    
    # 合并数据
    combined_data <- bind_rows(import_data, export_data)
    
    # 创建图表
    p <- ggplot(combined_data, aes(x = display_period, y = value/1e6, color = type, group = type)) +
      geom_line(linewidth = 1) +
      labs(
        title = paste0(title, "- Import and export value"),
        x = NULL,
        y = "Amount (million NZD)",
        color = "Type"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 30, hjust = 1))
    
    ggplotly(p)
  })
  
  get_time_title <- function(){
    title_parts <- c()
    if(input$time_range == "specific_year") title_parts <- c(paste0("Year: ", input$year_select_range))
    if(input$time_range == "custom_range") title_parts <- c(paste0("Custom Range: ", input$year_range[1], " - ", input$year_range[2]))
    
    title <- if(length(title_parts) > 0) paste(title_parts, collapse = ", ") else "All time"
    
    return(title)
  }
  
  # Modified code for current_hs_top10_import_countries
  output$current_hs_top10_import_countries <- renderPlotly({
    # Get filtered data
    filtered_data <- filtered_hs_reactive()
    
    # Get top 10 importing countries
    top10_countries <- filtered_data$imports %>%
      group_by(country) %>%
      summarise(total_import = sum(imports_nzd_vfd, na.rm = TRUE)) %>%
      arrange(desc(total_import)) %>%
      slice_head(n = 10) %>%
      pull(country)
    
    # Create time series data for these 10 countries
    top10_data <- filtered_data$imports %>%
      filter(country %in% top10_countries) %>%
      group_by(display_period, country) %>%
      summarise(Value = sum(imports_nzd_vfd, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(hover_text = paste0(
        'time: ', display_period,
        '<br>country: ', country, 
        "<br>Value: $", round(Value/1e6, 2), " million"
      ))
    
    title <- paste("Top 10 Importing Countries -", get_analysis_title(), get_time_title(), collapse = ", ")
    
    # Create line chart
    p <- ggplot(top10_data, aes(x = display_period, y = Value/1e6, color = country, 
                                group = country, text = hover_text)) +
      geom_line(linewidth = 1) +
      labs(
        title = title,
        x = NULL, 
        y = "Value (million NZD)",
        color = "Country"
      ) +
      theme_minimal() +
      theme(legend.position = "right",
            axis.text.x = element_text(angle = 30, hjust = 1))
    
    ggplotly(p, tooltip = 'text')
  })
  
  # Modified code for current_hs_top10_export_countries
  # output$current_hs_top10_export_countries <- renderPlotly({
  #   # Get filtered data
  #   filtered_data <- filtered_hs_reactive()
  #   
  #   # Get top 10 exporting countries
  #   top10_countries <- filtered_data$exports %>%
  #     group_by(country) %>%
  #     summarise(total_export = sum(total_exports_nzd_fob, na.rm = TRUE)) %>%
  #     arrange(desc(total_export)) %>%
  #     slice_head(n = 10) %>%
  #     pull(country)
  #   
  #   # Create time series data for these 10 countries
  #   top10_data <- filtered_data$exports %>%
  #     filter(country %in% top10_countries) %>%
  #     group_by(display_period, country) %>%
  #     summarise(Value = sum(total_exports_nzd_fob, na.rm = TRUE)) %>%
  #     ungroup()
  #   
  #   title <- paste("Top 10 Exporting Countries -", get_analysis_title(), get_time_title(), collapse = ", ")
  #   
  #   # Create line chart
  #   p <- ggplot(top10_data, aes(x = display_period, y = Value/1e6, color = country, group = country)) +
  #     geom_line(linewidth = 1) +
  #     labs(
  #       title = title,
  #       x = NULL, 
  #       y = "Value (million NZD)",
  #       color = "Country"
  #     ) +
  #     theme_minimal() +
  #     theme(legend.position = "right",
  #           axis.text.x = element_text(angle = 30, hjust = 1))
  #   
  #   ggplotly(p)
  # })
  
  # Modified code for current_hs_top10_export_countries
  output$current_hs_top10_export_countries <- renderPlotly({
    # Get filtered data
    filtered_data <- filtered_hs_reactive()
    
    # Get top 10 exporting countries
    top10_countries <- filtered_data$exports %>%
      group_by(country) %>%
      summarise(total_export = sum(total_exports_nzd_fob, na.rm = TRUE)) %>%
      arrange(desc(total_export)) %>%
      slice_head(n = 10) %>%
      pull(country)
    
    # Create time series data for these 10 countries
    top10_data <- filtered_data$exports %>%
      filter(country %in% top10_countries) %>%
      group_by(display_period, country) %>%
      summarise(Value = sum(total_exports_nzd_fob, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(hover_text = paste0(
        'time: ', display_period,
        '<br>country: ', country, 
        "<br>Value: $", round(Value/1e6, 2), " million"
      ))
    
    title <- paste("Top 10 Exporting Countries -", get_analysis_title(), get_time_title(), collapse = ", ")
    
    # Create line chart
    p <- ggplot(top10_data, aes(x = display_period, y = Value/1e6, color = country, 
                                group = country, text = hover_text)) +
      geom_line(linewidth = 1) +
      labs(
        title = title,
        x = NULL, 
        y = "Value (million NZD)",
        color = "Country"
      ) +
      theme_minimal() +
      theme(legend.position = "right",
            axis.text.x = element_text(angle = 30, hjust = 1))
    
    ggplotly(p, tooltip = 'text')
  })
  # 获取当前选择的下一级别
  get_next_level <- reactive({
    if(input$hscode_select != "ALL") {
      return(NULL)  # 已经是最底层，没有下一级
    } else if(input$hs6_select != "ALL") {
      return(list(level = "harmonised_system_code", parent_level = "hs6", parent_value = input$hs6_select))
    } else if(input$hs4_select != "ALL") {
      return(list(level = "hs6", parent_level = "hs4", parent_value = input$hs4_select))
    } else if(input$hs2_select != "ALL") {
      return(list(level = "hs4", parent_level = "hs2", parent_value = input$hs2_select))
    } else if(input$hs2_group_select != "ALL") {
      return(list(level = "hs2", parent_level = "hs2_group", parent_value = input$hs2_group_select))
    } else {
      return(list(level = "hs2_group", parent_level = NULL, parent_value = NULL))
    }
  })
  
  # 下级进口表格
  output$sublevel_import_table <- DT::renderDataTable({
    # 获取下一级别
    next_level <- get_next_level()
    
    # 如果没有下一级，返回一个简单的消息表格
    if(is.null(next_level)) {
      return(
        DT::datatable(
          data.frame(Message = "It is already the lowest level of classification, and there is no subordinate data"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      )
    }
    
    # 使用当前筛选条件构建基础数据
    filtered_imports <- filtered_hs_reactive()$imports
    
    # 按下一级分类汇总数据
    summary_data <- filtered_imports %>%
      group_by(!!sym(next_level$level)) %>%
      summarise(total_import = sum(imports_nzd_vfd, na.rm = TRUE)) %>%
      arrange(desc(total_import))
    
    # 标记前十项
    summary_data <- summary_data %>%
      mutate(is_top10 = row_number() <= 10)
    
    # 添加分类描述
    if(next_level$level == "hs2_group") {
      # 确保hs2_map中的HS_codes是字符类型，与summary_data中的键匹配
      hs2_map_local <- hs2_map %>% 
        mutate(HS_codes = as.character(HS_codes))
      
      summary_data <- summary_data %>%
        mutate(code_key = as.character(!!sym(next_level$level))) %>%
        left_join(hs2_map_local, by = c("code_key" = "HS_codes")) %>%
        mutate(description = ifelse(is.na(HS_description), 
                                    as.character(!!sym(next_level$level)), 
                                    HS_description))
    } else {
      # 确保hs_map中的HS_codes是字符类型，与summary_data中的键匹配
      hs_map_local <- hs_map %>% 
        mutate(HS_codes = as.character(HS_codes))
      
      summary_data <- summary_data %>%
        mutate(code_key = as.character(!!sym(next_level$level))) %>%
        left_join(hs_map_local, by = c("code_key" = "HS_codes")) %>%
        mutate(description = ifelse(is.na(HS_description), 
                                    as.character(!!sym(next_level$level)), 
                                    HS_description))
    }
    
    # 格式化数据用于显示 - 金额转换为百万
    level_name <- next_level$level
    display_data <- summary_data %>%
      rename_with(~ "hs_code", all_of(level_name)) %>%
      mutate(total_import_millions = total_import / 1e6) %>%
      select(hs_code, description, total_import_millions, is_top10)
    # 创建表格标题
    table_caption <- paste0("Imports: Subcategories at level ", next_level$level, " for ", get_analysis_title())
  
    # 创建表格
    dt <- DT::datatable(
      display_data,
      caption = htmltools::tags$caption(
        style = paste(
          'caption-side: top;', 
          'text-align: center;',  # 居中对齐
          'font-weight: bold;', 
          'font-size: 14px;',     # 适合ggplot默认标题字号
          'font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;', # 使用与ggplot相似的字体
          'color: #333333;',      # 与ggplot标题颜色相似
          'margin-bottom: 10px;', # 增加底部边距
          'width: 100%;'          # 确保标题占据整个表格宽度
        ),
        table_caption
      ),
      colnames = c(next_level$level, "description", "Import value (million NZD)", "前十"),
      options = list(
        pageLength = 15,
        dom = 'ftp',
        columnDefs = list(list(targets = 3, visible = FALSE))  # 隐藏is_top10列
      ),
      rownames = FALSE
    )
    
    # 格式化百万金额为两位小数
    dt <- dt %>% DT::formatRound(
      columns = 'total_import_millions',
      digits = 2
    )
    
    # 根据是否前十添加样式
    dt <- dt %>% DT::formatStyle(
      columns = c('hs_code', 'description', 'total_import_millions'),
      valueColumns = 'is_top10',
      backgroundColor = DT::styleEqual(c(TRUE, FALSE), c('#e6f7ff', 'white')),
      fontWeight = DT::styleEqual(c(TRUE, FALSE), c('bold', 'normal'))
    )
    
    return(dt)
  })
    
    
  # 下级分类进口走势
  output$sublevel_import_trend <- renderPlotly({
    # 获取下一级别
    next_level <- get_next_level()
    
    # 如果没有下一级，显示空图表
    if(is.null(next_level)) {
      return(plot_ly() %>% 
               layout(title = "It is already the lowest level of classification, and there is no subordinate data",
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
    }
    
    # 使用当前筛选条件构建基础数据
    filtered_imports <- filtered_hs_reactive()$imports
    
    # 获取下一级的所有唯一值
    next_level_values <- unique(filtered_imports[[next_level$level]])
    
    # 如果值太多，只选择前10个（按总金额排序）
    if(length(next_level_values) > 10) {
      top_values <- filtered_imports %>%
        group_by(!!sym(next_level$level)) %>%
        summarise(total = sum(imports_nzd_vfd, na.rm = TRUE)) %>%
        arrange(desc(total)) %>%
        slice_head(n = 10) %>%
        pull(!!sym(next_level$level))
      
      filtered_imports <- filtered_imports %>% filter(!!sym(next_level$level) %in% top_values)
      next_level_values <- top_values
    }
    
    agg_data <- filtered_imports %>%
      group_by(display_period, !!sym(next_level$level)) %>%
      summarise(value = sum(imports_nzd_vfd, na.rm = TRUE)) %>%
      ungroup()
    
    title = paste("SubLevel (", next_level$level, ") Import trend - ", get_analysis_title(), collapse = ", ")
    if (input$hs2_group_select == "ALL") {
      hs2_map <- hs2_map %>%
        mutate(HS_codes = as.factor(HS_codes))
      agg_data <- agg_data %>%
        left_join(hs2_map, by = setNames("HS_codes", next_level$level)) %>%
        mutate(
          display_description = truncate_text(HS_description, 5),
          # 在悬停文本中也使用截断描述
          hover_text = paste0(
            'time : ', display_period,
            '<br>', next_level$level, ": ", !!sym(next_level$level), 
            '<br>description : ', display_description, 
            "<br>Value: $", round(value/1e6, 2), " million"
          )
        )
    } else {
      hs_map <- hs_map %>%
        mutate(HS_codes = as.factor(HS_codes))
      agg_data <- agg_data %>%
        left_join(hs_map, by = setNames("HS_codes", next_level$level)) %>%
        mutate(
          display_description = truncate_text(HS_description, 10),
          # 在悬停文本中也使用截断描述
          hover_text = paste0(
            'time : ', display_period,
            '<br>', next_level$level, ": ", !!sym(next_level$level), 
            '<br>description : ', display_description, 
            "<br>Value: $", round(value/1e6, 2), " million"
          )
        )
    }
    # 创建图表
    p <- ggplot(agg_data, aes(x = display_period, y = value/1e6, 
                              color = !!sym(next_level$level), group = !!sym(next_level$level),
                              text = hover_text,
                              )) +
      geom_line(linewidth = 1) +
      labs(
        title = title,
        x = NULL,
        y = "Import value VFD (million NZD)",
        color = next_level$level
      ) +
      theme_minimal() +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 30, hjust = 1))
    
    ggplotly(p,tooltip="text")
  })
  
  output$sublevel_export_table <- DT::renderDataTable({
    # 获取下一级别
    next_level <- get_next_level()
    
    # 如果没有下一级，返回一个简单的消息表格
    if(is.null(next_level)) {
      return(
        DT::datatable(
          data.frame(Message = "It is already the lowest level of classification, and there is no subordinate data"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      )
    }
    
    # 使用当前筛选条件构建基础数据
    filtered_exports <- filtered_hs_reactive()$exports
    
    # 按下一级分类汇总数据
    summary_data <- filtered_exports %>%
      group_by(!!sym(next_level$level)) %>%
      summarise(total_export = sum(exports_nzd_fob, na.rm = TRUE)) %>%
      arrange(desc(total_export))
    
    # 标记前十项
    summary_data <- summary_data %>%
      mutate(is_top10 = row_number() <= 10)
    
    # 添加分类描述
    if(next_level$level == "hs2_group") {
      # 确保hs2_map中的HS_codes是字符类型，与summary_data中的键匹配
      hs2_map_local <- hs2_map %>% 
        mutate(HS_codes = as.character(HS_codes))
      
      summary_data <- summary_data %>%
        mutate(code_key = as.character(!!sym(next_level$level))) %>%
        left_join(hs2_map_local, by = c("code_key" = "HS_codes")) %>%
        mutate(description = ifelse(is.na(HS_description), 
                                    as.character(!!sym(next_level$level)), 
                                    HS_description))
    } else {
      # 确保hs_map中的HS_codes是字符类型，与summary_data中的键匹配
      hs_map_local <- hs_map %>% 
        mutate(HS_codes = as.character(HS_codes))
      
      summary_data <- summary_data %>%
        mutate(code_key = as.character(!!sym(next_level$level))) %>%
        left_join(hs_map_local, by = c("code_key" = "HS_codes")) %>%
        mutate(description = ifelse(is.na(HS_description), 
                                    as.character(!!sym(next_level$level)), 
                                    HS_description))
    }
    
    # 格式化数据用于显示 - 金额转换为百万
    level_name <- next_level$level
    display_data <- summary_data %>%
      rename_with(~ "hs_code", all_of(level_name)) %>%
      mutate(total_export_millions = total_export / 1e6) %>%
      select(hs_code, description, total_export_millions, is_top10)
    
    # 创建表格标题
    table_caption <- paste0("Exports: Subcategories at level ", next_level$level, " for ", get_analysis_title())
    
    # 创建表格
    dt <- DT::datatable(
      display_data,
      caption = htmltools::tags$caption(
        style = paste(
          'caption-side: top;', 
          'text-align: center;',  # 居中对齐
          'font-weight: bold;', 
          'font-size: 14px;',     # 适合ggplot默认标题字号
          'font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;', # 使用与ggplot相似的字体
          'color: #333333;',      # 与ggplot标题颜色相似
          'margin-bottom: 10px;', # 增加底部边距
          'width: 100%;'          # 确保标题占据整个表格宽度
        ),
        table_caption
      ),
      colnames = c(level_name, "description", "Import value VFD (million NZD)", "前十"),
      options = list(
        pageLength = 15,
        dom = 'ftp',
        columnDefs = list(list(targets = 3, visible = FALSE))  # 隐藏is_top10列
      ),
      rownames = FALSE
    )
    
    # 格式化百万金额为两位小数
    dt <- dt %>% DT::formatRound(
      columns = 'total_export_millions',
      digits = 2
    )
    
    # 根据是否前十添加样式
    dt <- dt %>% DT::formatStyle(
      columns = c('hs_code', 'description', 'total_export_millions'),
      valueColumns = 'is_top10',
      backgroundColor = DT::styleEqual(c(TRUE, FALSE), c('#e6f7ff', 'white')),
      fontWeight = DT::styleEqual(c(TRUE, FALSE), c('bold', 'normal'))
    )
    
    return(dt)
  })
  # 下级分类出口走势
  output$sublevel_export_trend <- renderPlotly({
    # 获取下一级别
    next_level <- get_next_level()
    
    # 如果没有下一级，显示空图表
    if(is.null(next_level)) {
      return(plot_ly() %>% 
               layout(title = "It is already the lowest level of classification, and there is no subordinate data",
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
    }
    
    # 使用当前筛选条件构建基础数据
    filtered_exports <- filtered_hs_reactive()$exports
    
    # 获取下一级的所有唯一值
    next_level_values <- unique(filtered_exports[[next_level$level]])
    
    # 如果值太多，只选择前10个（按总金额排序）
    if(length(next_level_values) > 10) {
      top_values <- filtered_exports %>%
        group_by(!!sym(next_level$level)) %>%
        summarise(total = sum(total_exports_nzd_fob, na.rm = TRUE)) %>%
        arrange(desc(total)) %>%
        slice_head(n = 10) %>%
        pull(!!sym(next_level$level))
      
      filtered_exports <- filtered_exports %>% filter(!!sym(next_level$level) %in% top_values)
      next_level_values <- top_values
    }
    
    # 准备按年份和下一级分组的数据
    agg_data <- filtered_exports %>%
      group_by(display_period, !!sym(next_level$level)) %>%
      summarise(value = sum(total_exports_nzd_fob, na.rm = TRUE)) %>%
      ungroup()
    if (input$hs2_group_select == "ALL") {
      hs2_map <- hs2_map %>%
        mutate(HS_codes = as.factor(HS_codes))
      agg_data <- agg_data %>%
        left_join(hs2_map, by = setNames("HS_codes", next_level$level)) %>%
        mutate(
          display_description = truncate_text(HS_description, 5),
          # 在悬停文本中也使用截断描述
          hover_text = paste0(
            'time : ', display_period,
            '<br>', next_level$level, ": ", !!sym(next_level$level), 
            '<br>description : ', display_description, 
            "<br>Value: $", round(value/1e6, 2), " million"
          )
        )
    } else {
      hs_map <- hs_map %>%
        mutate(HS_codes = as.factor(HS_codes))
      agg_data <- agg_data %>%
        left_join(hs_map, by = setNames("HS_codes", next_level$level)) %>%
        mutate(
          display_description = truncate_text(HS_description, 10),
          # 在悬停文本中也使用截断描述
          hover_text = paste0(
            'time : ', display_period,
            '<br>', next_level$level, ": ", !!sym(next_level$level), 
            '<br>description : ', display_description, 
            "<br>Value: $", round(value/1e6, 2), " million"
          )
        )
    }
    title = paste("SubLevel (", next_level$level, ") Export trend - ", get_analysis_title(), collapse = ", ")
    # 创建图表
    p <- ggplot(agg_data, aes(x = display_period, y = value/1e6, color = !!sym(next_level$level), 
                              group = !!sym(next_level$level),
                              text = hover_text)) +
      geom_line(linewidth = 1) +
      labs(
        title = title,
        x = NULL,
        y = "Export value (million NZD)",
        color = next_level$level
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p,tooltip='text')
  })
  
  # 实现进口桑基图
  output$hs_subcategory_countries_import_sankey <- renderPlotly({
    # 获取下一级别
    next_level <- get_next_level()
    
    # 如果没有下一级，显示空图表
    if(is.null(next_level)) {
      return(plot_ly() %>% 
               layout(title = "It is already the lowest level of classification, and there is no subordinate data",
                      annotations = list(
                        x = 0.5,
                        y = 0.5,
                        text = "Please select a higher-level classification to view trade flows",
                        showarrow = FALSE,
                        font = list(size = 14)
                      ),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
    }
    
    # 从reactive表达式获取过滤后的数据
    filtered_imports <- filtered_hs_reactive()$imports
    
    # 获取下一级的分类数据
    subcategory_country_data <- filtered_imports %>%
      group_by(!!sym(next_level$level), country) %>%
      summarise(value = sum(imports_nzd_vfd, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(value))
    
    # 获取前10个子分类（按金额排序）
    top_subcategories <- subcategory_country_data %>%
      group_by(!!sym(next_level$level)) %>%
      summarise(total_value = sum(value)) %>%
      arrange(desc(total_value)) %>%
      slice_head(n = 10) %>%
      pull(!!sym(next_level$level))
    
    # 获取前10个国家（按金额排序）
    top_countries <- subcategory_country_data %>%
      group_by(country) %>%
      summarise(total_value = sum(value)) %>%
      arrange(desc(total_value)) %>%
      slice_head(n = 10) %>%
      pull(country)
    
    # 筛选数据只包含前10个子分类和前10个国家，并将其他归为"其他"类别
    sankey_data <- subcategory_country_data %>%
      mutate(
        subcategory = ifelse(!!sym(next_level$level) %in% top_subcategories, 
                             as.character(!!sym(next_level$level)), 
                             "other subcategories"),
        country = ifelse(country %in% top_countries, 
                         as.character(country), 
                         "other countries")
      ) %>%
      group_by(subcategory, country) %>%
      summarise(value = sum(value)) %>%
      ungroup()
    
    # 获取所有唯一的子分类和国家，用于构建桑基图节点
    unique_subcategories <- sort(unique(sankey_data$subcategory))
    unique_countries <- sort(unique(sankey_data$country))
    all_nodes <- c(unique_subcategories, unique_countries)
    
    # 获取HS码描述（根据层级）
    if(next_level$level == "hs2_group") {
      # 确保数据类型一致
      hs2_map <- hs2_map %>% mutate(HS_codes = as.character(HS_codes))
  
      # 使用hs2_map获取描述
      sankey_data <- sankey_data %>%
        mutate(subcategory = as.character(subcategory)) %>%
        left_join(hs2_map, by = c("subcategory" = "HS_codes")) %>%
        mutate(
          hover_text = paste0(
            ifelse(!is.na(HS_description), paste0(" (", truncate_text(HS_description, 10), ")"), "")
          )
        )
    } else {
      # 确保数据类型一致
      hs_map <- hs_map %>% mutate(HS_codes = as.character(HS_codes))
      
      # 使用hs_map获取描述
      sankey_data <- sankey_data %>%
        mutate(subcategory = as.character(subcategory)) %>%
        left_join(hs_map, by = c("subcategory" = "HS_codes")) %>%
        mutate(
          hover_text = paste0(
            ifelse(!is.na(HS_description), paste0(" (", truncate_text(HS_description, 10), ")"), "")
          )
        )
    }
    
    title = paste()
    
    sankey_data %>%
      plot_ly(
        type = "sankey",
        orientation = "h",
        node = list(
          label = all_nodes
        ),
        link = list(
          source = as.numeric(factor(.$country, levels = unique_countries)) - 1 + length(unique_subcategories),
          target = as.numeric(factor(.$subcategory, levels = unique_subcategories)) - 1,
          value = .$value / 1e6,
          label = .$hover_text
        )
      ) %>%
      layout(
        title = paste("Imports:", get_analysis_title(), 
                              next_level$level,
                              "Trade Flows by Country -", get_time_title(),
                              sep = " ")
        # font = list(size = 10)
      )
  })
  
  # 实现出口桑基图
  output$hs_subcategory_countries_export_sankey <- renderPlotly({
    # 获取下一级别
    next_level <- get_next_level()
    
    # 如果没有下一级，显示空图表
    if(is.null(next_level)) {
      return(plot_ly() %>% 
               layout(title = "It is already the lowest level of classification, and there is no subordinate data",
                      annotations = list(
                        x = 0.5,
                        y = 0.5,
                        text = "Please select a higher-level classification to view trade flows",
                        showarrow = FALSE,
                        font = list(size = 14)
                      ),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
    }
    
    # 从reactive表达式获取过滤后的数据
    filtered_exports <- filtered_hs_reactive()$exports
    
    # 获取下一级的分类数据
    subcategory_country_data <- filtered_exports %>%
      group_by(!!sym(next_level$level), country) %>%
      summarise(value = sum(total_exports_nzd_fob, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(value))
    
    # 获取前10个子分类（按金额排序）
    top_subcategories <- subcategory_country_data %>%
      group_by(!!sym(next_level$level)) %>%
      summarise(total_value = sum(value)) %>%
      arrange(desc(total_value)) %>%
      slice_head(n = 10) %>%
      pull(!!sym(next_level$level))
    
    # 获取前10个国家（按金额排序）
    top_countries <- subcategory_country_data %>%
      group_by(country) %>%
      summarise(total_value = sum(value)) %>%
      arrange(desc(total_value)) %>%
      slice_head(n = 10) %>%
      pull(country)
    
    # 筛选数据只包含前10个子分类和前10个国家，并将其他归为"其他"类别
    sankey_data <- subcategory_country_data %>%
      mutate(
        subcategory = ifelse(!!sym(next_level$level) %in% top_subcategories, 
                             as.character(!!sym(next_level$level)), 
                             "other subcategories"),
        country = ifelse(country %in% top_countries, 
                         as.character(country), 
                         "other countries")
      ) %>%
      group_by(country, subcategory) %>%
      summarise(value = sum(value)) %>%
      ungroup()
    
    # 获取所有唯一的国家和子分类，用于构建桑基图节点
    unique_countries <- sort(unique(sankey_data$country))
    unique_subcategories <- sort(unique(sankey_data$subcategory))
    all_nodes <- c(unique_countries, unique_subcategories)
    if(next_level$level == "hs2_group") {
      # 确保数据类型一致
      hs2_map <- hs2_map %>% mutate(HS_codes = as.character(HS_codes))
      
      # 使用hs2_map获取描述
      sankey_data <- sankey_data %>%
        mutate(subcategory = as.character(subcategory)) %>%
        left_join(hs2_map, by = c("subcategory" = "HS_codes")) %>%
        mutate(
          hover_text = paste0(
            ifelse(!is.na(HS_description), paste0(" (", truncate_text(HS_description, 10), ")"), "")
          )
        )
    } else {
      # 确保数据类型一致
      hs_map <- hs_map %>% mutate(HS_codes = as.character(HS_codes))
      
      # 使用hs_map获取描述
      sankey_data <- sankey_data %>%
        mutate(subcategory = as.character(subcategory)) %>%
        left_join(hs_map, by = c("subcategory" = "HS_codes")) %>%
        mutate(
          hover_text = paste0(
            ifelse(!is.na(HS_description), paste0(" (", truncate_text(HS_description, 10), ")"), "")
          )
        )
    }
    
    sankey_data %>%
      plot_ly(
        type = "sankey",
        orientation = "h",
        node = list(
          label = all_nodes
        ),
        link = list(
          source = as.numeric(factor(.$subcategory, levels = unique_subcategories)) - 1 + length(unique_countries),
          target = as.numeric(factor(.$country, levels = unique_countries)) - 1,
          value = .$value / 1e6,
          label = .$hover_text
        )
      ) %>%
      layout(
        title = paste("ports:", get_analysis_title(), 
                      next_level$level,
                      "Trade Flows by Country -", get_time_title(),
                      sep = " ")
        # font = list(size = 10)
      )
  })
  
  # 第三个 tab 是说明书
  output$hs2_group_table <- DT::renderDataTable({
    hs2_group_data <- data.frame(
      编号 = 1:21,
      分组代码 = c("01-05", "06-14", "15", "16-24", "25-27", "28-38", "39-40", "41-43", "44-46", "47-49", 
               "50-63", "64-67", "68-70", "71", "72-83", "84-85", "86-89", "90-92", "93", "94-96", "97-99"),
      商品类别 = c("活动物及动物产品", "植物产品", "动植物油脂", "食品、饮料、烟草", "矿产品", "化工产品", 
               "塑料和橡胶", "皮革及其制品", "木及木制品", "纸张、纸浆及纸制品", 
               "纺织品及纺织制品", "鞋类、帽类等", "石材、陶瓷、玻璃", "珠宝、贵金属", "贱金属及其制品", 
               "机械、电气设备", "运输设备", "精密仪器及设备", "武器和弹药", "杂项制品", "艺术品、收藏品")
    )
    
    DT::datatable(hs2_group_data, 
                  options = list(
                    pageLength = 10,
                    lengthMenu = c(5, 10, 15, 20),
                    dom = 'tp'
                  ),
                  rownames = FALSE)
  })
}

# 启动应用
shinyApp(ui = ui, server = server)
