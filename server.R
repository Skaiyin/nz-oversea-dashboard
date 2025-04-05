# 加载必要的库
library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(tidyverse)
library(plotly)

imports_raw <- readRDS("10_years_imports.rds")
exports_raw <- readRDS("10_years_exports.rds")

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
    geom_line(aes(x = month, y = Total_Imports_vfd / 1e6, color = "VFD"), linewidth = 1) +
    # 绘制 CIF (以百万为单位)
    geom_line(aes(x = month, y = Total_Imports_cif / 1e6, color = "CIF"), linewidth = 1) +
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
    geom_line(aes(x = month, y = Total_Only_Exports_nzd_fob / 1e6, color = "Only_Exports"),  size = 1) +
    geom_line(aes(x = month, y = Total_Reexports_nzd_fob / 1e6, color = "Reexports"), size = 1) +
    geom_line(aes(x = month, y = Total_Exports_nzd_fob / 1e6, color = "Exports"), size = 1) +
    labs(title = "Monthly Imports fob($NZD)", 
         x = "Month", 
         y = "Total Imports ($NZD, in millions)") +
    theme_minimal() +
    scale_color_manual(values = c("blue", "red", "green"))
}


# 数据准备
geo_data <- bind_rows(
  imports_raw %>% 
    group_by(iso3) %>%
    summarise(Value = sum(imports_nzd_cif)) %>%
    mutate(Type = "Import"),
  exports_raw %>%
    group_by(iso3) %>% 
    summarise(Value = sum(total_exports_nzd_fob)) %>%
    mutate(Type = "Export")
)

# 绘制
plot_geo(geo_data) %>%
  add_trace(
    split = ~Type,
    z = ~Value,
    locations = ~iso3,
    colorscale = "Viridis",
    showscale = FALSE
  ) %>%
  layout(
    geo = list(
      showland = TRUE,
      landcolor = "#ECECEC",
      subunitcolor = "#FFFFFF"
    ),
    grid = list(
      rows = 1,
      columns = 2,
      pattern = "independent"
    ),
    annotations = list(
      list(x=0.25, y=0.5, text="Imports", showarrow=F),
      list(x=0.75, y=0.5, text="Exports", showarrow=F)
    )
  )
# UI部分
ui <- dashboardPage(
  dashboardHeader(title = "New Zealand OverSea Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "tab_overview", icon = icon("tachometer-alt")),
      menuItem("数据分析", tabName = "tab_analysis", icon = icon("chart-line")),
      menuItem("设置", tabName = "tab_settings", icon = icon("cogs"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # 首页 Tab
      tabItem(tabName = "tab_overview",
              fluidRow(
                box(title = "选择时间和显示粒度", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("time_range", "选择时间范围:", 
                                choices = c("全部时间", "具体年份", "自定义范围"),
                                selected = "全部时间"),
                    uiOutput("year_range_ui"),  # 动态显示年份选择或日期范围选择
                    selectInput("time_period", "显示粒度:", choices = c("按年" = "year", "按月" = "month"), selected = "month",)
                )
              ),
              fluidRow(
                box(title = "Import and export overview", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("overview_plot")
                ),
              ),
              fluidRow(
                column(6,
                       box(title = "import_overview", status = "warning", solidHeader = TRUE, width = 12,
                           plotlyOutput("import_overview")
                       )
                ),
                column(6,
                       box(title = "export_overview", status = "danger", solidHeader = TRUE, width = 12,
                           plotlyOutput("export_overview")
                       )
                )
              ),
              fluidRow(
                column(6,
                       box(title = "import_geo", status = "warning", solidHeader = TRUE, width = 12,
                           plotlyOutput("geo_imports")
                       )
                ),
                column(6,
                       box(title = "export_geo", status = "danger", solidHeader = TRUE, width = 12,
                           plotlyOutput("geo_exports")
                       )
                )
              ),
              fluidRow(
                box(title = "Top Ten Countries", status = "primary", solidHeader = TRUE, width = 12,
                    tabsetPanel(
                      tabPanel("Import", 
                               fluidRow(
                                 column(12,
                                        plotlyOutput("top10_import_countries_value"),
                                 ),
                               )
                      ),
                      tabPanel("Export", 
                               fluidRow(
                                 column(12,
                                        plotlyOutput("top10_export_countries_value"),
                                 ),
                               )
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Top Ten Commodities", status = "primary", solidHeader = TRUE, width = 12,
                    tabsetPanel(
                      tabPanel("Import", 
                               fluidRow(
                                 column(12,
                                        plotlyOutput("top10_import_commodities_combined")
                                 )
                               )
                      ),
                      tabPanel("Export", 
                               fluidRow(
                                 column(12,
                                        plotlyOutput("top10_export_commodities_combined")
                                 )
                               )
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Year-on-Year and Month-on-Month Analysis of Imports and Exports", status = "success", solidHeader = TRUE, width = 12,
                    tabsetPanel(
                      tabPanel("Import", 
                               fluidRow(
                                 column(6,
                                        plotlyOutput("import_yoy_growth"),
                                        h4("Year-on-Year Growth Rate of Imports (%)")
                                 ),
                                 column(6,
                                        plotlyOutput("import_mom_growth"),
                                        h4("Month-on-Month Growth Rate of Imports (%)")
                                 )
                               )
                      ),
                      tabPanel("Export", 
                               fluidRow(
                                 column(6,
                                        plotlyOutput("export_yoy_growth"),
                                        h4("Year-on-Year Growth Rate of Exports (%)")
                                 ),
                                 column(6,
                                        plotlyOutput("export_mom_growth"),
                                        h4("Month-on-Month Growth Rate of Exports (%)")
                                 )
                               )
                      )
                    )
                )
              ),
      ),
      
      # 数据分析 Tab
      tabItem(tabName = "tab_analysis",
              fluidRow(
                box(title = "商品分类选择", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(3,
                             selectInput("hs2_group_select", "HS2 group:", 
                                         choices = c("全部" = "ALL"), 
                                         selected = "ALL")
                      ),
                      column(3,
                             selectInput("hs2_select", "HS2 code:", 
                                         choices = c("全部" = "ALL"), 
                                         selected = "ALL")
                      ),
                      column(3,
                             selectInput("hs4_select", "HS4 code:", 
                                         choices = c("全部" = "ALL"), 
                                         selected = "ALL")
                      ),
                      column(3,
                             selectInput("hs5_select", "HS5 code:", 
                                         choices = c("全部" = "ALL"), 
                                         selected = "ALL")
                      ),
                      column(4,
                             selectInput("hscode_select", "Full HS Code:", 
                                         choices = c("全部" = "ALL"), 
                                         selected = "ALL")
                      ),
                      column(4,
                             div(
                               textInput("hscode_input", "Directly enter HS Code:", ""),
                               actionButton("search_hscode", "Query", icon = icon("search"), 
                                            style = "margin-top: 5px; width: 100%;")
                             )
                      )
                    ),
                    fluidRow(
                      column(6,
                             box(title = "Import and export trends of currently selected HS code", status = "info", solidHeader = TRUE, width = 12,
                                 plotlyOutput("current_hs_trend")
                             )
                      ),
                      column(6,
                             box(title = "Current top ten HS code trading countries", status = "info", solidHeader = TRUE, width = 12,
                                 plotlyOutput("current_hs_top10_countries")
                             )
                      )
                    ),
                    fluidRow(
                      column(6,
                             box(title = "Import trends by sub level", status = "warning", solidHeader = TRUE, width = 12,
                                 plotlyOutput("sublevel_import_trend")
                             )
                      ),
                      column(6,
                             box(title = "Export trends by sub level", status = "danger", solidHeader = TRUE, width = 12,
                                 plotlyOutput("sublevel_export_trend")
                             )
                      )
                    ),
                )
              ),
              fluidRow(
                box(title = "Trade flows by commodity and continent", status = "info", solidHeader = TRUE, width = 12,
                    tabsetPanel(
                      tabPanel("Import", plotlyOutput("continent_import_sankey", height = 500)),
                      tabPanel("Export", plotlyOutput("continent_export_sankey", height = 500))
                    )
                )
              ),
              fluidRow(
                box(title = "Trade Structure Tree", status = "primary", solidHeader = TRUE, width = 12,
                    tabsetPanel(
                      tabPanel("Export Structure", plotOutput("export_treemap", height = 500)),
                      tabPanel("Import Structure", plotOutput("import_treemap", height = 500))
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
                      tags$li(strong("商品分类选择"), " - 可通过HS码分类层级（HS2分组、HS2码、HS4码、HS5码、详细HSCode）筛选特定商品。"),
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
                      tags$li(strong("HS5码"), " - 五位数编码，进一步细分商品类别。"),
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
  
  data_reactive <- reactive({
    # 根据时间范围进行数据筛选
    if (input$time_range == "全部时间") {
      imports_data <- imports_raw
      exports_data <- exports_raw
    } else if (input$time_range == "具体年份") {
      imports_data <- filter(imports_raw, year == input$year_select_range)
      exports_data <- filter(exports_raw, year == input$year_select_range)
    } else if (input$time_range == "自定义范围") {
      imports_data <- filter(imports_raw, month >= input$date_range[1] & month <= input$date_range[2])
      exports_data <- filter(exports_raw, month >= input$date_range[1] & month <= input$date_range[2])
    }
    
    trade_balance_data <- imports_data %>%
      group_by(month) %>%
      summarise(Imports = sum(imports_nzd_cif, na.rm = TRUE)) %>%
      full_join(
        exports_data %>% 
          group_by(month) %>% 
          summarise(Exports = sum(total_exports_nzd_fob, na.rm = TRUE)),
        by = "month"
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
  
  
  output$overview_plot <- renderPlotly({
    p <- data_reactive()$trade_balance_data %>% ggplot() +
      geom_area(aes(x=month, y=Imports/1e6), fill="#FF6B6B", alpha=0.3) + 
      geom_area(aes(x=month, y=Exports/1e6), fill="#4ECDC4", alpha=0.3) +
      geom_line(aes(x=month, y=Balance/1e6), color="#2D3047", linewidth=1.2) +
      geom_label(
        data = data_reactive()$trade_balance_data %>% 
          filter(Balance == max(Balance) | Balance == min(Balance)),
        aes(x=month, y=Balance/1e6, 
            label=paste(scales::dollar(Balance/1e6),"M")),
        color="#FFFFFF", fill="#2D3047"
      ) +
      geom_vline(
        data = data_reactive()$trade_balance_data %>% 
          filter(sign(Balance) != sign(lag(Balance))),
        aes(xintercept=month), 
        linetype="dashed", color="#FF6B6B"
      )+
      scale_y_continuous(
        name = "Trade Value (Millions NZD)",
        sec.axis = sec_axis(~.*1, name="Trade Balance (Millions $NZD)")
      ) +
      labs(title = "2021 Trade Flow Dynamics") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$year_range_ui <- renderUI({
    if (input$time_range == "具体年份") {
      # 显示年份选择框
      selectInput("year_select_range", "选择年份:", choices = unique(imports_raw$year), selected = unique(imports_raw$year)[1])
    } else if (input$time_range == "自定义范围") {
      # 显示日期范围选择
      dateRangeInput("date_range", "选择日期范围:",
                     start = min(choices), end = max(choices),
                     min = min(choices), max = max(choices))
    } else {
      return(NULL)  # 全部时不显示任何选择
    }
  })
  
  output$import_overview <- renderPlotly({
    p <- data_reactive()$imports_data %>% imports_overview_plot(month)
    ggplotly(p)
  })
  
  output$export_overview <- renderPlotly({
    p <- data_reactive()$exports_data %>% exports_overview_plot(month)
    ggplotly(p)
  })
  
  # 第二部分是地理，加入单独的时间进度条，同步变化, 
  output$geo_imports <- renderPlotly({
    plot_geo(data_reactive()$geo_imports) %>%
      add_trace(
        split = ~Type,
        z = ~Value,
        locations = ~iso3,
        colorscale = "Viridis",
        showscale = FALSE
      ) %>%
      layout(
        geo = list(
          showland = TRUE,
          landcolor = "#ECECEC",
          subunitcolor = "#FFFFFF"
        ),
        grid = list(
          rows = 1,
          columns = 2,
          pattern = "independent"
        )
      )
  })
  
  output$geo_exports <- renderPlotly({
    plot_geo(data_reactive()$geo_exports) %>%
      add_trace(
        split = ~Type,
        z = ~Value,
        locations = ~iso3,
        colorscale = "Viridis",
        showscale = FALSE
      ) %>%
      layout(
        geo = list(
          showland = TRUE,
          landcolor = "#ECECEC",
          subunitcolor = "#FFFFFF"
        ),
        grid = list(
          rows = 1,
          columns = 2,
          pattern = "independent"
        )
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
      group_by(year, country) %>%
      summarise(Value = sum(imports_nzd_vfd, na.rm = T)) %>%
      ungroup()
    
    p1 <- ggplot(top10_data, aes(x = year, y = Value/1e6, color = country, group = country)) +
      geom_line(linewidth = 1) +
      labs(
        x = "年", 
        y = "进口额 (百万 NZD)",
        color = "国家"
      ) +
      theme_minimal() +
      theme(legend.position = "none") 
    
    fig1 <- ggplotly(p1)
    
    
    # 计算每年总进口额
    monthly_total <- data_reactive()$imports_data %>%
      group_by(year) %>%
      summarise(Total = sum(imports_nzd_vfd, na.rm = T))
    
    # 按月份整理这10个国家的数据并计算比例
    top10_data <- data_reactive()$imports_data %>%
      filter(country %in% top10_countries) %>%
      group_by(year, country) %>%
      summarise(Value = sum(imports_nzd_vfd, na.rm = T)) %>%
      ungroup() %>%
      left_join(monthly_total, by = "year") %>%
      mutate(Percentage = Value / Total * 100)
    
    p2 <- ggplot(top10_data, aes(x = year, y = Percentage, color = country, group = country)) +
      geom_line(linewidth = 1) +
      labs(
        x = "年", 
        y = "占总进口比例 (%)",
        color = "国家"
      ) +
      theme_minimal()
    
    fig2 <- ggplotly(p2)
    subplot(style(fig1, showlegend = F), fig2 ,nrows = 1, shareY = F, titleX = T, titleY=T, shareX = T) %>%
      layout(showlegend = TRUE) 
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
      group_by(year, country) %>%
      summarise(Value = sum(total_exports_nzd_fob, na.rm = TRUE)) %>%
      ungroup()
    
    p1 <- ggplot(top10_data, aes(x = year, y = Value/1e6, color = country, group = country)) +
      geom_line(linewidth = 1) +
      labs(
        x = "年", 
        y = "出口额 (百万 NZD)",
        color = "国家"
      ) +
      theme_minimal()
    
    fig1 <- ggplotly(p1)
    
    # 计算每月总出口额
    monthly_total <- data_reactive()$exports_data %>%
      group_by(year) %>%
      summarise(Total = sum(total_exports_nzd_fob, na.rm = T))
    
    # 按月份整理这10个国家的数据并计算比例
    top10_data <- data_reactive()$exports_data %>%
      filter(country %in% top10_countries) %>%
      group_by(year, country) %>%
      summarise(Value = sum(total_exports_nzd_fob, na.rm = T)) %>%
      ungroup() %>%
      left_join(monthly_total, by = "year") %>%
      mutate(Percentage = Value / Total * 100)
    
    p2 <- ggplot(top10_data, aes(x = year, y = Percentage, color = country, group = country)) +
      geom_line(linewidth = 1) +
      labs(
        x = "年", 
        y = "占总出口比例 (%)",
        color = "国家"
      ) +
      theme_minimal()
    
    fig2 <- ggplotly(p2)
    subplot(style(fig1, showlegend = F), fig2 ,nrows = 1, shareY = F, titleX = T, titleY=T, shareX = T) %>%
      layout(showlegend = TRUE) 
  })
  
  # 前十进出口商品
  # 前十进口商品
  output$top10_import_commodities_combined <- renderPlotly({
    # 获取前10名商品（按总额排名）- 使用VFD
    top10_commodities <- data_reactive()$imports_data %>%
      group_by(harmonised_system_code) %>%
      summarise(Total = sum(imports_nzd_vfd, na.rm = T)) %>%
      arrange(desc(Total)) %>%
      slice_head(n = 10) %>%
      pull(harmonised_system_code)

    
    # 按月份整理这10个商品的数据 - 金额 (VFD)
    top10_data_value <- data_reactive()$imports_data %>%
      filter(harmonised_system_code %in% top10_commodities) %>%
      group_by(year, harmonised_system_code) %>%
      summarise(Value = sum(imports_nzd_vfd, na.rm = T)) %>%
      ungroup()
    
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
      mutate(Percentage = Value / Total * 100)
    
    # 创建金额图
    p1 <- ggplot(top10_data_value, aes(x = year, y = Value/1e6, color = harmonised_system_code, group = harmonised_system_code)) +
      geom_line(linewidth = 1) +
      labs(
        x = "月份", 
        y = "进口金额 VFD (百万 NZD)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")  # 移除图例
    
    # 创建比例图
    p2 <- ggplot(top10_data_percent, aes(x = year, y = Percentage, color = harmonised_system_code, group = harmonised_system_code)) +
      geom_line(linewidth = 1) +
      labs(
        x = "月份", 
        y = "占总进口比例 (%)"
      ) +
      theme_minimal() 
    
    # 转换为plotly对象
    fig1 <- ggplotly(p1) 
    fig2 <- ggplotly(p2)
    
    # 创建子图并排列，使用共享的X轴
    subplot(style(fig1, showlegend = F), fig2 ,nrows = 1, shareY = F, titleX = T, titleY=T, shareX = T) %>%
      layout(showlegend = TRUE)
  })
  
  output$top10_export_commodities_combined <- renderPlotly({
    # 获取前10名商品（按总额排名）- 使用VFD
    top10_commodities <- data_reactive()$exports_data %>%
      group_by(harmonised_system_code) %>%
      summarise(Total = sum(total_exports_nzd_fob, na.rm = T)) %>%
      arrange(desc(Total)) %>%
      slice_head(n = 10) %>%
      pull(harmonised_system_code)
    
    
    # 按月份整理这10个商品的数据 - 金额 (VFD)
    top10_data_value <- data_reactive()$exports_data %>%
      filter(harmonised_system_code %in% top10_commodities) %>%
      group_by(year, harmonised_system_code) %>%
      summarise(Value = sum(total_exports_nzd_fob, na.rm = T)) %>%
      ungroup()
    
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
      mutate(Percentage = Value / Total * 100)
    
    # 创建金额图
    p1 <- ggplot(top10_data_value, aes(x = year, y = Value/1e6, color = harmonised_system_code, group = harmonised_system_code)) +
      geom_line(linewidth = 1) +
      labs(
        x = "年", 
        y = "进口金额 VFD (百万 NZD)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")  # 移除图例
    
    # 创建比例图
    p2 <- ggplot(top10_data_percent, aes(x = year, y = Percentage, color = harmonised_system_code, group = harmonised_system_code)) +
      geom_line(linewidth = 1) +
      labs(
        x = "年", 
        y = "占总进口比例 (%)"
      ) +
      theme_minimal() 
    
    # 转换为plotly对象
    fig1 <- ggplotly(p1) 
    fig2 <- ggplotly(p2)
    
    # 创建子图并排列，使用共享的X轴
    subplot(style(fig1, showlegend = F), fig2 ,nrows = 1, shareY = F, titleX = T, titleY=T, shareX = T) %>%
      layout(showlegend = TRUE)
  })
  
  # 第四部分是同比，环比，加入 hover 信息
  # 计算进口同比增长率
  output$import_yoy_growth <- renderPlotly({
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
        x = "日期",
        y = "同比增长率 (%)"
      ) +
      theme_minimal()
    
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
        x = "日期",
        y = "环比增长率 (%)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 计算出口同比增长率
  output$export_yoy_growth <- renderPlotly({
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
        x = "日期",
        y = "同比增长率 (%)"
      ) +
      theme_minimal()
    
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
        x = "日期",
        y = "环比增长率 (%)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  # 第二个 tab 是商品分析，先整体的树形图可以选择hscode，按照0 2 4 5，按照时间分布的占进口的比例，有 sankey plot，地图展示, 前十国家的进出口和比例，
  # sankey plot
  unique_hs2code <- sort(unique(c(imports_raw$hs2_group, exports_raw$hs2_group)))
  unique_continent <- sort(unique(c(imports_raw$continent, exports_raw$continent)))

  # 准备进口桑基图数据
  output$continent_import_sankey <- renderPlotly({
    data_reactive()$continent_import_sankey %>%
      plot_ly(
        type = "sankey",
        orientation = "h",
        node = list(
          label = c(unique_hs2code, unique_continent)
        ),
        link = list(
          source = as.numeric(factor(.$hs2_group,level = unique_hs2code)) - 1,
          target = as.numeric(factor(.$continent,level = unique_continent)) - 1 + length(unique_hs2code),
          value = .$Value
        )
      )%>%
        layout(title = "Imports Flow: HS Chapters to Continent")
  })

  
  # 准备出口桑基图数据
  output$continent_export_sankey <- renderPlotly({
    data_reactive()$continent_export_sankey %>%
      plot_ly(
        type = "sankey",
        orientation = "h",
        node = list(
          label = c(unique_hs2code, unique_continent)
        ),
        link = list(
          source = as.numeric(factor(.$hs2_group,level = unique_hs2code)) - 1,
          target = as.numeric(factor(.$continent,level = unique_continent)) - 1 + length(unique_hs2code),
          value = .$Value
        )
      )%>%
      layout(title = "Exports Flow: HS Chapters to Continent")
  })
  
  # 第二部分是树形框，展示比例，加入 hover 信息，
  output$export_treemap <- renderPlot({
    data_reactive()$exports_data %>%
      group_by(hs2_group, hs2) %>%
      summarise(total = sum(total_exports_nzd_fob)) %>%
      ggplot(aes(area = total, fill = hs2_group,
               label = hs2, subgroup = hs2_group)) +
      geom_treemap() +
      geom_treemap_subgroup_border() +
      geom_treemap_text(colour = "white") +
      scale_fill_viridis_d()
  })
  
  output$import_treemap <- renderPlot({
    data_reactive()$imports_data %>%
      group_by(hs2_group, hs2) %>%
      summarise(total = sum(imports_nzd_vfd)) %>%
      ggplot(aes(area = total, fill = hs2_group,
                 label = hs2, subgroup = hs2_group)) +
      geom_treemap() +
      geom_treemap_subgroup_border() +
      geom_treemap_text(colour = "white") +
      scale_fill_viridis_d()
  })
  # TODO: 加入 hover，加入多层级 
  
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
    
    # 添加"全部"选项
    hs2_groups <- c("全部" = "ALL", hs2_groups)
    
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
    
    # 添加"全部"选项
    hs2_codes <- c("全部" = "ALL", hs2_codes)
    
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
    
    # 添加"全部"选项
    hs4_codes <- c("全部" = "ALL", hs4_codes)
    
    # 更新选择框
    updateSelectInput(session, "hs4_select", 
                      choices = hs4_codes,
                      selected = "ALL")
  })
  
  # 当HS4码改变时，更新HS5选择框
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
      hs5_codes <- sort(unique(c(
        imports_filtered %>% filter(hs4 == input$hs4_select) %>% pull(hs5),
        exports_filtered %>% filter(hs4 == input$hs4_select) %>% pull(hs5)
      )))
    } else {
      # 如果HS4是"全部"，则显示所有符合前面筛选条件的HS5
      hs5_codes <- sort(unique(c(
        imports_filtered %>% pull(hs5),
        exports_filtered %>% pull(hs5)
      )))
    }
    
    if(input$hs5_select != "ALL") {
      hscodes <- sort(unique(c(
        imports_filtered %>% filter(hs5 == input$hs5_select) %>% pull(hs5),
        exports_filtered %>% filter(hs5 == input$hs5_select) %>% pull(hs5)
      )))
    } else {
      # 如果HS5是"全部"，则显示所有符合前面筛选条件的HSCode
      hscodes <- sort(unique(c(
        imports_filtered %>% pull(hs5),
        exports_filtered %>% pull(hs5)
      )))
    }
    
    # 过滤掉NA值
    hs5_codes <- hs5_codes[!is.na(hs5_codes)]
    
    # 添加"全部"选项
    hs5_codes <- c("全部" = "ALL", hs5_codes)
    
    # 更新选择框
    updateSelectInput(session, "hs5_select", 
                      choices = hs5_codes,
                      selected = "ALL")
  })
  # 当HS5码改变时，更新HSCode选择框
  observeEvent(input$hs5_select, {
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
    
    # 应用HS5筛选
    if(input$hs5_select != "ALL") {
      hscodes <- sort(unique(c(
        imports_filtered %>% filter(hs5 == input$hs5_select) %>% pull(harmonised_system_code),
        exports_filtered %>% filter(hs5 == input$hs5_select) %>% pull(harmonised_system_code)
      )))
    } else {
      # 如果HS5是"全部"，则显示所有符合前面筛选条件的HSCode
      hscodes <- sort(unique(c(
        imports_filtered %>% pull(harmonised_system_code),
        exports_filtered %>% pull(harmonised_system_code)
      )))
    }
    
    # 过滤掉NA值
    hscodes <- hscodes[!is.na(hscodes)]
    
    # 添加"全部"选项
    hscodes <- c("全部" = "ALL", hscodes)
    
    # 更新选择框
    updateSelectInput(session, "hscode_select", 
                      choices = hscodes,
                      selected = "ALL")
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
    
    # 根据选择的HS5码筛选
    if(input$hs5_select != "ALL") {
      imports_filtered <- imports_filtered %>% filter(hs5 == input$hs5_select)
      exports_filtered <- exports_filtered %>% filter(hs5 == input$hs5_select)
    }
    
    # 根据选择的HSCode筛选
    if(input$hscode_select != "ALL") {
      imports_filtered <- imports_filtered %>% filter(hscode == input$hscode_select)
      exports_filtered <- exports_filtered %>% filter(hscode == input$hscode_select)
    }
    
    return(list(imports = imports_filtered, exports = exports_filtered))
  }
  
  output$current_hs_trend <- renderPlotly({
    # 获取筛选后的数据
    filtered_data <- build_hs_filter(data_reactive()$imports_data, data_reactive()$exports_data)
    
    # 构建标题
    title_parts <- c()
    if(input$hs2_group_select != "ALL") title_parts <- c(title_parts, paste0("HS2分组: ", input$hs2_group_select))
    if(input$hs2_select != "ALL") title_parts <- c(title_parts, paste0("HS2: ", input$hs2_select))
    if(input$hs4_select != "ALL") title_parts <- c(title_parts, paste0("HS4: ", input$hs4_select))
    if(input$hs5_select != "ALL") title_parts <- c(title_parts, paste0("HS5: ", input$hs5_select))
    if(input$hscode_select != "ALL") title_parts <- c(title_parts, paste0("HSCode: ", input$hscode_select))
    
    title <- if(length(title_parts) > 0) paste(title_parts, collapse = ", ") else "所有商品"
    
    # 按月份汇总进出口数据
    import_data <- filtered_data$imports %>%
      group_by(month) %>%
      summarise(value = sum(imports_nzd_vfd, na.rm = TRUE)) %>%
      mutate(type = "进口")
    
    export_data <- filtered_data$exports %>%
      group_by(month) %>%
      summarise(value = sum(total_exports_nzd_fob, na.rm = TRUE)) %>%
      mutate(type = "出口")
    
    # 合并数据
    combined_data <- bind_rows(import_data, export_data)
    
    # 创建图表
    p <- ggplot(combined_data, aes(x = month, y = value/1e6, color = type, group = type)) +
      geom_line(linewidth = 1) +
      labs(
        title = paste0(title, " - 进出口价值"),
        x = "月份",
        y = "金额 (百万 NZD)",
        color = "类型"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # 当前HS码的前十交易国家
  output$current_hs_top10_countries <- renderPlotly({
    # 获取筛选后的数据
    filtered_data <- build_hs_filter(data_reactive()$imports_data, data_reactive()$exports_data)
    
    # 获取前10名进口国家
    top10_import_countries <- filtered_data$imports %>%
      group_by(country) %>%
      summarise(total_import = sum(imports_nzd_vfd, na.rm = TRUE)) %>%
      arrange(desc(total_import)) %>%
      slice_head(n = 10) %>%
      mutate(type = "进口")
    
    # 获取前10名出口国家
    top10_export_countries <- filtered_data$exports %>%
      group_by(country) %>%
      summarise(total_export = sum(total_exports_nzd_fob, na.rm = TRUE)) %>%
      arrange(desc(total_export)) %>%
      slice_head(n = 10) %>%
      mutate(type = "出口") %>%
      rename(total_import = total_export)  # 为了合并数据，统一列名
    
    # 合并数据
    combined_data <- bind_rows(top10_import_countries, top10_export_countries)
    
    # 创建图表
    p <- ggplot(combined_data, aes(x = reorder(country, total_import), y = total_import/1e6, fill = type)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      labs(
        x = "国家",
        y = "金额 (百万 NZD)",
        fill = "类型"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # 获取当前选择的下一级别
  get_next_level <- reactive({
    if(input$hscode_select != "ALL") {
      return(NULL)  # 已经是最底层，没有下一级
    } else if(input$hs5_select != "ALL") {
      return(list(level = "hscode", parent_level = "hs5", parent_value = input$hs5_select))
    } else if(input$hs4_select != "ALL") {
      return(list(level = "hs5", parent_level = "hs4", parent_value = input$hs4_select))
    } else if(input$hs2_select != "ALL") {
      return(list(level = "hs4", parent_level = "hs2", parent_value = input$hs2_select))
    } else if(input$hs2_group_select != "ALL") {
      return(list(level = "hs2", parent_level = "hs2_group", parent_value = input$hs2_group_select))
    } else {
      return(list(level = "hs2_group", parent_level = NULL, parent_value = NULL))
    }
  })
  
  # 下级分类进口走势
  output$sublevel_import_trend <- renderPlotly({
    # 获取下一级别
    next_level <- get_next_level()
    
    # 如果没有下一级，显示空图表
    if(is.null(next_level)) {
      return(plot_ly() %>% 
               layout(title = "已经是最底层分类，无下级数据",
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
    }
    
    # 使用当前筛选条件构建基础数据
    filtered_imports <- data_reactive()$imports_data
    
    # 应用当前的筛选链
    if(input$hs2_group_select != "ALL") {
      filtered_imports <- filtered_imports %>% filter(hs2_group == input$hs2_group_select)
    }
    if(input$hs2_select != "ALL") {
      filtered_imports <- filtered_imports %>% filter(hs2 == input$hs2_select)
    }
    if(input$hs4_select != "ALL") {
      filtered_imports <- filtered_imports %>% filter(hs4 == input$hs4_select)
    }
    if(input$hs5_select != "ALL") {
      filtered_imports <- filtered_imports %>% filter(hs5 == input$hs5_select)
    }
    
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
    
    # 准备按年和下一级分组的数据
    agg_data <- filtered_imports %>%
      group_by(year, !!sym(next_level$level)) %>%
      summarise(value = sum(imports_nzd_vfd, na.rm = TRUE)) %>%
      ungroup()
    
    # 创建图表
    p <- ggplot(agg_data, aes(x = year, y = value/1e6, color = !!sym(next_level$level), group = !!sym(next_level$level))) +
      geom_line(linewidth = 1) +
      labs(
        title = paste0("下级分类 (", next_level$level, ") 进口走势"),
        x = "月份",
        y = "进口金额 (百万 NZD)",
        color = next_level$level
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # 下级分类出口走势
  output$sublevel_export_trend <- renderPlotly({
    # 获取下一级别
    next_level <- get_next_level()
    
    # 如果没有下一级，显示空图表
    if(is.null(next_level)) {
      return(plot_ly() %>% 
               layout(title = "已经是最底层分类，无下级数据",
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
    }
    
    # 使用当前筛选条件构建基础数据
    filtered_exports <- data_reactive()$exports_data
    
    # 应用当前的筛选链
    if(input$hs2_group_select != "ALL") {
      filtered_exports <- filtered_exports %>% filter(hs2_group == input$hs2_group_select)
    }
    if(input$hs2_select != "ALL") {
      filtered_exports <- filtered_exports %>% filter(hs2 == input$hs2_select)
    }
    if(input$hs4_select != "ALL") {
      filtered_exports <- filtered_exports %>% filter(hs4 == input$hs4_select)
    }
    if(input$hs5_select != "ALL") {
      filtered_exports <- filtered_exports %>% filter(hs5 == input$hs5_select)
    }
    
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
      group_by(year, !!sym(next_level$level)) %>%
      summarise(value = sum(total_exports_nzd_fob, na.rm = TRUE)) %>%
      ungroup()
    
    # 创建图表
    p <- ggplot(agg_data, aes(x = year, y = value/1e6, color = !!sym(next_level$level), group = !!sym(next_level$level))) +
      geom_line(linewidth = 1) +
      labs(
        title = paste0("下级分类 (", next_level$level, ") 出口走势"),
        x = "月份",
        y = "出口金额 (百万 NZD)",
        color = next_level$level
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
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
