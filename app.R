library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)

# Area and time above threshold functions
compute_area_above_threshold <- function(time_vec, value_vec, threshold) {
  n <- length(time_vec)
  total_area <- 0
  if (n < 2) return(0)
  for (i in 1:(n - 1)) {
    t1 <- time_vec[i]; t2 <- time_vec[i + 1]; dt <- t2 - t1
    x1 <- value_vec[i]; x2 <- value_vec[i + 1]
    if (is.na(x1) || is.na(x2)) next
    if (x1 > threshold && x2 > threshold) {
      total_area <- total_area + dt * ((x1 - threshold + x2 - threshold) / 2)
    } else if (x1 > threshold && x2 <= threshold) {
      t_cross <- t1 + (x1 - threshold) / (x1 - x2) * dt
      area <- (t_cross - t1) * (x1 - threshold) / 2
      total_area <- total_area + area
    } else if (x1 <= threshold && x2 > threshold) {
      t_cross <- t1 + (threshold - x1) / (x2 - x1) * dt
      area <- (t2 - t_cross) * (x2 - threshold) / 2
      total_area <- total_area + area
    }
  }
  return(total_area)
}

compute_time_above_threshold <- function(time_vec, value_vec, threshold) {
  n <- length(time_vec)
  total_time <- 0
  if (n < 2) return(0)
  for (i in 1:(n - 1)) {
    t1 <- time_vec[i]; t2 <- time_vec[i + 1]; dt <- t2 - t1
    x1 <- value_vec[i]; x2 <- value_vec[i + 1]
    if (is.na(x1) || is.na(x2)) next
    if (x1 > threshold && x2 > threshold) {
      total_time <- total_time + dt
    } else if (x1 > threshold && x2 <= threshold) {
      t_cross <- t1 + (x1 - threshold) / (x1 - x2) * dt
      total_time <- total_time + (t_cross - t1)
    } else if (x1 <= threshold && x2 > threshold) {
      t_cross <- t1 + (threshold - x1) / (x2 - x1) * dt
      total_time <- total_time + (t2 - t_cross)
    }
  }
  return(total_time)
}

# UI
ui <- fluidPage(
  titlePanel("RMSI Calculator for Myocardial Stress (DCD)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      helpText("CSV must include: time, HR, SBP, DBP, SaO2")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", tableOutput("calculated_table")),
        tabPanel("RMSI Curve", plotOutput("rmsi_plot")),
        tabPanel("Summary Metrics", verbatimTextOutput("metrics"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  data_input <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    
    df <- df %>%
      mutate(
        RPP = HR * SBP,
        rr_sec = 60 / HR,
        tsyst = 0.35 - 0.001 * (HR - 60),
        tdiast = pmax(rr_sec - tsyst, 0),
        diastolic_frac = if_else(HR > 0, tdiast / rr_sec, 0),
        Perfusion = DBP * (SaO2 / 100) * diastolic_frac,
        RPP_z = (RPP - 8831.1) / 6501.861,
        Perf_z = (Perfusion - 9.94) / 12.36,
        MSI = if_else(is.finite(RPP) & is.finite(Perfusion) & RPP > 0 & Perfusion > 0,
                      RPP_z - Perf_z,
                      NA_real_)
      )
    
    baseline_msi <- df$MSI[1]
    df <- df %>%
      mutate(RMSI = if_else(!is.na(MSI),
                            ((MSI - baseline_msi) / abs(baseline_msi)) * 100,
                            NA_real_)) %>%
      select(time, HR, SBP, DBP, SaO2, RPP, Perfusion, MSI, RMSI)  # Keep only final columns
    
    return(df)
  })
  
  output$calculated_table <- renderTable({
    head(data_input(), 20)
  })
  
  output$rmsi_plot <- renderPlot({
    df <- data_input()
    ggplot(df, aes(x = time, y = RMSI)) +
      geom_line(color = "blue") +
      geom_hline(yintercept = c(50, 100), linetype = "dashed", color = "red") +
      labs(title = "Relative Myocardial Stress Index (RMSI)",
           x = "Time (minutes)", y = "RMSI (%)") +
      theme_minimal()
  })
  
  output$metrics <- renderPrint({
    df <- data_input()
    
    df_filtered <- df %>%
      filter(!is.na(RMSI) & is.finite(RMSI) & !is.na(time) & is.finite(time))
    
    avg_rmsi <- if (nrow(df_filtered) > 0) {
      mean(df_filtered$RMSI, na.rm = TRUE)
    } else {
      NA
    }
    
    auc_rmsi <- if (nrow(df_filtered) > 1) {
      sum(diff(df_filtered$time) * zoo::rollmean(df_filtered$RMSI, 2, fill = NA), na.rm = TRUE)
    } else {
      NA
    }
    
    time_above_50 <- compute_time_above_threshold(df_filtered$time, df_filtered$RMSI, 50)
    area_above_50 <- compute_area_above_threshold(df_filtered$time, df_filtered$RMSI, 50)
    
    time_above_100 <- compute_time_above_threshold(df_filtered$time, df_filtered$RMSI, 100)
    area_above_100 <- compute_area_above_threshold(df_filtered$time, df_filtered$RMSI, 100)
    
    df_10 <- df_filtered[df_filtered$time <= 10, ]
    slope <- if (nrow(df_10) >= 2) {
      coef(lm(RMSI ~ time, data = df_10))[2]
    } else {
      NA
    }
    
    cat("Average RMSI: ", round(avg_rmsi, 2), "\n")
    cat("Cumulative AUC (approx): ", round(auc_rmsi, 2), "\n")
    cat("Time above 50% RMSI: ", round(time_above_50, 2), "min\n")
    cat("Area above 50% RMSI: ", round(area_above_50, 2), "\n")
    cat("Time above 100% RMSI: ", round(time_above_100, 2), "min\n")
    cat("Area above 100% RMSI: ", round(area_above_100, 2), "\n")
    cat("Slope of RMSI in first 10 min: ", round(slope, 4), "\n")
  })
}

shinyApp(ui = ui, server = server)
