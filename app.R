# ---- Install & load required packages ----
required_pkgs <- c(
  "shiny",
  "shinyFiles",
  "shinycssloaders",
  "data.table",
  "openxlsx"
)

install_if_missing <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(to_install) > 0) {
    install.packages(
      to_install,
      dependencies = TRUE,
      repos = "https://cloud.r-project.org"
    )
  }
}

load_packages <- function(pkgs) {
  for (p in pkgs) {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }
}

install_if_missing(required_pkgs)
load_packages(required_pkgs)

# =====================================================================
# 1. Dynamic Filter Module (UI)
# =====================================================================
filterInputUI <- function(id, label) {
  ns <- NS(id)
  selectizeInput(
    ns("value"),
    label,
    choices = NULL,
    multiple = TRUE,
    options = list(
      plugins = list("remove_button"),
      placeholder = paste("Select", label),
      persist = TRUE
    )
  )
}

# =====================================================================
# 2. Dynamic Filter Module (Server)
# =====================================================================
filterInputServer <- function(id, data, column) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      req(data())
      col_data <- data()[[column]]
      choices <- c("All", sort(unique(col_data)))
      
      updateSelectizeInput(
        session, "value",
        choices = choices,
        selected = "All",
        server = TRUE
      )
    })
    
    reactive({
      req(data())
      if ("All" %in% input$value) unique(data()[[column]])
      else input$value
    })
  })
}

# =====================================================================
# 3. User Interface
# =====================================================================
ui <- fluidPage(
  
  titlePanel("ISMC Data Export"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # --- PSP or Non-PSP ---
      radioButtons(
        "psp", "PSP or Non-PSP",
        choices = c("nonPSP", "PSP"),
        selected = "nonPSP",
        inline = TRUE
      ),
      
      # --- Compilation Date ---
      selectizeInput(
        "compdate",
        "Compilation Date",
        choices = NULL,
        multiple = FALSE
      ),
      
      # --- Dynamic Filters (Modules) ---
      filterInputUI("sampltype", "Sample Type"),
      filterInputUI("tsa",       "TSA"),
      filterInputUI("mgmtunit",  "Management Unit"),
      filterInputUI("ownership", "Ownership"),
      filterInputUI("sitecode",  "Site Purpose"),
      
      selectizeInput( 
        "ysm", 
        "YSM", 
        choices = c("All", "YSM Pilot only"), selected = "All",
        multiple = F,
        options = list('plugins' = list('remove_button'), 
                       placeholder = 'Select YSM', 'persist' = TRUE)
      ),
      
      sliderInput("year", "Year Range", min = 1900, max = 2025,
                  value = c(1900, 2025), step = 1, sep = ""),
      
      # Export button + Download link
      actionButton("db", "Export Data", width = "100%"),
      # Optional: text output to show download ready
      #textOutput("download_status") %>% withSpinner(type = 6, color = "#0072B2")
      tags$head(tags$script(HTML('
         Shiny.addCustomMessageHandler("jsCode",
           function(message) { eval(message.value); });
      '))),
      downloadLink("downloadCSV", label="")
    ),
    
    mainPanel(
      tableOutput("setting"),
      #textOutput("samplesize"),
      tableOutput("table1"),
      tableOutput("table2")
    )
  )
)

# =====================================================================
# 4. Server Logic
# =====================================================================
server <- function(input, output, session) {
  
  # -------------------------------------------
  # PSP / non-PSP + Compilation Dates
  # -------------------------------------------
  datapath <- "//objectstore3.nrs.bcgov/s164/S63016/!Workgrp/Inventory/Compilation/ismc/forpublish"
  
  pspornot <- reactive({
    if (input$psp == "PSP") "PSP" else "nonPSP"
  })
  
  compdate_list <- reactive({
    if (input$psp == "PSP") {
      as.numeric(gsub("PSP_", "", dir(datapath, pattern="^PSP_", recursive=FALSE)))
    } else {
      as.numeric(gsub("nonPSP_", "", dir(datapath, pattern="^nonPSP_", recursive=FALSE)))
    }
  })
  
  latest_compdate <- reactive({ max(compdate_list()) })
  
  # Update compilation date selector when PSP changes
  observeEvent(input$psp, {
    freezeReactiveValue(input, "compdate")
    
    choices <- if (input$psp == "PSP") {
      c("Latest", compdate_list())
    } else {
      c("Latest", compdate_list())
    }
    
    updateSelectizeInput(session, "compdate", choices = choices, selected = "Latest")
  })
  
  compdate <- reactive({
    if (input$compdate != "Latest") input$compdate
    else latest_compdate()
  })
  
  filepath <- reactive({
    paste0(datapath, "/", pspornot(), "_", compdate())
  })
  
  # -------------------------------------------
  # Load samples & header from selected path
  # -------------------------------------------
  samples <- reactive({
    fread(file.path(filepath(), "faib_sample_byvisit.csv"))
  })
  
  sites <- reactive({
    fread(file.path(filepath(), "faib_header.csv"))
  })
  
  sample_site <- reactive({
    merge(
      samples(),
      sites()[, !"SAMPLE_ESTABLISHMENT_TYPE"],
      by = "SITE_IDENTIFIER"
    )
  })
  
  # -------------------------------------------
  # Filters (Modules)
  # -------------------------------------------
  sampltype <- filterInputServer("sampltype", sample_site, "SAMPLE_ESTABLISHMENT_TYPE")
  tsa       <- filterInputServer("tsa",       sample_site, "TSA_DESC")
  mgmtunit  <- filterInputServer("mgmtunit",  sample_site, "MGMT_UNIT")
  ownership <- filterInputServer("ownership", sample_site, "OWN_SCHED_DESCRIP")
  sitecode  <- filterInputServer("sitecode",  sample_site, "SAMPLE_SITE_PURPOSE_TYPE_CODE")
  
  # YSM filter
  ysm <- reactive({
    if ("All" %in% input$ysm) c("Y", "N", "", NA) else "Y"
  })
  year <- reactive(input$year)
  
  # -------------------------------------------
  # Selected samples
  # -------------------------------------------
  selected_sample <- reactive({
    ss <- sample_site()
    
    ss[
      SAMPLE_ESTABLISHMENT_TYPE %in% sampltype() &
        TSA_DESC %in% tsa() &
        MGMT_UNIT %in% mgmtunit() &
        OWN_SCHED_DESCRIP %in% ownership() &
        SAMPLE_SITE_PURPOSE_TYPE_CODE %in% sitecode() &
        MEAS_YR >= input$year[1] &
        MEAS_YR <= input$year[2] &
        YSM_PILOT_FM %in% ysm(),
      CLSTR_ID
    ]
  })
  
  selected_site <- reactive({
    sample_site <- sample_site()
    sample_site[sample_site$SAMPLE_ESTABLISHMENT_TYPE %in% sampltype() &
                  sample_site$TSA_DESC %in% tsa() &
                  sample_site$MGMT_UNIT %in% mgmtunit() &
                  sample_site$OWN_SCHED_DESCRIP %in% ownership() &
                  sample_site$MEAS_YR >= year()[1] &
                  sample_site$MEAS_YR <= year()[2] &
                  sample_site$SAMPLE_SITE_PURPOSE_TYPE_CODE %in% sitecode() &
                  (sample_site$YSM_PILOT_FM %in% ysm() | sample_site$YSM_PILOT_LM %in% ysm())
                , SITE_IDENTIFIER]
  })
  
  # -------------------------------------------
  # Example Outputs
  # -------------------------------------------
  #output$setting <- renderTable({
  #  data.frame(
  #    PSP_or_nonPSP = pspornot(),
  #    CompilationDate = compdate(),
  #    Path = filepath()
  #  )
  #})
  
  output$setting <- renderTable({
    data.frame(Data = c("Location", "PSP or non-PSP", 
                        "Compilation Date"
    ),
    Source = c(paste(datapath), paste(pspornot()), 
               paste(compdate())
    ))
  })
  
  #output$samplesize <- renderText({
  #  paste("Selected sample size:", length(selected_sample()))
  #})
  
  output$table1 <- renderTable({
    
    # Helper to collapse input safely
    collapse_safe <- function(x, all_values = NULL, sep = ", ") {
      if (is.null(x) || length(x) == 0) return("")
      if (!is.null(all_values) && length(x) == length(all_values)) return("All")
      paste(x, collapse = sep)
    }
    
    data.frame(
      Field = c(
        "SAMPLE_ESTABLISHMENT_TYPE", 
        "TSA_DESC",
        "MGMT_UNIT",
        "OWN_SCHED_DESCRIP",
        "MEAS_YR",
        "SAMPLE_SITE_PURPOSE_TYPE_CODE",
        "YSM"
      ),
      #Input = c(
      #  collapse_safe(input$sampltype),
      #  collapse_safe(input$tsa),
      #  collapse_safe(input$mgmtunit),
      #  collapse_safe(input$ownership),
      #  collapse_safe(input$year, sep = "-"),
      #  collapse_safe(input$sitecode, sep = "-"),
      #  collapse_safe(input$ysm)
      #),
      Input = c(
        collapse_safe(sampltype(), all_values = unique(sample_site()$SAMPLE_ESTABLISHMENT_TYPE)),
        collapse_safe(tsa(), all_values = unique(sample_site()$TSA_DESC)),
        collapse_safe(mgmtunit(), all_values = unique(sample_site()$MGMT_UNIT)),
        collapse_safe(ownership(), all_values = unique(sample_site()$OWN_SCHED_DESCRIP)),
        paste(year(), collapse = "-"),   # year usually no "All"
        collapse_safe(sitecode(), all_values = unique(sample_site()$SAMPLE_SITE_PURPOSE_TYPE_CODE)),
        ifelse(all(ysm() == "Y"), "YSM Pilot only", "All")
      ),
      stringsAsFactors = FALSE
    )
  })
  
  
  output$table2 <- renderTable({
    data.frame(Name = c("Total sites:", "Selected sites:", 
                        "Total samples:", "Selected samples:"),
               Number = c(nrow(sites()), length(unique(selected_site())), 
                          nrow(samples()), length(selected_sample()))
    )
  })
  
  #output$table1 <- renderTable({
  #  head(samples(), 10)
  #})
  #
  #output$table2 <- renderTable({
  #  head(sites(), 10)
  #})
  
  # -------------------------------------------
  # Download handler (CSV)
  # -------------------------------------------
  #observeEvent(input$db, {
  #  
  #  summary <- fread(paste0(filepath(), "/faib_compiled_smries.csv"))
  #  summary_ht <- fread(paste0(filepath(), "/faib_compiled_smries_ht.csv"))
  #  summary_wk <- fread(paste0(filepath(), "/faib_compiled_smries_wk.csv"))
  #  summary_spc <- fread(paste0(filepath(), "/faib_compiled_spcsmries.csv"))
  #  summary_siteage <- fread(paste0(filepath(), "/faib_compiled_spcsmries_siteage.csv"))
  #  summary_spc_wk <- fread(paste0(filepath(), "/faib_compiled_spcsmries_wk.csv"))
  #  plot_header <- fread(paste0(filepath(), "/faib_plot_header.csv"))
  #  tree <- fread(paste0(filepath(), "/faib_tree_detail.csv"))
  #  
  #  wb <- loadWorkbook(paste0(filepath(), "/data_dictionary.xlsx"))
  #  
  #  #Create CSVs to download
  #  output$downloadCSV <<- downloadHandler(
  #    
  #    
  #    filename = paste("bc_custom_sample_data_", Sys.Date(), ".zip", sep=""),
  #    content = function(fname) {
  #      fs <- c("faib_sample_byvisit.csv", "faib_header.csv",
  #              "faib_compiled_smries.csv", "faib_compiled_smries_ht.csv",
  #              "faib_compiled_smries_wk.csv", "faib_compiled_spcsmries.csv",
  #              "faib_compiled_spcsmries_siteage.csv", "faib_compiled_spcsmries_wk.csv",
  #              "faib_plot_header.csv", "faib_tree_detail.csv",
  #              "data_dictionary.xlsx")
  #      
  #      fwrite(samples()[CLSTR_ID %in% selected_sample(),], file = "faib_sample_byvisit.csv")
  #      fwrite(sites()[SITE_IDENTIFIER %in% selected_site(),], file = "faib_header.csv")
  #      fwrite(summary[SITE_IDENTIFIER %in% selected_site(),], file = "faib_compiled_smries.csv")
  #      fwrite(summary_ht[CLSTR_ID %in% selected_sample(),], file = "faib_compiled_smries_ht.csv")
  #      fwrite(summary_wk[CLSTR_ID %in% selected_sample(),], file = "faib_compiled_smries_wk.csv")
  #      fwrite(summary_spc[CLSTR_ID %in% selected_sample(),], file = "faib_compiled_spcsmries.csv")
  #      fwrite(summary_siteage[CLSTR_ID %in% selected_sample(),], file = "faib_compiled_spcsmries_siteage.csv")
  #      fwrite(summary_spc_wk[CLSTR_ID %in% selected_sample(),], file = "faib_compiled_spcsmries_wk.csv")
  #      fwrite(plot_header[CLSTR_ID %in% selected_sample(),], file = "faib_plot_header.csv")
  #      fwrite(tree[CLSTR_ID %in% selected_sample(),], file = "faib_tree_detail.csv")
  #      saveWorkbook(wb, file = "data_dictionary.xlsx", overwrite = TRUE)
  #      
  #      zip(zipfile=fname, files=fs)
  #      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
  #    },
  #    contentType = "application/zip")
  #  # ------------------------
  #  # Trigger automatic download via JS
  #  # ------------------------
  #  jsinject <- "setTimeout(function(){window.open($('#downloadCSV').attr('href'))}, 100);"
  #  session$sendCustomMessage(type = 'jsCode', list(value = jsinject))
  #  
  #})
  
  observeEvent(input$db, {
    
    # Show progress
    withProgress(message = "Preparing export files...", value = 0, {
      
      incProgress(0.1, detail = "Loading summary tables")
      summary       <- fread(file.path(filepath(), "faib_compiled_smries.csv"))
      summary_ht    <- fread(file.path(filepath(), "faib_compiled_smries_ht.csv"))
      #summary_wk    <- fread(file.path(filepath(), "faib_compiled_smries_wk.csv"))
      summary_spc   <- fread(file.path(filepath(), "faib_compiled_spcsmries.csv"))
      summary_siteage <- fread(file.path(filepath(), "faib_compiled_spcsmries_siteage.csv"))
      #summary_spc_wk <- fread(file.path(filepath(), "faib_compiled_spcsmries_wk.csv"))
      plot_header   <- fread(file.path(filepath(), "faib_plot_header.csv"))
      tree          <- fread(file.path(filepath(), "faib_tree_detail.csv"))
      wb <- openxlsx::loadWorkbook(file.path(filepath(), "data_dictionary.xlsx"))
      
      if (pspornot() != "PSP") {
        summary_wk    <- fread(file.path(filepath(), "faib_compiled_smries_wk.csv"))
        summary_spc_wk <- fread(file.path(filepath(), "faib_compiled_spcsmries_wk.csv"))
      }
      
      incProgress(0.3, detail = "Writing temporary CSV files")
      #tmp_files <- list(
      #  "faib_sample_byvisit.csv" = samples()[CLSTR_ID %in% selected_sample(), ],
      #  "faib_header.csv" = sites()[SITE_IDENTIFIER %in% selected_site(), ],
      #  "faib_compiled_smries.csv" = summary[SITE_IDENTIFIER %in% selected_site(), ],
      #  "faib_compiled_smries_ht.csv" = summary_ht[CLSTR_ID %in% selected_sample(), ],
      #  "faib_compiled_smries_wk.csv" = summary_wk[CLSTR_ID %in% selected_sample(), ],
      #  "faib_compiled_spcsmries.csv" = summary_spc[CLSTR_ID %in% selected_sample(), ],
      #  "faib_compiled_spcsmries_siteage.csv" = summary_siteage[CLSTR_ID %in% selected_sample(), ],
      #  "faib_compiled_spcsmries_wk.csv" = summary_spc_wk[CLSTR_ID %in% selected_sample(), ],
      #  "faib_plot_header.csv" = plot_header[CLSTR_ID %in% selected_sample(), ],
      #  "faib_tree_detail.csv" = tree[CLSTR_ID %in% selected_sample(), ]
      #)
      
      tmp_files <- list(
        "faib_sample_byvisit.csv" = samples()[CLSTR_ID %in% selected_sample(), ],
        "faib_header.csv" = sites()[SITE_IDENTIFIER %in% selected_site(), ],
        "faib_compiled_smries.csv" = summary[SITE_IDENTIFIER %in% selected_site(), ],
        "faib_compiled_smries_ht.csv" = summary_ht[CLSTR_ID %in% selected_sample(), ],
        "faib_compiled_spcsmries.csv" = summary_spc[CLSTR_ID %in% selected_sample(), ],
        "faib_compiled_spcsmries_siteage.csv" = summary_siteage[CLSTR_ID %in% selected_sample(), ],
        "faib_plot_header.csv" = plot_header[CLSTR_ID %in% selected_sample(), ],
        "faib_tree_detail.csv" = tree[CLSTR_ID %in% selected_sample(), ]
      )
      
      # Conditionally add weekly summaries if not PSP
      if (pspornot() != "PSP") {
        tmp_files[["faib_compiled_smries_wk.csv"]] <- summary_wk[CLSTR_ID %in% selected_sample(), ]
        tmp_files[["faib_compiled_spcsmries_wk.csv"]] <- summary_spc_wk[CLSTR_ID %in% selected_sample(), ]
      }
      
      # Write CSVs
      for(f in names(tmp_files)) {
        fwrite(tmp_files[[f]], file = f)
      }
      
      # Save Excel workbook
      saveWorkbook(wb, file = "data_dictionary.xlsx", overwrite = T)
      
      incProgress(0.6, detail = "Zipping files")
      files_to_zip <- c(names(tmp_files), "data_dictionary.xlsx")
      zipfile <- paste0(tempfile(), ".zip")
      zip(zipfile = zipfile, files = files_to_zip, extras = "-j")  # -j flattens structure
      
      # Remove temporary files
      #file_remove(files_to_zip)
      
      incProgress(1, detail = "Ready for download")
      
      # Trigger download
      output$downloadCSV <- downloadHandler(
        filename = paste0("bc_custom_sample_data_", Sys.Date(), ".zip"),
        content = function(fname) {
          file.copy(zipfile, fname)
        },
        contentType = "application/zip"
      )
      
      # Update UI message
      output$download_status <- renderText("Click the link below to download:")
      
      # Optional: auto-click download link using JS
      jsinject <- "setTimeout(function(){window.open($('#downloadCSV').attr('href'))}, 100);"
      session$sendCustomMessage(type = 'jsCode', list(value = jsinject))
    })
  })
  
}

# =====================================================================
# 5. Run App
# =====================================================================
shinyApp(ui, server)