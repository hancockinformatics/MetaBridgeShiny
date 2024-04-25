# Load packages -----------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(dplyr)
})

metabridgeTheme <- bs_theme(
  version = 5,
  preset = "flatly",
  "bslib-sidebar-bg" = "var(--bs-bg)"
)

metabridgeVersion <- gsub(
  x = grep("^Version\\: ", readLines("DESCRIPTION"), value = TRUE),
  pattern = "^Version\\: ",
  replacement = ""
)

# Dependencies
dependencyTable <- tibble(
  name = c(
    "bslib",
    "KEGG Release 109",
    "MetaCyc v27",
    "shiny",
    "shinycssloaders",
    "shinyjs",
    "tidyverse",
    "pathview"
  ),
  link = c(
    "https://rstudio.github.io/bslib/",
    "https://www.genome.jp/kegg/",
    "https://metacyc.org/",
    "https://shiny.rstudio.com/",
    "https://github.com/andrewsali/shinycssloaders",
    "https://deanattali.com/shinyjs/",
    "https://www.tidyverse.org/",
    "https://doi.org/10.1093/bioinformatics/btt285"
  ),
  description = c(
    "A modern UI toolkit for Shiny based on Bootstrap.",
    "Large database containing multiple data types.",
    "Curated database for human metabolomic data.",
    "Web application framework for R.",
    "Animated loaders for Shiny outputs.",
    "Improve the user experience of your Shiny apps in seconds.",
    "A collection of R packages designed for data science.",
    "Pathway-based data integration and visualization."
  )
)


# UI ----------------------------------------------------------------------

metabridgeUI <- page_navbar(
  id = "navbarLayout",
  window_title = "MetaBridge",
  theme = metabridgeTheme,
  bg = bs_get_variables(metabridgeTheme, "primary"),
  inverse = FALSE,
  header = tags$head(
    includeHTML("www/google_analytics.html"),
    useShinyjs(),
    tags$script(src = "js/client.js"),
    tags$link(rel = "stylesheet", href = "css/custom.css"),
    tags$link(rel = "icon", href = "img/favicon.png")
  ),
  nav_item(HTML("<img src='img/logo_white.svg' alt='M' height='28'>")),


  # |- Welcome ----------------------------------------------------------

  nav_panel(
    value = "welcomePanel",
    title = "MetaBridge",

    div(
      class = "mx-2 my-2",
      div(
        class = "p-5 bg-body-tertiary rounded-3",

        div(
          class = "logoWrapper-home",
          h1(
            class = "display-3 fw-bold text-body-emphasis",
            "Welcome"
          ),
          div(
            class = "mx-auto fs-4 text-muted",
            p(
              "Welcome to MetaBridge, a user-friendly web tool for ",
              "network-based integrative analysis of metabolomics data. Here ",
              "you can upload a list of metabolite IDs and identify the ",
              "directly interacting enzymes for network integration."
            ),
            p(
              "To start, you'll want a set of metabolites as HMDB or KEGG ",
              "IDs. We recommend",
              a(
                "MetaboAnalyst",
                href = "http://www.metaboanalyst.ca",
                target = "_blank",
                rel = "noopener noreferrer"
              ),
              "for metabolomics data processing and ID conversion, if you ",
              "have only compound names."
            ),
            p(
              "With the output of MetaBridge, you can create a ",
              "protein-protein interaction network using your metabolomics ",
              "data. We suggest",
              a(
                "NetworkAnalyst",
                href = "http://www.networkanalyst.ca",
                target = "_blank",
                rel = "noopener noreferrer"
              ),
              "for generation of these networks, and for network-based ",
              "integration with data from other omics types."
            ),
            p(
              class = "mb-4",
              "Click the button below to get started! If you'd like to learn ",
              "more about how MetaBridge can be used, check our Tutorial. For ",
              "more information, including where to report bugs or problems ",
              "and how to cite MetaBridge, please refer to the About page."
            ),

            div(
              actionButton(
                inputId = "getStarted",
                class = "btn-primary btn-lg px-4 me-md-2 disabled",
                icon = icon("circle-notch", class = "fa fa-spin"),
                label = "Loading..."
              ) %>%
                tooltip(
                  "Let's go!",
                  placement = "bottom"
                ),

              actionButton(
                inputId = "tutorial",
                class = "btn-success btn-lg px-4 me-md-2 btn-hidden",
                label = "Tutorial"
              ) %>% tooltip(
                "See how to use MetaBridge for integrative analysis",
                placement = "bottom"
              ),

              actionButton(
                inputId = "about",
                class = "btn-info btn-lg px-4 me-md-2 btn-hidden",
                label = "About"
              ) %>%
                tooltip(
                  "Learn more about MetaBridge",
                  placement = "bottom"
                )
            )
          )
        )
      ),
      div(
        style = "position:fixed; bottom:0px; padding-bottom:10px",
        HTML(
          "<a href='http://cmdr.ubc.ca/bobh/'",
          "target='blank' rel='noopener noreferrer'>",
          "<img src='img/hancock-lab-logo.svg'></a>"
        )
      )
    )
  ),


  # |- Upload -------------------------------------------------------------

  nav_panel(
    title = "Upload",
    value = "uploadPanel",
    layout_sidebar(
      sidebar = sidebar(
        class = "d-flex",
        width = "500px",
        open = NA,

        wellPanel(
          h1(class = "sidebar-title", "Upload your metabolites"),
          HTML(paste0(
            "<p class='mt-2'>Select a plain-text spreadsheet (a file ending ",
            "in csv, txt, or tsv) containing your metabolites in a single ",
            "column. You can also try our example data using",
            tooltip(
              actionLink(
                inputId = "tryExamples",
                label = "this link",
                .noWS = "after"
              ),
              "Load an example dataset from MetaboAnalyst"
            ), ".</p>"
          )),

          fileInput(
            inputId = "metaboliteUpload",
            label = NULL,
            buttonLabel = list(icon("upload"), "Browse..."),
            accept = c("csv", ".csv", "tsv", ".tsv", "txt", ".txt")
          ),

          strong("Does your data contain column names?"),
          div(
            class = "mt-1",
            checkboxInput(
              inputId = "header",
              label = "My data has a header",
              value = TRUE
            )
          ),

          strong("How is your data separated?"),
          div(
            class = "mt-1",
            radioButtons(
              inputId = "sep",
              label = NULL,
              choices = c(Comma = ",", Tab = "\t", Semicolon = ";"),
              selected = ","
            )
          )
        ),
        uiOutput("idTypePanel")
      ),
      uiOutput("uploadedTablePanel")
    )
  ),


  # |- Map --------------------------------------------------------------

  nav_panel(
    title = "Map",
    value = "mapPanel",
    layout_sidebar(
      sidebar = sidebar(
        id = "mapPanelSidebar",
        class = "d-flex",
        width = "500px",
        open = NA,

        wellPanel(
          h1(class = "sidebar-title mb-2", "Map your metabolites"),
          p(
            "Select one of the databases below. MetaCyc has higher quality ",
            "annotations, but KEGG may yield more hits, and will also allow ",
            "you to visualize your results with ",
            a(
              "Pathview",
              href = "https://bioconductor.org/packages/pathview",
              target = "_blank", rel = "noopener noreferrer", .noWS = "after"
            ),
            "."
          ),

          radioButtons(
            inputId = "dbChosen",
            label = NULL,
            choices = c("MetaCyc", "KEGG"),
            selected = "MetaCyc"
          ),

          disabled(
            actionButton(
              inputId = "mapButton",
              class = "btn-info",
              icon = icon("arrows-alt"),
              label = "Map metabolites",
              width = 200
            ) %>%
              tooltip("Map your metabolites to the selected database")
          )
        ),
        uiOutput("downloadPanel"),
        uiOutput("continueToViz")
      ),
      div(
        uiOutput("mappingSummaryPanel"),
        uiOutput("mappedMetabolitePanel")
      )
    )
  ),


  # |- Visualize --------------------------------------------------------

  nav_panel(
    title = "Visualize",
    value = "vizPanel",
    layout_sidebar(
      sidebar = sidebar(
        class = "sidebar d-flex",
        width = "500px",
        open = NA,
        wellPanel(
          h1(class = "sidebar-title", "Visualize with Pathview"),
          p(
            class = "mt-2",
            "To visualize pathways, upload some data and map the metabolites ",
            "with the KEGG database. Select a compound from the results ",
            "summary table, and you'll be able to visualize its pathways."
          ),
          uiOutput("pathwayPanel")
        )
      ),
      uiOutput("vizPanelUI")
    )
  ),


  # |- Tutorial ---------------------------------------------------------

  nav_panel(
    title = "Tutorial",
    value = "tutorialPanel",
    includeHTML("www/tutorial/tutorial.html")
  ),


  # |- About ------------------------------------------------------------

  nav_panel(
    title = "About",
    value = "aboutPanel",

    div(
      class = "mx-2 my-2",
      div(
        class = "bg-body-tertiary rounded-3 p-5 mb-4",
        div(
          class = "logoWrapper-about",
          h1(
            class = "display-3 fw-bold text-body-emphasis",
            "About"
          ),
          div(
            class = "mx-auto fs-4 text-muted",

            HTML(paste0(
              "<p>MetaBridge was created by Samuel Hinshaw, and is maintained ",
              "by Travis Blimkie at the ",
              "<a href='http://cmdr.ubc.ca/bobh' target='blank' ",
              "rel='noopener noreferrer'>REW Hancock Laboratory</a>",
              " at The University of British Columbia. It was originally ",
              "published in <i>Bioinformatics</i> (doi: ",
              "<a href='https://doi.org/10.1093/bioinformatics/bty331' ",
              "target='blank' rel='noopener noreferrer'>",
              "10.1093/bioinformatics/bty331</a>",
              "); please cite this paper when using MetaBridge in your ",
              "analyses. We also have a protocol for MetaBridge published in ",
              "<i>Current Protocols in Bioinformatics</i>. It covers how to ",
              "prepare data for input to MetaBridge, and includes an example ",
              "of building a protein-protein interaction network from ",
              "MetaBridge results using ",
              "<a href='https://networkanalyst.ca' target='blank' ",
              "rel='noopener noreferrer'>NetworkAnalyst</a>",
              ". The article is available at doi: ",
              "<a href='https://doi.org/10.1002/cpbi.98' target='blank' ",
              "rel='noopener noreferrer'>10.1002/cpbi.98</a>",
              ".</p>"
            )),

            p(
              "The example data used by MetaBridge is based on results from ",
              "a metabolomics study of pediatric sepsis published by ",
              "Mickiewicz et al., available ",
              a(
                "here",
                href = "https://atsjournals.org/doi/full/10.1164/rccm.201209-1726OC",
                target = "_blank",
                rel = "noopener noreferrer",
                .noWS = "after"
              ),
              "."
            ),

            p(
              "If you encounter any bugs or run into other troubles, please ",
              "post an issue at the ",
              a(
                "GitHub page",
                href = "https://github.com/hancockinformatics/MetaBridgeShiny/issues",
                target = "_blank",
                rel = "noopener noreferrer",
                .noWS = "after"
              ),
              ". Be sure to include detailed information on the error you ",
              "received, and the input you used, if possible."
            ),

            p(strong("MetaBridge uses the following databases and packages:"))
          ),
          div(wrapList(dependencyTable))
        )
      )
    )
  ),


  # |- Right side items -------------------------------------------------

  nav_spacer(),

  nav_item(input_dark_mode()),

  nav_item(a(
    icon("github"),
    "GitHub",
    href = "https://github.com/hancockinformatics/MetaBridgeShiny",
    target = "_blank",
    rel = "noopener noreferrer"
  )),

  # Divider
  nav_item(tagList(
    div(class = "vr d-none d-sm-flex h-100 mx-sm-2 text-white"),
    hr(class = "d-lg-none my-2 text-white-50")
  )),

  nav_item(metabridgeVersion, style = "color: var(--bs-nav-link-color)")
)


# Server ------------------------------------------------------------------

metabridgeServer <- function(input, output, session) {


  # Initialize ------------------------------------------------------------

  observeEvent(input$sessionInitialized, {
    source("deferred.R")
    runjs("handlers.initGetStarted();")
  }, ignoreInit = TRUE, once = TRUE)

  metaboliteObject <- reactiveVal()
  mappedMetabolites <- reactiveVal()
  mappingObject <- reactiveVal()
  mappingSummary <- reactiveValues(table = NULL, dbChosen = NULL)
  mappedMetaboliteTable <- reactiveVal()
  databaseChosen <- reactiveVal()
  selectedMetab <- reactiveVal()
  idTypeChosen <- reactiveVal()
  columnPicked <- reactiveVal()


  # Welcome buttons -------------------------------------------------------

  observeEvent(
    input$getStarted,
    nav_select("navbarLayout", selected = "uploadPanel")
  )
  observeEvent(
    input$tutorial,
    nav_select("navbarLayout", selected = "tutorialPanel")
  )
  observeEvent(
    input$about,
    nav_select("navbarLayout", selected = "aboutPanel")
  )


  # Example data ----------------------------------------------------------

  observeEvent(input$tryExamples, {
    metaboliteObject(example_data)
    mappingObject(NULL)
    mappedMetabolites(NULL)
    mappingObject(NULL)
    mappingSummary$table <- NULL
    mappingSummary$dbChosen <- NULL
    mappedMetaboliteTable(NULL)
    databaseChosen(NULL)
  })


  # User data -------------------------------------------------------------

  observeEvent({
    input$metaboliteUpload
    input$sep
    input$header
  }, {
    if (!is.null(input$metaboliteUpload)) {
      readr::read_delim(
        file = input$metaboliteUpload$datapath,
        col_names = input$header,
        delim = input$sep
      ) %>% metaboliteObject()

      mappingObject(NULL)
      mappedMetabolites(NULL)
      mappingObject(NULL)
      mappingSummary$table <- NULL
      mappingSummary$dbChosen <- NULL
      mappedMetaboliteTable(NULL)
      databaseChosen(NULL)
    }
  })


  # Render input table view -----------------------------------------------

  output$uploadSuccess <- renderUI({
    input$tryExamples
    if (is.null(metaboliteObject())) {
      return(NULL)
    }
    tagList(
      h4("Input data preview & ID selection"),
      HTML(
        "<p>If your data has loaded correctly, click a column to <b><u>",
        "highlight it in blue</u></b>, select the matching ID type in the ",
        "lower left box, then continue via the <b>Proceed to mapping</b> ",
        "button.</p>"
      )
    )
  })

  output$uploadedDataTable <- DT::renderDataTable(
    {
      input$tryExamples
      input$sep
      input$header
      input$metaboliteUpload
      if (is.null(metaboliteObject())) {
        return(NULL)
      } else {
        metaboliteObject()
      }
    },
    rownames = FALSE,
    selection = list(mode = "single", target = "column"),
    style = "bootstrap",
    options = list(
      scrollX = "100%",
      scrollY = "67vh",
      scrollCollapse = TRUE,
      paging  = FALSE,
      dom = "tir"
    )
  )

  output$uploadedTablePanel <- renderUI(div(
    uiOutput("uploadSuccess"),
    DT::dataTableOutput("uploadedDataTable"),
  ))


  # Column selection ------------------------------------------------------

  output$idTypePanel <- renderUI({
    if (!is.null(metaboliteObject())) {
      wellPanel(
        h1(class = "sidebar-title mb-2", "Select an ID type"),
        HTML(
          "<p>MetaBridge supports mapping with HMDB or KEGG metabolite IDs. ",
          "Once you've uploaded your data, select the IDs to use for mapping ",
          "by clicking anywhere inside the column. Then, select the ",
          "appropriate option below before clicking the <b>Proceed to ",
          "mapping</b> button."
        ),
        radioButtons(
          inputId = "idType",
          label = NULL,
          choices = c("HMDB", "KEGG"),
          selected = character(0)
        ),
        disabled(
          actionButton(
            inputId = "continueToMap",
            class = "btn-primary",
            icon = icon("check"),
            label = "Proceed to mapping",
            width = "100%"
          ) %>% tooltip("Continue to the mapping step")
        )
      )
    } else {
      NULL
    }
  })

  observeEvent({
    input$uploadedDataTable_columns_selected
    input$sep
    input$header
  }, {
    columnIndex <- input$uploadedDataTable_columns_selected + 1
    columnName <- colnames(metaboliteObject())[columnIndex]
    columnPicked(columnName)
  })

  observeEvent({
    columnPicked()
    input$idType
  }, {
    if (all(length(columnPicked()) != 0 & !is.null(input$idType))) {
      enable("continueToMap")
    } else {
      disable("continueToMap")
    }
  })


  # Mapping ---------------------------------------------------------------

  observeEvent(input$continueToMap, {
    nav_select(id = "navbarLayout", selected = "mapPanel")
    enable("mapButton")
  })

  observeEvent(input$mapButton, idTypeChosen(input$idType))

  observeEvent(input$mapButton, {
    databaseChosen(input$dbChosen)
    removeUI(selector = "#mappingAlert")

    mappingOutput <- mapGenerally(
      importDF = metaboliteObject(),
      col = columnPicked(),
      db = databaseChosen(),
      idType = idTypeChosen()
    )

    mappedMetabolites(mappingOutput$data)
    mappingObject(mappingOutput)

    mappingAlert(
      status = mappingOutput$status,
      message = mappingOutput$message,
      suggest = mappingOutput$suggest
    )
  })

  observeEvent(
    input$remap,
    nav_select(id = "navbarLayout", selected = "uploadPanel")
  )


  # Summary/grouped table -------------------------------------------------

  observeEvent(input$mapButton, {
    results <- generateSummaryTable(
      mappingObject(),
      idTypeChosen(),
      databaseChosen()
    )
    mappingSummary$table <- results$table
    mappingSummary$dbChosen <- results$dbChosen
  })

  output$mappingSummaryTable <- DT::renderDataTable(
    {
      mappingSummary$table %>% hyperlinkTable(databaseChosen())
    },
    rownames = FALSE,
    style = "bootstrap",
    escape = FALSE,
    selection = "single",
    options = list(
      scrollX = "100%",
      scrollY = "35vh",
      scrollCollapse = TRUE,
      paging  = FALSE,
      dom = "tir"
    )
  )

  output$mappingSummaryPanel <- renderUI({
    mappingSummary$table

    if (is.null(mappingObject())) {
      return(NULL)
    } else if (mappingObject()$status %in% c("error", "empty")) {
      return(NULL)
    } else {
      return(
        tagList(
          h3(paste0("Mapping summary: ", databaseChosen())),
          DT::dataTableOutput("mappingSummaryTable")
        )
      )
    }
  })

  observeEvent(
    input$mappingSummaryTable_rows_selected,
    selectedMetab(input$mappingSummaryTable_rows_selected)
  )

  observeEvent(input$mapButton, selectedMetab(NULL))


  # Single-metabolite table -----------------------------------------------

  observeEvent({
    selectedMetab()
    input$mapButton
  }, {
    if (mappingObject()$status %in% c("error", "empty")) {
      mappingObject()$data %>% mappedMetaboliteTable()

    } else if (databaseChosen() == "KEGG") {
      if (mappingSummary$dbChosen != "KEGG") {
        return(NULL)
      } else {
        generateKEGGMetabTable(
          mappingObject(),
          mappingSummary$table,
          selectedMetab(),
          idTypeChosen()
        ) %>% mappedMetaboliteTable()
      }

    } else if (databaseChosen() == "MetaCyc") {
      if (mappingSummary$dbChosen != "MetaCyc") {
        return(NULL)
      } else {
        generateMetaCycMetabTable(
          mappingObject(),
          mappingSummary$table,
          selectedMetab(),
          idTypeChosen()
        ) %>% mappedMetaboliteTable()
      }
    }
  })

  output$mappedMetaboliteTable <- DT::renderDataTable(
    {
      if (is.null(mappingObject()) | is.null(selectedMetab())) {
        return(data.frame())

      } else if (mappingObject()$status == "success") {
        mappedMetaboliteTable() %>% hyperlinkTable(databaseChosen())
      }
    },
    rownames = FALSE,
    style = "bootstrap",
    escape = FALSE,
    selection = "single",
    options = list(
      scrollX = "100%",
      scrollY = "35vh",
      scrollCollapse = TRUE,
      paging  = FALSE,
      dom = "tir"
    )
  )

  output$mappedMetabolitePanel <- renderUI({
    div(
      if (is.null(mappingObject())) {
        return(NULL)
      } else if (mappingObject()$status %in% c("error", "empty")) {
        tags$h3("Intermediate Results")
      } else {
        tagList(
          tags$hr(),
          tags$h3("Per-metabolite results")
        )
      },
      DT::dataTableOutput("mappedMetaboliteTable")
    )
  })


  # Results download ------------------------------------------------------

  output$downloadPanel <- renderUI({
    if (!is.null(mappedMetabolites())) {
      wellPanel(
        h1(class = "sidebar-title mb-2", "Download your results"),
        p(
          "Use the button below to download your full mapping results as ",
          "a tab-delimited text file."
        ),
        downloadButton(
          outputId = "downloadMappingData",
          label = "Download results",
          class = "btn-success",
          style = "width: 200px"
        )
      )
    }
  })

  # Clean up MetaCyc reactions if they're being downloaded
  cleanMappedMetabolites <- reactive({
    req(mappedMetabolites())
    if (databaseChosen() == "MetaCyc") {
      mappedMetabolites() %>% cleanReactions(.)
    } else {
      mappedMetabolites()
    }
  })

  output$downloadMappingData <- downloadHandler(
    filename = function() {
      paste0(
        ifelse(
          test = is.null(input$metaboliteUpload),
          yes = "example_dataset",
          no = tools::file_path_sans_ext(input$metaboliteUpload$name)
        ),
        "_mapped_",
        databaseChosen(),
        ".tsv"
      )
    },
    content = function(filename) {
      readr::write_tsv(
        x = cleanMappedMetabolites(),
        file = filename
      )
    }
  )


  # Switch to Visualize -------------------------------------------------

  output$continueToViz <- renderUI({
    if (!is.null(mappedMetabolites())) {
      if (is.null(databaseChosen()) | databaseChosen() == "MetaCyc") {
        return(NULL)
      } else {
        wellPanel(
          h1(class = "sidebar-title mb-2", "Visualize your results"),
          HTML(
            "<p>If you chose <b>KEGG</b> as the database to map your ",
            "metabolites, you can Visualize your results with Pathview. ",
            "Select a metabolite from the top table, then click the button ",
            "below to see the pathways it's involved in.</p>"
          ),
          if (databaseChosen() == "KEGG" & !is.null(selectedMetab())) {
            actionButton(
              inputId = "visualizeButton",
              class = "btn-primary",
              icon = icon("eye"),
              label = "Visualize with Pathview",
              width = "100%"
            )
          } else {
            disabled(
              actionButton(
                inputId = "visualizeButton",
                class = "btn-primary",
                icon = icon("eye"),
                label = "Visualize with Pathview",
                width = "100%"
              ) %>%
                tooltip(
                  "Select a metabolite from the summary table to visualize"
                )
            )
          }
        )
      }
    }
  })

  observeEvent(input$visualizeButton, {
    nav_select(id = "navbarLayout", selected = "vizPanel")
    removeNotification(id = "mappingAlert")
  })


  # Visualize tab ---------------------------------------------------------

  selectedRowAttrs <- reactiveValues(
    "selectedCompound" = NULL,
    "selectedCompoundName" = NULL,
    "pathwaysOfSelectedCompound" = NULL,
    "genesOfSelectedCompound" = NULL
  )

  observeEvent(input$mappingSummaryTable_rows_selected, {
    pathwayMappingAttrs <- mapPathways(
      idType = "KEGG",
      selectedRow = selectedMetab(),
      summaryTable = mappingSummary$table,
      fullTable = mappingObject()$data
    )

    selectedRowAttrs$selectedCompound <-
      pathwayMappingAttrs$selectedCompound

    selectedRowAttrs$selectedCompoundName <-
      pathwayMappingAttrs$selectedCompoundName

    selectedRowAttrs$genesOfSelectedCompound <-
      pathwayMappingAttrs$genesOfSelectedCompound

    selectedRowAttrs$pathwaysOfSelectedCompound <-
      pathwayMappingAttrs$pathwaysOfSelectedCompound
  })


  # Pathway selection UI --------------------------------------------------

  map_tab <- actionLink(inputId = "back_to_map", "Map tab", .noWS = "after")

  output$pathwayPanel <- renderUI({
    if (!is.null(mappingObject())) {

      if (databaseChosen() == "KEGG") {

        if (is.null(selectedRowAttrs$selectedCompound)) {
          tagList(
            strong("No compound selected"),
            p("You must select a compound in the ", map_tab, " to see its pathways.")
          )
        } else if (nrow(selectedRowAttrs$pathwaysOfSelectedCompound) == 0) {
          tagList(
            strong(paste0(
              "Pathways for ",
              stringr::str_to_title(selectedRowAttrs$selectedCompoundName)
            )),
            p("No pathways were found for this compound.")
          )
        } else {
          tagList(
            strong(paste0(
              "Pathways for ",
              stringr::str_to_title(selectedRowAttrs$selectedCompoundName)
            )),
            HTML(
              "<p>Note that each pathway may take some time to process. For ",
              "each pathway, only the compound selected is shown, but ",
              "<b>ALL</b> mapped genes are highlighted.</p>"
            ),
            selectInput(
              inputId = "pathwaysPicked",
              label = NULL,
              choices = selectedRowAttrs$pathwaysOfSelectedCompound$namedPway
            )
          )
        }
      } else if (databaseChosen() == "MetaCyc") {
        tagList(
          strong("Incorrect mapping database"),
          p(
            "You must use the KEGG database to enable visualization with ",
            "Pathview; click to return to the ", map_tab, "."
          )
        )
      } else {
        tagList(
          strong("Pathway mapping error"),
          p(
            "Something went wrong with the metabolite or pathway mapping. ",
            "Please check your inputs and try again."
          )
        )
      }
    }
  })

  observeEvent(input$back_to_map, nav_select("navbarLayout", selected = "mapPanel"))


  # Render Pathview image -------------------------------------------------

  output$pathwayView <- renderImage({
    pathwayNameIDcol <- as.name("namedPway")
    selectedPathway <- quo(input$pathwaysPicked)

    selectedPathwayID <-
      selectedRowAttrs$pathwaysOfSelectedCompound %>%
      filter(!!(pathwayNameIDcol) == input$pathwaysPicked) %>%
      extract2("id")

    filename <- visualizePathview(
      pathway = selectedPathwayID,
      genes = selectedRowAttrs$genesOfSelectedCompound,
      cpd = selectedRowAttrs$selectedCompound
    )

    return(list(
      src = filename,
      contentType = "image/png",
      width = "95%",
      alt = paste0("Pathway map of KEGG pathway ", input$pathwaysPicked)
    ))
  }, deleteFile = TRUE)

  observeEvent(input$pathwaysPicked, {

    output$vizPanelUI <- renderUI({

      if (is.null(databaseChosen())) {
        tagList(
          h3(class = "mb-4", "Pathway view"),
          div(
            class = "alert alert-dismissible alert-danger",
            "There is nothing selected to map!"
          )
        )
      } else if (databaseChosen() == "MetaCyc") {
        tagList(
          h3(class = "mb-4", "Pathway view"),
          div(
            class = "alert alert-dismissible alert-danger",
            "You must map via KEGG to visualize your results with pathview!"
          )
        )
      } else if (is.null(selectedMetab())) {
        tagList(
          h3(class = "mb-4", "Pathway view"),
          div(
            class = "alert alert-dismissible alert-danger",
            "You must select a metabolite to visualize your results with pathview!"
          )
        )
      } else {
        shinycssloaders::withSpinner(
          ui_element = imageOutput("pathwayView", inline = TRUE),
          type = 8,
          color = bs_get_variables(metabridgeTheme, "primary")
        )
      }
    })
  })
}

shinyApp(metabridgeUI, metabridgeServer)
