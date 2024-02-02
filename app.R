# Load packages -----------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(dplyr)
})

metabridgeTheme <- bs_theme(version = 5, preset = "flatly")


# UI ----------------------------------------------------------------------

metabridge_ui <- page_fluid(
  title = "MetaBridge",
  theme = metabridgeTheme,
  useShinyjs(),

  tags$head(
    tags$script(src = "js/client.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    tags$link(
      rel = "icon",
      type = "image/png",
      sizes = "32x32",
      href = "img/favicon-32x32.png"
    ),
    tags$link(
      rel = "icon",
      type = "image/png",
      sizes = "16x16",
      href = "img/favicon-16x16.png"
    )
  ),

  page_navbar(
    id = "navbarLayout",
    nav_item(HTML("<img src='img/logo_white.svg' alt='' height='28'>")),


    # |- Welcome ----------------------------------------------------------

    nav_panel(
      value = "welcomePanel",
      title = "MetaBridge",

      div(
        class = "mx-2 my-2",
        div(
          class = "p-5 bg-body-tertiary rounded-3",
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
              "To start, you'll want a set of metabolites as HMDB or KEGG IDs. ",
              "We recommend",
              a(
                "MetaboAnalyst",
                href = "http://www.metaboanalyst.ca",
                target = "_blank",
                rel = "noopener noreferrer"
              ),
              "for metabolomics data processing and ID conversion, if ",
              "you have only compound names."
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
              "Click the button below to Get Started! If you'd like to learn ",
              "more about how MetaBridge can be used, check our Tutorial. For ",
              "more information, including where to report bugs or problems and ",
              "how to cite MetaBridge, please refer to the About page."
            ),

            div(
              actionButton(
                inputId = "getStarted",
                class = "btn-primary btn-lg px-4 me-md-2 disabled",
                icon = icon("circle-notch", class = "fa fa-spin"),
                label = "Initializing...",
                width = "200px"
              ),

              actionButton(
                inputId = "tutorial",
                class = "btn-success btn-lg px-4 me-md-2 btn-hidden",
                label = "Tutorial",
                width = "200px"
              ),

              actionButton(
                inputId = "about",
                class = "btn-info btn-lg px-4 me-md-2 btn-hidden",
                label = "About",
                width = "200px"
              )
            )
          )
        ),
        div(
          style = "position:fixed; bottom:0px; padding-bottom:10px",
          HTML(
            "<a href='http://cmdr.ubc.ca/bobh/'>",
            "<img src='img/hancock-lab-logo.svg'></a>"
          )
        )
      )
    ),


    # |- Upload -------------------------------------------------------------

    nav_panel(
      title = "Upload",
      value = "uploadPanel",
      card(
        height = "88vh",
        layout_sidebar(
          sidebar = sidebar(
            title = "Upload your metabolites",
            class = "d-flex",
            width = "500px",
            open = NA,

            p(
              "Select a plain-text spreadsheet (a file ending in csv, txt, or ",
              "tsv) containing your metabolites in a single column. You can ",
              "also try our example data using",
              tooltip(
                actionLink(inputId = "tryExamples", "this link."),
                "Try an example dataset from MetaboAnalyst"
              )
            ),

            fileInput(
              inputId = "metaboliteUpload",
              label = NULL,
              buttonLabel = list(icon("upload"), "Browse..."),
              accept = c("csv", ".csv", "tsv", ".tsv", "txt", ".txt")
            ),

            strong("Does your data contain column names?"),
            checkboxInput(
              inputId = "header",
              label = "My data has a header",
              value = TRUE
            ),

            br(),

            strong("How is your data separated?"),
            radioButtons(
              inputId = "sep",
              label = NULL,
              choices = c(Comma = ",", Tab = "\t", Semicolon = ";"),
              selected = ","
            ),

            br(),

            strong("Select an ID Type"),
            HTML(
              "<p>MetaBridge supports mapping with HMDB or KEGG metabolite ",
              "IDs. Please ensure the ID selected here matches the column ",
              "<b><u>highlighted in blue</u></b> before clicking the <b>",
              "Proceed</b> button."
            ),
            radioButtons(
              inputId = "idType",
              label = NULL,
              choices = c("HMDB", "KEGG"),
              selected = character(0)
            ),

            br(),

            disabled(
              actionButton(
                inputId = "continueToMap",
                class = "btn-primary",
                icon = icon("check"),
                label = "Proceed to Mapping",
                width = "100%"
              ) %>% tooltip("Continue to the mapping step")
            )
          ),
          uiOutput("uploadedTablePanel")
        )
      )
    ),


    # |- Map --------------------------------------------------------------

    nav_panel(
      title = "Map",
      value = "mapPanel",
      card(
        min_height = "88vh",
        layout_sidebar(
          sidebar = sidebar(
            title = "Map your metabolites",
            id = "mapPanelSidebar",
            class = "d-flex",
            width = "500px",
            open = NA,

            p(
              "Select one of the database options below. MetaCyc has higher ",
              "quality annotations, but KEGG may yield more hits, and will ",
              "also allow you to visualize your results with ",
              a(
                "Pathview",
                href = "https://bioconductor.org/packages/release/bioc/html/pathview.html",
                target = "_blank",
                rel = "noopener noreferrer",
                .noWS = "after"
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
                label = "Map Metabolites",
                width = 200
              ) %>%
                tooltip(
                  id = "mapButtonTT",
                  "Map your metabolites to the selected database"
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
      )
    ),


    # |- Pathview ---------------------------------------------------------

    nav_panel(
      title = "Pathview",
      value = "vizPanel",
      card(
        min_height = "88vh",
        layout_sidebar(
          sidebar = sidebar(
            title = "Visualize with Pathview",
            class = "sidebar d-flex",
            width = "500px",
            open = NA,
            p(
              "To visualize pathways, upload data, and map the metabolites ",
              "with KEGG. Then you can return here to see the pathway images."
            ),
            uiOutput("pathwayPanel")
          ),
          uiOutput("vizPanelUI")
        )
      )
    ),


    # |- Tutorial ---------------------------------------------------------

    nav_panel(
      title = "Tutorial",
      value = "tutorialPanel",

      div(
        class = "mx-2 my-2",
        div(
          class = "bg-body-tertiary rounded-3 p-5 mb-4",
          h1(
            class = "display-3 fw-bold text-body-emphasis",
            "Tutorial"
          ),
          h3("Network-based integrative analysis with MetaBridge"),
          div(
            class = "mx-auto fs-4 text-muted",
            p(
              "This page covers a sample workflow for integrating your ",
              "metabolomics data with transcriptomics or proteomics data ",
              "using network-based approaches. You can also view this ",
              "information on our ",
              a(
                "GitHub page",
                href = paste0(
                  "https://github.com/hancockinformatics/MetaBridgeShiny/blob/",
                  "master/tutorial/tutorial.md"
                ),
                target = "_blank",
                rel = "noopener noreferrer",
                .noWS = "after"
              ),
              "."
            )
          )
        ),
        div(
          class = "container tutorial",
          includeMarkdown("tutorial/tutorial.md")
        )
      )
    ),


    # |- About ------------------------------------------------------------

    nav_panel(
      title = "About",
      value = "aboutPanel",

      div(
        class = "mx-2 my-2",
        div(
          class = "bg-body-tertiary rounded-3 p-5 mb-4",
          h1(
            class = "display-3 fw-bold text-body-emphasis",
            "About"
          ),
          div(
            class = "mx-auto fs-4 text-muted",

            HTML(paste0(
              "<p>MetaBridge was created by Samuel Hinshaw, and is maintained ",
              "by Travis Blimkie at the ",
              "<a href='http://cmdr.ubc.ca/bobh' target='blank' rel='noopener ",
              "noreferrer'>REW Hancock Laboratory</a>",
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
              "The example data used by MetaBridge is based on results from a ",
              "metabolomics study of pediatric sepsis published by Mickiewicz ",
              "et al., available ",
              a(
                "here",
                href = "https://www.atsjournals.org/doi/full/10.1164/rccm.201209-1726OC",
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
            )
          ),

          h3(
            class = "pt-4",
            strong("MetaBridge uses the following databases and packages:")
          ),

          tags$dl(
            class = "fs-6",
            tags$dt(
              a(href = "https://metacyc.org/", "MetaCyc v26"),
              tags$dd("Curated database for human metabolomic data.")
            ),

            tags$dt(
              a(
                href = "https://www.genome.jp/kegg/",
                "KEGG Release 105",
                target = "_blank",
                rel = "noopener noreferrer"
              ),
              tags$dd("Large database containing multiple data types.")
            ),

            tags$dt(
              a(
                href = "https://shiny.rstudio.com/",
                "Shiny",
                target = "_blank",
                rel = "noopener noreferrer",
              ),
              tags$dd("Web application framework for R.")
            ),

            tags$dt(
              a(
                href = "https://github.com/andrewsali/shinycssloaders",
                "shinycssloaders",
                target = "_blank",
                rel = "noopener noreferrer"
              ),
              tags$dd("Animated loaders for shiny outputs.")
            ),

            tags$dt(
              a(
                href = "https://deanattali.com/shinyjs/",
                "shinyjs",
                target = "_blank",
                rel = "noopener noreferrer"
              ),
              tags$dd(
                "Improve the user experience of your Shiny apps in seconds."
              )
            ),

            tags$dt(
              a(
                href = "https://www.tidyverse.org/",
                "Tidyverse",
                target = "_blank",
                rel = "noopener noreferrer"
              ),
              tags$dd(
                "A collection of R packages designed for data science."
              )
            ),

            tags$dt(
              a(
                href = "https://doi.org/10.1093/bioinformatics/btt285",
                "Pathview",
                target = "_blank",
                rel = "noopener noreferrer"
              ),
              tags$dd("Pathway-based data integration and visualization.")
            )
          )
        )
      )
    ),


    # |- Right side items -------------------------------------------------

    nav_spacer(),

    nav_item(a(
      icon("github"),
      "GitHub",
      href = "https://github.com/hancockinformatics/ShinyABCi",
      target = "_blank",
      rel = "noopener noreferrer"
    ))
  )
)


# Server ------------------------------------------------------------------

metabridge_server <- function(input, output, session) {


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
    nav_select(id = "navbarLayout", selected = "uploadPanel")
  )
  observeEvent(
    input$tutorial,
    nav_select(id = "navbarLayout", selected = "tutorialPanel")
  )
  observeEvent(
    input$about,
    nav_select(id = "navbarLayout", selected = "aboutPanel")
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
        "lower left box, then continue via the <b>Proceed</b> button.</p>"
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
      scrollY = "60vh",
      scrollCollapse = TRUE,
      paging  = FALSE,
      dom = "tir"
    )
  )


  # Output input table view -----------------------------------------------

  output$uploadedTablePanel <- renderUI(div(
    uiOutput("uploadSuccess"),
    DT::dataTableOutput("uploadedDataTable"),
  ))


  # Column selection ------------------------------------------------------

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
    } else if (mappingObject()$status == "error" | mappingObject()$status == "empty") {
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
    if (mappingObject()$status == "error" | mappingObject()$status == "empty") {
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
      } else if (
        mappingObject()$status == "error" | mappingObject()$status == "empty"
      ) {
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
      tagList(
        hr(),
        strong("Download your results"),
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


  # Switch to Pathview --------------------------------------------------

  output$continueToViz <- renderUI({
    if (!is.null(mappedMetabolites())) {
      if (is.null(databaseChosen()) | databaseChosen() == "MetaCyc") {
        return(NULL)
      } else {
        tagList(
          hr(),
          strong("Visualize your results"),
          HTML(
            "<p>If you chose <b>KEGG</b> as the database to map your ",
            "metabolites, you can visualize your results with Pathview. ",
            "Select a metabolite from the top table, then click the button ",
            "below to see the pathways it's involved in.</p>"
          ),
          if (databaseChosen() == "KEGG" & !is.null(selectedMetab())) {
            actionButton(
              inputId = "visualizeButton",
              class = "btn-primary",
              icon = icon("eye"),
              label = "Visualize",
              width = "100%"
            )
          } else {
            disabled(actionButton(
              inputId = "visualizeButton",
              class = "btn-primary",
              icon = icon("eye"),
              label = "Visualize",
              width = "100%"
            ) %>% tooltip("Select a metabolite from the summary table to visualize"))
          }
        )
      }
    }
  })

  observeEvent(
    input$visualizeButton,
    nav_select(id = "navbarLayout", selected = "vizPanel")
  )


  # Pathview tab ----------------------------------------------------------

  selectedRowAttrs <- reactiveValues(
    "selectedCompound" = NULL,
    "selectedCompoundName" = NULL,
    "pathwaysOfSelectedCompound" = NULL,
    "genesOfSelectedCompound" = NULL
  )

  observeEvent(input$mappingSummaryTable_rows_selected, {
    pathwayMappingAttrs <- generalPathwayMapping(
      db = databaseChosen(),
      idType = idTypeChosen(),
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

  output$pathwayPanel <- renderUI({

    if (!is.null(mappingObject())) {
      if (nrow(selectedRowAttrs$pathwaysOfSelectedCompound) == 0) {
        div(
          strong(paste0(
            "Pathways for ",
            selectedRowAttrs$selectedCompoundName
          )),
          p("No pathways found for this compound.")
        )
      } else if (databaseChosen() == "KEGG") {
        div(
          strong(paste0(
            "Pathways for ",
            stringr::str_to_title(selectedRowAttrs$selectedCompoundName)
          )),
          HTML(
            "<p>Note that each pathway may take some time to process. For each ",
            "pathway, only the compound selected is shown, but <b>ALL</b> ",
            "mapped genes are highlighted.</p>"
          ),
          selectInput(
            inputId = "pathwaysPicked",
            label = NULL,
            choices = selectedRowAttrs$pathwaysOfSelectedCompound$namedPway
          )
        )
      } else if (databaseChosen() == "MetaCyc") {
        div(
          strong(paste0(
            "Pathways for ",
            selectedRowAttrs$selectedCompoundName
          )),
          HTML(
            "<p>Note that each pathway may take some time to process. For each ",
            "pathway, only the compound selected is shown, but <b>ALL</b> ",
            "mapped genes are highlighted.</p>"
          ),
          selectInput(
            inputId = "pathwaysPicked",
            label = NULL,
            choices = selectedRowAttrs$pathwaysOfSelectedCompound$pathwayName
          )
        )
      }
    }
  })


  # Render Pathview image -------------------------------------------------

  output$pathwayView <- renderImage({
    if (is.null(input$pathwaysPicked)) {
      return(
        list(
          src = "./logo_background.svg",
          class = "card-img-top",
          contentType = "image/svg",
          width = 512,
          height = 512,
          alt = "pathway placeholder"
        )
      )
    }

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
      width = 1000,
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
          ui_element = imageOutput("pathwayView"),
          type = 8,
          color = bs_get_variables(metabridgeTheme, "blue")
        )
      }
    })
  })
}

shinyApp(metabridge_ui, metabridge_server)
