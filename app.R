# Load packages -----------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(dplyr)
})


# UI ----------------------------------------------------------------------

metabridge_ui <- page_fluid(
  title = "MetaBridge",
  theme = bs_theme(version = 5, preset = "flatly"),
  useShinyjs(),

  tags$head(
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


    # |- Welcome ----------------------------------------------------------

    nav_panel(
      value = "welcomePanel",
      title = "MetaBridge",

      div(
        class = "container my-2",
        div(
          class = "p-5 bg-body-tertiary rounded-3",
          h1(
            class = "display-3 fw-bold text-body-emphasis",
            "Welcome"
          ),

          div(
            class = "mx-auto fs-5 text-muted",
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
                class = "btn-primary btn-lg px-4 me-md-2",
                label = "Get started",
                width = "155px"
              ),
              actionButton(
                inputId = "tutorial",
                class = "btn-success btn-lg px-4 me-md-2",
                label = "Tutorial",
                width = "155px"
              ),
              actionButton(
                inputId = "about",
                class = "btn-info btn-lg px-4 me-md-2",
                label = "About",
                width = "155px"
              )
            )
          )
        )
      )
    ),


    # |- Upload -------------------------------------------------------------

    nav_panel(
      title = "Upload",
      value = "uploadPanel",
      card(
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
              actionLink(inputId = "tryExamples", "this link", .noWS = "after"),
              "."
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
            uiOutput("columnPickerPanel")
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
        layout_sidebar(
          sidebar = sidebar(
            title = "Choose a database",
            id = "mapPabelSidebar",
            class = "d-flex",
            width = "500px",
            open = NA,

            p(
              "Select one of the options below. ",
              "MetaCyc has higher quality annotations, but KEGG may yield more ",
              "hits, and will also allow you to visualize your results with ",
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
                class = "btn-primary",
                icon = icon("arrows-alt"),
                label = "Map Metabolites"
              ) %>%
                tooltip(
                  id = "mapButtonTT",
                  "Map your metabolites to the selected database"
                )
            )
          )
        )
      )
    ),


    # |- Pathview ---------------------------------------------------------

    nav_panel(
      title = "Pathview",
      value = "vizPanel",
      id = "visualizationPanel",
      class = "viz-panel",
      uiOutput("vizPanelUI")
    ),


    # |- Tutorial ---------------------------------------------------------

    nav_panel(
      title = "Tutorial",
      div(
        class = "container my-2",
        div(
          class = "bg-body-tertiary rounded-3 p-5 mb-4",
          h1(
            class = "display-3 fw-bold text-body-emphasis",
            "Tutorial"
          ),
          h3("Network-based integrative analysis with MetaBridge"),
          div(
            class = "mx-auto fs-5 text-muted",
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
          class = "tutorial",
          includeMarkdown("tutorial/tutorial.md")
        )
      )
    ),


    # |- About ------------------------------------------------------------

    nav_panel(
      title = "About",
      div(
        class = "container my-2",
        div(
          class = "bg-body-tertiary rounded-3 p-5 mb-4",
          h1(
            class = "display-3 fw-bold text-body-emphasis",
            "About"
          ),
          div(
            class = "mx-auto fs-5 text-muted",

            p(
              "MetaBridge was created by Samuel Hinshaw, and is maintained by ",
              "Travis Blimkie at the ",
              a(
                "REW Hancock Laboratory",
                href = "http://cmdr.ubc.ca/bobh",
                target = "_blank",
                rel = "noopener noreferrer"
              ),
              "at The University of British Columbia. It was originally ",
              "published in <i>Bioinformatics</i> (doi: ",
              a(
                "10.1093/bioinformatics/bty331",
                href = "https://doi.org/10.1093/bioinformatics/bty331",
                target = "_blank",
                rel = "noopener noreferrer",
                .noWS = "after"
              ),
              "; please cite this paper when using MetaBridge in your ",
              "analyses. We also have a protocol for MetaBridge published in ",
              "<i>Current Protocols in Bioinformatics</i>. It covers how to ",
              "prepare data for input to MetaBridge, and includes an example ",
              "of building a protein-protein interaction network from ",
              "MetaBridge results using ",
              a(
                "NetworkAnalyst",
                href = "https://networkanalyst.ca",
                target = "_blank",
                rel = "noopener noreferrer",
                .noWS = "after"
              ),
              ". The article is available at doi: ",
              a(
                "10.1002/cpbi.98",
                href = "https://doi.org/10.1002/cpbi.98",
                target = "_blank",
                rel = "noopener noreferrer",
                .noWS = "after"
              ),
              "."
            ),

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
            ),

            h4(
              class = "pt-4",
              strong("MetaBridge uses the following databases and packages:")
            ),

            p(
              tags$dl(
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

metabridge_server <- function(input, output, session) {}

shinyApp(metabridge_ui, metabridge_server)
