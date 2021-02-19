
# 1. To-do ----------------------------------------------------------------

# Updated the tutorial images

# Need to run these lines each time app is published so packages from
# Bioconductor can be found by Shiny.
# library(BiocManager)
# options(repos = BiocManager::repositories())




# 2. Load the first couple libraries --------------------------------------

# Most libraries and functions are loaded through a call to `deferred.R` at the
# beginning of the `server()` function (approx. #488).
library(shiny)
library(shinyjs)

# Useful colours which match the flatly theme:
# Dark blue     #2c3e50
# Turquoise     #18bc9c
# Light blue    #3498db
# DT blue       #0075b0
# Grey          #ecf0f1
# White         #fff




# 3. Define UI code -------------------------------------------------------

# Workaround to ensure logo in top left corner (tab bar) is found/rendered when
# app is published to "shinyapps.io"
addResourcePath(prefix = "pics", directoryPath = "./www")

ui <- fluidPage(

  # Head linking to Flatly bootstrap theme and my personal tweaks.
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/user.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/tippy.css"),

    # Favicon options
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "/apple-touch-icon.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "/favicon-16x16.png"),
    tags$link(rel = "manifest", href = "/manifest.json"),
    tags$link(rel = "mask-icon", href = "/safari-pinned-tab.svg", color = "#303e4e"),
    tags$meta(name = "theme-color", content = "#303e4e")
  ),

  # * 3.1 Begin the tab bar layout -------------------------------------------

  navbarPage(
    id          = "navbarLayout",
    position    = "fixed-top",
    windowTitle = "MetaBridge",
    collapsible = TRUE,

    # title = htmltools::HTML(
    #   "<img src='pics/logo_white.svg' alt='' height='28'"
    # ),
    title = div(
      htmltools::HTML(
          "<img src='pics/logo_white.svg' alt='' height='28'>"
        ),

      tags$div(
        id = "github-img",
        htmltools::HTML(paste0(
          "<a href='https://github.com/hancockinformatics/MetaBridgeShiny'> ",
          "<img src = 'github.svg'> </a>"
        ))
      )
    ),

    # Make sure we enable ShinyJS. We also add the `tags$style()` call to add
    # space between the navbar and body content; otherwise the navbar would
    # overlap the elements below it (caused by fixed navbar).
    header = tagList(
      useShinyjs(),
      tags$style(type = "text/css", "body {padding-top: 80px;}")
    ),

    # * 3.2 Welcome tab & landing page ------------------------------------

    tabPanel(
      title = "MetaBridge",
      value = "welcomePanel",

      # Main panel that will contain text and links
      tags$div(
        id    = "welcomeHero",
        class = "jumbotron",

        h1("Welcome"),

        tags$hr(),

        tags$div(
          class = "logoWrapper",

          tags$p(
            "Welcome to MetaBridge, a user-friendly web tool for ",
            "network-based integrative analysis of metabolomics data. Here ",
            "you can upload a set of metabolites and identify the directly ",
            "interacting enzymes for network integration."
          ),
          tags$p(
            "To start, you'll want a set of metabolites as",
            "HMDB or KEGG IDs. We recommend",
            tags$a("MetaboAnalyst", href = "http://www.metaboanalyst.ca"),
            "for metabolomics data processing, as well as ID conversion if ",
            "you have only compound names."
          ),
          tags$p(
            "With the output of MetaBridge, you can create a ",
            "protein-protein interaction network representative ",
            "of your metabolomics data. We recommend",
            tags$a("NetworkAnalyst", href = "http://www.networkanalyst.ca"),
            "for generation of these networks and for network-based ",
            "integration with data from other omics types."
          ),

          tags$p(
            "Click the button below to Get Started! If you'd like to learn ",
            "more about how MetaBridge can be used, check the Tutorial. For ",
            "more information, including where to report bugs or problems and ",
            "how to cite MetaBridge, please refer to the About page."
          ),

          tags$br(),

          div(
            # Buttons linking to various tabs of the app. To see how these
            # buttons are hidden, refer to "www/js/client.js".
            # First the button which shows the app loading, then links to the
            # Upload panel.
            actionButton(
              inputId = "getStarted",
              label = "Initializing App...",
              class = "btn-primary btn-lg disabled",
              `data-position` = "bottom",
              icon("circle-o-notch", class = "fa fa-spin", lib = "font-awesome")
            ),

            # Horizontal spacer
            HTML("&nbsp;&nbsp;&nbsp;"),

            # Linking to the Tutorials page
            actionButton(
              inputId = "tutorial",
              label   = "Tutorial",
              class   = "btn-success btn-lg btn-tooltip btn-hidden",
              style   = "width: 155px",
              title   = "Learn how to use MetaBridge for integrative analysis.",
              `data-position` = "bottom"
            ),

            HTML("&nbsp;&nbsp;&nbsp;"),

            # Button linking straight to the About page
            actionButton(
              inputId = "about",
              label = "About",
              class = "btn-info btn-lg btn-tooltip btn-hidden",
              style = "width: 155px",
              `data-position` = "bottom",
              title = "Learn more about MetaBridge."
            )
          )
        )
      ),
      # Separate div to include the lab logo which serves as a link leading to
      # the lab website
      tags$div(
        style = "position:fixed; bottom:0px; padding-bottom:10px",
        htmltools::HTML(paste0(
          "<a href='http://cmdr.ubc.ca/bobh/'> ",
          "<img src='pics/hancock-lab-logo.svg'> </a>"
        ))
      )
    ),


    # * 3.3 Upload panel --------------------------------------------------

    tabPanel(
      "Upload",
      value = "uploadPanel",

      # Sidebar
      tags$div(
        class = "col-sm-3 manual-sidebar",

        # Separate form 'wells' within the sidebar (custom CSS class)
        tags$form(
          class = "well",
          tags$label("Upload your Metabolites"),

          tags$p(HTML(
            "Select a plain-text spreadsheet (a file ending in csv, txt, or ",
            "tsv) containing your metabolites of interest in a single column. ",
            "You can also try our example data using the button below."
          ), style = "padding-bottom: 5px;"),

          # Upload handling. Note that the "Browse..." button is customized in
          # "www/css/user.css". We set the the label to NULL so we can include
          # an icon along with the text via the "buttonLabel" argument.
          fileInput(
            inputId = "metaboliteUpload",
            label   = NULL,
            buttonLabel = list(icon("upload"), "Browse..."),
            accept  = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv",
              "text/tsv",
              "text/tab-separated-values,text/plain",
              ".tsv",
              "text/txt",
              "text/tab-separated-values,text/plain",
              ".txt"
            )
          ),

          # Header in file?
          tags$p(
            HTML("<b>Does your data contain column names?</b>"),
            style = "margin-bottom: 0;"
          ),
          checkboxInput(
            inputId = "header",
            label   = "My data has a header",
            value   = TRUE
          ),

          # Tab- or comma-separated?
          radioButtons(
            inputId = "sep",
            label = "How is your data separated?",
            choices = c(Comma = ",", Tab = "\t", Semicolon = ";"),
            selected = ","
          ),

          tags$br(),

          # Or try our example data
          actionButton(
            inputId = "tryExamples",
            class   = "btn-secondary btn-tooltip",
            `data-position` = "right",
            label   = tags$b("Load Example Data"),
            title   = "Try an example dataset from MetaboAnalyst"
          )
        ),

        # Show the columns of the uploaded file
        uiOutput("columnPickerPanel")
      ),

      # Display the file that was uploaded
      uiOutput("uploadedTablePanel")
    ),


    # * 3.4 Mapping Panel -------------------------------------------------

    tabPanel(
      title = "Map",
      value = "mapPanel",

      # Manual Sidebar
      tags$div(
        class = "col-sm-3 manual-sidebar",
        id = "mapPanelSidebar",

        tags$form(
          class = "well",
          tags$label("Choose a Database"),

          tags$p(HTML(
            "Select one of the options below. MetaCyc has higher quality ",
            "annotations, but KEGG may yield more hits, and will also allow ",
            "you to visualize your results with <b>Pathview</b>."
          ), style = "padding-bottom: 5px;"),

          # Choose database for mapping.
          radioButtons(
            "dbChosen",
            label    = NULL,
            choices  = c("MetaCyc", "KEGG"),
            selected = "MetaCyc"
          ),

          tags$br(),

          # Map!
          actionButton(
            "mapButton",
            tags$b("Map Metabolites"),
            class = "btn-primary btn-tooltip",
            `data-position` = "right",
            title = "Map your metabolites to the selected database",
            icon = icon("arrows-alt")
          )
        ),

        # Let the user download their results
        uiOutput("saveMappingPanel"),

        # Show panel for continuing to visualization results
        uiOutput("continueToViz")
      ),

      # Display mapping results
      tags$div(
        class = "col-sm-9",

        # Show summary table.
        uiOutput("mappingSummaryPanel"),

        # Show FULL results for a selected metabolite.
        uiOutput("fullMappingResultsPanel")
      )
    ),


    # * 3.5 Visualize With Pathview ---------------------------------------

    tabPanel(
      title = "Pathview",
      value = "vizPanel",
      id = "visualizationPanel",
      class = "viz-panel",
      uiOutput("vizPanelUI")
    ),

    # * 3.6 Help Panel & dropdown -----------------------------------------

    navbarMenu(
      title = "Help",

      # * * 3.6.1 Tutorial Page -------------------------------------------

      tabPanel(
        title = "Tutorial",
        value = "tutorialPanel",

        tags$div(
          class = "jumbotron",

          tags$h1("Tutorial"),

          tags$hr(),

          tags$div(
            class = "logoWrapper",

            tags$h2("Network-Based Integrative Analysis with MetaBridge"),

            tags$p(
              "This page covers a sample workflow for integrating your ",
              "metabolomics data with transcriptomics or proteomics data via ",
              "network methodologies. You can also view this tutorial on our",
              HTML(paste0(
                "<a href='https://github.com/travis-m-blimkie/MetaBridgeShiny/",
                "blob/master/tutorial/tutorial.md' target='_blank'>GitHub page.</a>"
              ))
            ),

            tags$ol(
              tags$li(tags$a(
                "Metabolite Preprocessing",
                href = "#metabolite-preprocessing"
              )),
              tags$li(tags$a(
                "MetaBridge Mapping",
                href = "#metabridge-mapping"
              )),
              tags$li(tags$a(
                "NetworkAnalyst",
                href = "#networkanalyst"
              )),
              style = "font-size: 1.34em;"
            )
          )
        ),

        div(
          class = "col-lg-10 tutorial",
          includeMarkdown("tutorial/tutorial.md")
        )
      ),

      # * * 3.6.2 About page ----------------------------------------------

      tabPanel(
        value = "aboutPanel",
        title = "About",

        tags$div(
          class = "jumbotron",

          tags$h1("About"),

          tags$hr(),

          tags$div(
            class = "logoWrapper",

            tags$p(
              "MetaBridge was designed by Samuel Hinshaw, and is maintained ",
              "by Travis Blimkie at the ",
              tags$a(
                href = "http://cmdr.ubc.ca/bobh/",
                "Centre for Microbial Diseases and Immunity Research"
              ),
              " at The University of British Columbia. It was ",
              "originally published in ", tags$em("Bioinformatics"),
              " (doi: ",
              tags$a(
                href = "https://doi.org/10.1093/bioinformatics/bty331",
                "10.1093/bioinformatics/bty331",
                .noWS = "after"
              ),
              "); please cite this paper when using MetaBridge in your analyses."
            ),

            tags$p(
              "We also have a protocol on how to use MetaBridge published in ",
              tags$em("Current Protocols in Bioinformatics."), "It covers how ",
              "to prepare data for input to MetaBridge, and includes an ",
              "example of building a protein-protein interaction network from ",
              "MetaBridge results using",
              tags$a(
                href = "https://networkanalyst.ca",
                "NetworkAnalyst."
              ),
              "The article is available at doi:",
              tags$a(
                href = "https://doi.org/10.1002/cpbi.98",
                "10.1002/cpbi.98."
              )
            ),

            tags$p(
              "If you encounter any bugs or run into other troubles, you can ",
              "post an issue at the ",
              tags$a(
                href = "https://github.com/hancockinformatics/MetaBridgeShiny/issues",
                "GitHub page."
              ),
            ),

            tags$p(
              "MetaBridge uses the following databases and R packages:"
            ),

            tags$p(
              tags$dl(

                # MetaCyc
                tags$dt(
                  tags$a(href = "https://metacyc.org/", "MetaCyc v23"),
                  tags$dd("Curated database for human metabolomic data.")
                ),

                # KEGG
                tags$dt(
                  tags$a(href = "https://www.genome.jp/kegg/", "KEGG Release 92"),
                  tags$dd("Large database containing multiple data types.")
                ),

                # Shiny
                tags$dt(
                  tags$a(href = "https://shiny.rstudio.com/", "Shiny"),
                  tags$dd("Web application framework for R.")
                ),

                # ShinyCSSLoaders
                tags$dt(
                  tags$a(href = "https://github.com/andrewsali/shinycssloaders", "shinycssloaders"),
                  tags$dd("Animated loaders for shiny outputs.")
                ),

                # ShinyJS
                tags$dt(
                  tags$a(href = "https://deanattali.com/shinyjs/", "shinyjs"),
                  tags$dd("Improve the user experience of your Shiny apps in seconds.")
                ),

                # Tidyverse
                tags$dt(
                  tags$a(href = "https://www.tidyverse.org/", "tidyverse"),
                  tags$dd("A collection of R packages designed for data science.")
                ),

                # Pathview
                tags$dt(
                  tags$a(href = "https://doi.org/10.1093/bioinformatics/btt285", "Pathview"),
                  tags$dd("Pathway-based data integration and visualization.")
                )
              )
            )
          )
        )
      )
    )
  ),
  tags$script(src = "js/tippy.min.js"),
  tags$script(src = "js/client.js")
)




# 4. Define the server code -----------------------------------------------

server <- function(input, output, session) {

  # Wait for sessionInitialized to load packages. This does not have to be
  # defined in your UI, as the input will be passed via `Shiny.onInputChange()`
  observeEvent(input$sessionInitialized, {
    source("deferred.R")

    # After packages loaded, run button transform to signal ready states.
    runjs("handlers.initGetStarted();")
  }, ignoreInit = TRUE, once = TRUE)



  # Define reactive variables. These are isolated into individual reactive
  # values so we can depend on them for reactive changes.
  metaboliteObject <- reactiveVal()
  mappedMetabolites <- reactiveVal()
  mappingObject <- reactiveVal()
  mappingSummary <- reactiveValues(table = NULL, dbChosen = NULL)
  mappedMetaboliteTable <- reactiveVal()
  preSelectedIDType <- reactiveVal()
  databaseChosen <- reactiveVal()
  selectedMetab <- reactiveVal()
  idTypeChosen <- reactiveVal()
  columnPicked <- reactiveVal()
  hmdbCol <- reactiveVal()



  # 4.1 Welcome tab handlers ----------------------------------------------

  # When clicking "Get Started", switch to `Upload` panel
  observeEvent(input$getStarted, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "uploadPanel")
  }, ignoreInit = TRUE)

  # When clicking "Tutorial", switch to `Tutorial` panel
  observeEvent(input$tutorial, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "tutorialPanel")
  }, ignoreInit = TRUE)

  # When clicking "About", switch to the `About` panel
  observeEvent(input$about, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "aboutPanel")
  }, ignoreInit = TRUE)



  # 4.2 Upload tab handlers -----------------------------------------------

  # Inject example data frame when "Try Examples" is clicked
  observeEvent(input$tryExamples, {
    # Input examples...
    metaboliteObject(examples_2)
    # ...and wipe mapping objects
    mappingObject(NULL)
    mappedMetabolites(NULL)
    mappingObject(NULL)
    mappingSummary$table <- NULL
    mappingSummary$dbChosen <- NULL
    mappedMetaboliteTable(NULL)
    databaseChosen(NULL)
  }, ignoreInit = TRUE)

  # Read file when any of fileInput, checkboxInput, or radioButtons states
  # change
  observeEvent({
    input$metaboliteUpload
    input$sep
    input$header
  }, {
    if (!is.null(input$metaboliteUpload)) {
      # Save to the reactiveVal...
      read_delim(
        file = input$metaboliteUpload$datapath,
        col_names = input$header,
        delim = input$sep
      ) %>% metaboliteObject()
      # ...then wipe mapping objects for a fresh start.
      mappingObject(NULL)
      mappedMetabolites(NULL)
      mappingObject(NULL)
      mappingSummary$table <- NULL
      mappingSummary$dbChosen <- NULL
      mappedMetaboliteTable(NULL)
      databaseChosen(NULL)
    }
  }, ignoreInit = TRUE)


  # Once data is populated, render help text to the user,...
  output$uploadSuccess <- renderUI({
    input$tryExamples # ...making sure the "Try Examples" button is a dependency
    if (is.null(metaboliteObject())) {
      return(NULL)
    }
    tagList(
      tags$h2(
        "Input Data Preview & ID Selection",
        style = "margin-top: 0; margin-bottom: 0;"
      ),
      tags$h4(
        class = "conditional-help",
        HTML(
          # "Check below to see if your data was loaded correctly. If so, click ",
          # "a column to <b><u>highlight it in blue</u></b>, select the ",
          # "matching ID type in the lower left box, then continue via the ",
          # "<b>Proceed</b> button."
          "If your data has loaded correctly, click a column to ",
          "<b><u>highlight it in blue</u></b>, select the matching ID type in ",
          "the lower left box, then continue via the <b>Proceed</b> button."
        )
      ),
      tags$br()
    )
  })


  # Once the data is populated, render a preview of the data to the user.
  output$uploadedDataTable <- DT::renderDataTable({
    input$tryExamples
    input$sep
    input$header
    input$metaboliteUpload
    if (is.null(metaboliteObject())) {
      # Return `NULL` if nothing present so that we don't pass an error.
      return(NULL)
    } else {
      # Render the `uploadedDataTable()`.
      metaboliteObject()
    }
  },
  # DataTable options. We only need to provide options which we want different
  # from our defaults defined in "deferred.R".
  options = list(
    scrollY = "50vh"
  ),
  rownames = FALSE,
  selection = list(
    mode     = "single",
    target   = "column"
    # selected = 0
  ),
  style = "bootstrap",
  class = "table-bordered table-responsive"
  )

  output$uploadedTablePanel <- renderUI({
    tags$div(
      class = "col-sm-9",
      uiOutput("uploadSuccess"),
      DT::dataTableOutput("uploadedDataTable"),

      # tags$hr(),
      # tags$p("ID"),
      # verbatimTextOutput("chosen_id"),
      # tags$p("Col"),
      # verbatimTextOutput("picked_column")
    )
  })

  observeEvent({
    input$uploadedDataTable_columns_selected
    metaboliteObject()
  }, {
    # Wait 500ms after panel render and re-activate tooltips.
    runjs("setTimeout(() => { handlers.activateTooltips(['.panel-tooltip', '.btn-tooltip']); }, 100)")
  })

  # This has to be rendered separately from the column picker panel. Otherwise,
  # the entire column picker panel has to be re-rendered when the preselected
  # ID type gets updated, which resets the entire panel, which reverts to the
  # preselected column, effectively making it impossible to switch columns!

  # Note that enabling the auto selection via the "selected" argument causes an
  # issue with the Proceed button enabling, so it's been left out for now.
  output$idSelector <- renderUI({
    tags$form(
      class = "well",

      tags$label("Select an ID Type"),

      tags$p(HTML(
        "MetaBridge supports mapping with HMDB or KEGG metabolite IDs. Ensure ",
        "the ID selected here matches the column <b><u>highlighted in ",
        "blue</u></b> before clicking the <b>Proceed</b> button."
      ), style = "padding-bottom: 5px;"),

      radioButtons(
        inputId   = "idType",
        label     = NULL,
        choices   = c("HMDB", "KEGG"),
        selected  = character(0)
        # selected  = preSelectedIDType()
      ),

      tags$br(),

      # Include button to proceed, which is disabled until a column is selected
      # and the appropriate ID type is chosen
      disabled(
        actionButton(
          inputId = "continueToMap",
          label   = tags$b("Proceed"),
          class   = "btn-primary btn-tooltip",
          icon    = icon("check"),
          title   = "Continue to the mapping step"
        )
      )
    )
  })


  # Render the UI for the column picker panel
  columnPickerUI <- eventReactive({
    # Change on button click (uploaded file or example data)...
    input$tryExamples
    input$metaboliteUpload
    # ...or on header and separator change.
    input$sep
    input$header
  }, {
    # Only render if NOT `NULL`.
    if (!is.null(metaboliteObject())) {
      tags$form(
        class = "well",
        # Dynamically render the `idType()` selector panel here (see below). This
        # is intentionally separate so that we do not have a feedback loop that
        # triggers re-rendering. Otherwise, as soon as you change this value,
        # the entire panel re-renders, switching it back to its default.
        uiOutput("idSelector")
      )
    }
  })


  observeEvent({
  	input$uploadedDataTable_columns_selected
  	input$sep
  	input$header
  	}, {
    # DataTables indexes by 0, so we add one...
    columnIndex <- input$uploadedDataTable_columns_selected + 1
    # ...then pick the column name!
    columnName <- colnames(metaboliteObject())[columnIndex]
    columnPicked(columnName)
  })


  # Grab some reactive variable to check their values, printed under the input
  # data preview. Just here to aid with development and testing.
  output$picked_column <- renderPrint(columnPicked())
  output$chosen_id <- renderPrint(input$idType)


  # When data is populated, show column picker panel for users to select. This
  # is separate from the actual code to render so that we can only depend on
  # specific events triggering re-renders.
  output$columnPickerPanel <- renderUI({
    columnPickerUI()
  })


  # If the selected ID type is a column name in the data frame, preselect that
  # column for use in mapping. Check that we have a column selected first,
  # otherwise the second if statement causes an error and the app crashes.
  # Note that the current app version does not actually employ this section, a
  # it interferes with the enabling/disabling of the Proceed button (FIX?)
  observeEvent(columnPicked(), {
    if (length(columnPicked()) != 0) {
      if (tolower(columnPicked()) %in% c("hmdb", "kegg")) {
        preSelectedIDType(columnPicked())
      }
    }
  }, ignoreInit = TRUE)



  # Since the Proceed button starts disabled, we need to enable it under the
  # proper circumstances. The user needs to have chosen an ID type to use in the
  # mapping, and selected a column before the Proceed button is enabled. The
  # closing `else()` is included so that if the conditions are met, then un-met,
  # (i.e. click on the same column twice) the button is again disabled.
  observeEvent({
    columnPicked()
    input$idType
  }, {
    if (all( length(columnPicked()) != 0 & !is.null(input$idType) )) {
      enable("continueToMap")
    } else {
      disable("continueToMap")
    }
  })

  # Switch to `Map` panel when "Proceed" is clicked on the `Upload` tab
  observeEvent(input$continueToMap, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "mapPanel")
  }, ignoreInit = TRUE)



  # 4.3 Map tab handlers ----------------------------------------------------

  # Store ID type chosen as a reactive variable which only changes when the
  # "Map" button is clicked
  observeEvent(input$mapButton, {
    idTypeChosen(input$idType)
  })

  # Here's where the heavy lifting takes place! We now take the column the user
  # specified and map the IDs to genes.

  # When the map button is clicked, update the `dbChosen()`.
  observeEvent(input$mapButton, {
    databaseChosen(input$dbChosen)

    # Clear any pre-existing alerts
    removeUI(selector = "#mappingAlert")

    # Conduct the mapping with our `mapGenerally()` function defined in
    # "functions/mapGenerally.R".
    mappingOutput <- mapGenerally(
      importDF = metaboliteObject(),
      col      = columnPicked(),
      db       = databaseChosen(),
      idType   = idTypeChosen()
    )

    # Assign just the mapped data to our reactive value...
    mappedMetabolites(mappingOutput$data)

    # ...and assign the full object (data plus status, errors, etc.) so we can
    # access the status reports later.
    mappingObject(mappingOutput)

    # Create new alert bubble with the status message.
    mappingAlert(
      status  = mappingOutput$status,
      message = mappingOutput$message,
      suggest = mappingOutput$suggest
    )
  }, ignoreInit = TRUE)


  # 4.4 Render summarized mapping table -----------------------------------

  # 1. Generate table from `generateSummaryTable()`, depending only on the
  #    mapButton click.
  # 2. Render the generated table with DT::renderDataTable(). This is separate
  #    from #1 because we need to assign the reactive table object to its own
  #    output object.
  # 3. Render the entire UI surrounding the table and insert the rendered DT.

  # 1. Generate Table
  # ~~~~~~~~~~
  # Show a summary table of the mapped metabolites (just number of genes, etc.)
  # This calls `generateSummaryTable()` from "functions/generateTables.R" and
  # only renders when the "Map" button is clicked
  observeEvent(input$mapButton, {
    results <- generateSummaryTable(
      mappingObject(),
      idTypeChosen(),
      databaseChosen()
    )
    mappingSummary$table <- results$table
    mappingSummary$dbChosen <- results$dbChosen
  })

  # 2. Render Generated Table
  # ~~~~~~~~~~
  # Once metabolites have been mapped, render the results.
  output$mappingSummaryTable <- DT::renderDataTable({
    mappingSummary$table %>% hyperlinkTable(databaseChosen())
  },
  rownames = FALSE,
  style = "bootstrap",
  class = "table-bordered table-responsive compact",
  escape = FALSE,
  selection = "single"
  )

  # 3. Render UI
  # ~~~~~~~~~~
  # Render the panel separately so we have reactive control over all the UI
  # elements surrounding the table, not just the table itself.
  output$mappingSummaryPanel <- renderUI({
    # Make sure this depends on the summary table (and thus updates every time
    # the summary table does).
    mappingSummary$table
    # Now proceed
    if (is.null(mappingObject())) {
      return(NULL)
    } else if (mappingObject()$status == "error" | mappingObject()$status == "empty") {
      return(NULL)
      # Only render if we had non-null, non-error, non-empty results.
    } else {
      return(
        tagList(
          tags$h3(paste0("Mapping Summary - ", databaseChosen()), class = "tab-header"),
          # Insert the datatable here that we rendered above.
          DT::dataTableOutput("mappingSummaryTable")
        )
      )
    }
  })

  # When a new metabolite is selected, set it to the selected metabolite
  # reactive value! Why? So we can reset it given other certain conditions (see
  # the next function).
  observeEvent(input$mappingSummaryTable_rows_selected, {
    selectedMetab(input$mappingSummaryTable_rows_selected)
  })

  # But when the map button is selected, nullify any previously selected
  # metabolite.
  observeEvent(input$mapButton, {
    selectedMetab(NULL)
  })


  # 4.5 Render metabolite-specific table ----------------------------------

  # THREE STEP RENDER PROCESS, PART 2 - METABOLITE SPECIFIC TABLE
  # 1. Generate table from `generateTables.R::generateSummaryTable()`, depending
  #    only on the mapButton click.
  # 2. Render the generated table with DT::renderDataTable(). This is separate
  #    from #1 because we need to assign the reactive table object to its own
  #    output Object.
  # 3. Render the entire UI surrounding the table and insert the rendered DT.


  # 1. Generate Table
  # ~~~~~~~~~~
  # Now, show the filtered (unsummarized) table, based on what metabolite user
  # clicked on.
  observeEvent({
    # Update when we select a new metabolite in the summary table...
    selectedMetab()
    # ...or when we click the map button (this is important because we need to
    # be able to update in case there are errors we need to display).
    input$mapButton
  }, {
    # Pull the `$data` object from the `tryCatch()` output if there was an
    # error. This should default to the previous successful step.
    if (mappingObject()$status == "error" | mappingObject()$status == "empty") {
      mappingObject()$data %>% mappedMetaboliteTable()

      # Otherwise, generate our table depending on the chosen database! As with
      # `generateSummaryTable()`, these functions come from "generateTables.R"

    } else if (databaseChosen() == "KEGG") {
      if (mappingSummary$dbChosen != "KEGG") {
        cat("DATABASE WAS NOT KEGG, NULL RETURNING...")
        # If our summary table was somehow not updated yet, exit.
        return(NULL)
      } else {
        generateKEGGMetabTable(
          mappingObject(),
          mappingSummary$table,
          selectedMetab(),
          idTypeChosen()
        ) %>% mappedMetaboliteTable()
        # Otherwise proceed with generated the metabolite table.
      }

    } else if (databaseChosen() == "MetaCyc") {
      # If our summary table was somehow not updated yet, exit.
      if (mappingSummary$dbChosen != "MetaCyc") {
        cat("DATABASE WAS NOT METACYC, NULL RETURNING...")
        return(NULL)
        # Otherwise proceed with generated the metabolite table.
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

  # 2. Render Generated Table
  # ~~~~~~~~~~
  # Once metabolites have been mapped, render the results!
  output$mappedMetaboliteTable <- DT::renderDataTable({
    if (is.null(mappingObject()) | is.null(selectedMetab())) {
      return(data.frame())

    } else if (mappingObject()$status == "success") {
      # Only render if we had non-null, non-error, non-empty results.
      mappedMetaboliteTable() %>% hyperlinkTable(databaseChosen())
    }
  },
  rownames = FALSE,
  style = "bootstrap",
  class = "table-bordered table-responsive compact",
  escape = FALSE,
  selection = "single"
  )

  # 3. Render UI
  # ~~~~~~~~~~
  # Now render the whole UI that surrounds the table, along with the table
  # itself
  output$fullMappingResultsPanel <- renderUI({
    tags$div(
      if (is.null(mappingObject())) {
        return(NULL)
        # If we had an error, change the header to reflect that these are
        # intermediate results.

      } else if (
        mappingObject()$status == "error" | mappingObject()$status == "empty"
      ) {
        tags$h3("Intermediate Results")
        # Only render if we had non-null, non-error, non-empty results.

      } else {
        tagList(
          tags$hr(),
          tags$h3("Per-Metabolite Mapping Results")
        )
      },
      # Rendered table from "2." goes here!
      DT::dataTableOutput("mappedMetaboliteTable")
    )
  })



  # 4.6 Render sidebar to save results ------------------------------------

  # Watch for the "Try Again" button that will be rendered if an error occurs in
  # the mapping.
  observeEvent(input$remap, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "uploadPanel")
  }, ignoreInit = TRUE)

  # Once table exists, render the panel with the Download button.
  output$saveMappingPanel <- renderUI({
    if (!is.null(mappedMetabolites())) {

      tags$form(
        class = "well",
        tags$label("Download your Results"),

        tags$p("Use the button below to download your full mapping results as ",
        "a tab-delimited file."),

        downloadButton(
          "downloadMappingData",
          tags$b("Download Results"),
          class = "btn-info btn-tooltip btn-right",
          `data-position` = "right",
          title = "Download your full mapping results",
        ),

        # These breaks are only needed because we right-align the download
        # button. Without them, the button will not stay within the bounds of
        # the form/well UI object.
        tags$br(),
        tags$br()
      )
    }
  })


  # 4.7 Clean and export the data -------------------------------------------

  # Cleaning the mapped MetaCyc (not KEGG) data before download, to remove HTML
  # tags from reactions. Specifically, we are removing any HTML tags, using
  # plain text arrows, and switching Greek letters to English versions.
  cleanMappedMetabolites <- reactive({
    req(mappedMetabolites())

    if (databaseChosen() == "MetaCyc") {
      mappedMetabolites() %>% cleanReactions(.)
    } else {
      mappedMetabolites()
    }
  })

  # Export the data.
  output$downloadMappingData <- downloadHandler(
    # Name file format: `originalfilename_mapped_dbChosen.savetype`.
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
        # input$saveType
      )
    },
    content = function(file) {
      write_delim(
        cleanMappedMetabolites(),
        file,
        delim = "\t"
        # delim = switch(
        #   input$saveType,
        #   "csv" = ",",
        #   "tsv" = "\t"
        # )
      )
    }
  )



  # 4.8 Add navigation to Viz tab -----------------------------------------

  # Navigate to the "Visualize" page when KEGG was the chosen database.
  output$continueToViz <- renderUI({
    # Do not render panel if no database has been mapped against yet, because
    # `databaseChosen()` does not get `input$dbChosen` until the "Map" button
    # is clicked.
    if (is.null(databaseChosen())) {
      return(NULL)

      # Once mapped, render the panel.
    } else if (databaseChosen() == "MetaCyc") {
      return(NULL)
    } else {
      tags$form(
        class = "well",
        tags$label("Visualize your Results"),

        tags$p(HTML(
          "If you mapped against KEGG, you have the option",
          "to visualize your results with <b>Pathview</b>."
        )),

        tags$br(),
        tags$br(),

        # If we mapped against KEGG, show "Visualize" button.
        if (databaseChosen() == "KEGG" & !is.null(selectedMetab())) {
          actionButton(
            inputId = "visualizeButton",
            label   = tags$b("Visualize"),
            class   = "btn-med btn-primary btn-tooltip",
            title   = "Visualize your results with pathview",
            icon    = icon("eye")
          )
        # But if we mapped against MetaCyc disable the "Visualize" button.
        } else {
          actionButton(
            inputId = "visualizeButton",
            label   = tags$b("Visualize"),
            class   = "btn-med btn-tooltip disabled",
            title   = "Select a metabolite from the summary table",
            icon    = icon("eye")
          )
        }
      )
    }
  })

  # Client-side JS to enable/disable "Visualize" tab. Also disables the
  # "Visualize" tab in the navbar when visualization is not possible. Make sure
  # that we have a tooltip explaining why the "Visualization" tab is disabled. A
  # lot of this refers to code in "www/js/client.js".
  observeEvent(input$mapButton, {
    if (databaseChosen() == "KEGG" & !is.null(selectedMetab())) {
      runjs("$(\"a[data-value='vizPanel']\").parent().removeClass('disabled');")
      runjs("$(\"a[data-value='vizPanel']\").removeClass('panel-tooltip');")
    } else {
      runjs("$(\"a[data-value='vizPanel']\").parent().addClass('disabled');")
      runjs("$(\"a[data-value='vizPanel']\").addClass('panel-tooltip');")
    }
  })

  # Client-side JS to enable/disable "Visualization" tab!
  observeEvent(input$mappingSummaryTable_rows_selected, {
    if (databaseChosen() == "KEGG" & !is.null(selectedMetab())) {
      runjs("$(\"a[data-value='vizPanel']\").parent().removeClass('disabled');")
      runjs("$(\"a[data-value='vizPanel']\").removeClass('panel-tooltip');")
    } else {
      runjs("$(\"a[data-value='vizPanel']\").parent().addClass('disabled');")
      runjs("$(\"a[data-value='vizPanel']\").addClass('panel-tooltip');")
    }
  })

  # When clicking "Visualize", switch to the "Visualize" panel.
  observeEvent(input$visualizeButton, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "vizPanel")
  }, ignoreInit = TRUE)


  # 4.9 Viz tab handlers ----------------------------------------------------

  # Set up reactive values for:
  # - The selected compound of the clicked row
  # - The pathways that compound is involved in
  # - The genes (for the enzymes) that compound interacts with
  selectedRowAttrs <- reactiveValues(
    "selectedCompound" = NULL,
    "selectedCompoundName" = NULL,
    "pathwaysOfSelectedCompound" = NULL,
    "genesOfSelectedCompound" = NULL
  )

  # Now, when the selected row changes...
  observeEvent(input$mappingSummaryTable_rows_selected, {

    # ...map!
    pathwayMappingAttrs <- generalPathwayMapping(
      summaryTable = mappingSummary$table,

      # The fullTable provided used to be the table that was rendered just for
      # the selected metabolites. This means that the only genes were those
      # identified for the selected metabolites. For now, I have fixed this by
      # including all genes in the mapping. HOWEVER, in the future it could be
      # interesting to create a toggle that would let the user specify which
      # they would prefer. In that case, we would have to make sure that
      # mappedMetaboliteTable() was updated before the pathway mapping function
      # was called.

      # fullTable = mappedMetaboliteTable(),
      fullTable = mappingObject()$data,
      idType = idTypeChosen(),
      db = databaseChosen(),
      selectedRow = selectedMetab()
    )

    # Assign results to their reactive values
    selectedRowAttrs$selectedCompound <-
      pathwayMappingAttrs$selectedCompound

    selectedRowAttrs$selectedCompoundName <-
      pathwayMappingAttrs$selectedCompoundName

    selectedRowAttrs$genesOfSelectedCompound <-
      pathwayMappingAttrs$genesOfSelectedCompound

    selectedRowAttrs$pathwaysOfSelectedCompound <-
      pathwayMappingAttrs$pathwaysOfSelectedCompound
  })

  # Render the pathway panel once.
  output$pathwayPanel <- renderUI({
    # Check for results before rendering!
    if (nrow(selectedRowAttrs$pathwaysOfSelectedCompound) == 0) {
      tags$div(
        tags$h4(tags$b(
          paste0("Pathways for ", selectedRowAttrs$selectedCompoundName)
        )),
        tags$p("No pathways found for this compound.")
      )
    } else if (databaseChosen() == "KEGG") {
      tags$div(

        tags$h4(tags$b(paste0(
          "Pathways for ",
          str_to_title(selectedRowAttrs$selectedCompoundName)
        ))),

        selectInput(
          inputId = "pathwaysPicked",
          label = "",
          choices = selectedRowAttrs$pathwaysOfSelectedCompound$namedPway,
          selectize = FALSE
        ),

        tags$p(HTML("Note that each pathway may take some time to process. ",
        "For each pathway, only the compound selected is shown, but <b>ALL",
        "</b> mapped genes are shown."
        ))
      )
    } else if (databaseChosen() == "MetaCyc") {
      tags$div(
        tags$h4(paste0(
          "Pathways for ",
          selectedRowAttrs$selectedCompoundName
        )),
        selectInput(
          inputId = "pathwaysPicked",
          label = "",
          choices = selectedRowAttrs$pathwaysOfSelectedCompound$pathwayName,
          selectize = FALSE
        )
      )
    }
  })

  output$pathwayView <- renderImage({
    if (is.null(input$pathwaysPicked)) {
      return({
        list(
          src = "./logo_background.svg",
          contentType = "image/svg",
          width  = 512,
          height = 512,
          alt = "pathway placeholder"
        )
      })
    }

    # Setup named variables for standard evaluation.
    pathwayNameIDcol <- as.name("namedPway")
    selectedPathway <- quo(input$pathwaysPicked)

    # Pull the pathway ID from the pathway name selected by the user.
    selectedPathwayID <-
      selectedRowAttrs$pathwaysOfSelectedCompound %>%
      filter(!!(pathwayNameIDcol) == input$pathwaysPicked) %>%
      extract2("id")

    filename <- visualizePathview(
      pathway = selectedPathwayID,
      genes = selectedRowAttrs$genesOfSelectedCompound,
      cpd = selectedRowAttrs$selectedCompound
    )

    # Return a list containing the file name. Render image at 1000px and then
    # constrain image to `div` in CSS.
    return(list(
      src = filename,
      contentType = "image/png",
      width = 1000,
      # height = imageHeight(),
      alt = paste0("Pathway map of KEGG Pathway ", input$pathwaysPicked)
    ))
  }, deleteFile = TRUE)


  # Render entire UI for `vizPanel`.
  output$vizPanelUI <- renderUI({
    if (is.null(databaseChosen())) {
      tags$div(
        tags$h2("Pathway View", class = "tab-header"),
        # "Nothing to map" alert
        tags$div(
          class = "alert alert-dismissible alert-danger",
          tags$button(
            HTML("&times;"),
            type = "button",
            class = "close",
            `data-dismiss` = "alert"
          ),
          "There is nothing selected to map!"
        )
      )
    } else if (databaseChosen() == "MetaCyc") {
      tags$div(
        tags$h2("Pathway View", class = "tab-header"),
        # "Nothing to map" alert
        tags$div(
          class = "alert alert-dismissible alert-danger",
          tags$button(
            HTML("&times;"),
            type = "button",
            class = "close",
            `data-dismiss` = "alert"
          ),
          "You must map via KEGG to visualize your results with pathview!"
        )
      )
    } else if (is.null(selectedMetab())) {
      tags$div(
        tags$h2("Pathway View", class = "tab-header"),
        # "Nothing to map" alert
        tags$div(
          class = "alert alert-dismissible alert-danger",
          tags$button(
            HTML("&times;"),
            type = "button",
            class = "close",
            `data-dismiss` = "alert"
          ),
          "You must select a metabolite to visualize your results with pathview!"
        )
      )
    } else {
      tags$div(
        # Manual Sidebar
        tags$div(
          class = "col-sm-3 manual-sidebar",
          # Allow user to pick which pathway that the selected metabolite
          # participates in to view.
          tags$form(
            class = "well",
            uiOutput("pathwayPanel")
          )
        ),

        # Pathway visualization
        tags$div(
          class = "col-sm-9",
          tags$h2("Pathway View", class = "tab-header"),
          imageOutput("pathwayView") %>% withSpinner(type = 8, color = "#303E4E"),
          tags$br()
        )
      )
    }
  })
}


# 5. Finally, run the app! ---------------------------------------------------

shinyApp(ui, server)
