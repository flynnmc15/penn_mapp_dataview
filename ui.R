# header <- dashboardHeader(
#   title = a(
#     target = "_blank",
#     style = "background-color: #E0E3F4;",
#     img(
#       src = "mapp_logo.png",
#       style = "height: 100%;",
#       align = "left"
#     )
#   ),
#   titleWidth = "100%"
# )

intro_panel <- tabPanel(
  "Introduction",
  
  img(
    src = "MAPPDV_opening_page.PNG",
    width = "95%",
    alt = "MAPP DataView is an application that allows users to query data from the Multidisciplinary Approach to the Study of Chronic Pelvic Pain (MAPP) Study 
  and view the query results in tabular or graphic representation. Users can obtain summary statistics, including bar charts, boxplots, frequency 
  tabulations, and measures of central tendency and dispersion, on single variables, for the entire sample or for subgroups defined by levels of up to 
  three stratification variables. The MAPP DataView database is updated regularly to incorporate newly obtained follow-up data."
  ),
  
  #  titlePanel("Welcome to the MAPP Study!"),
  #  p("MAPP DataView is an application that allows users to query data from the Multidisciplinary Approach to the Study of Chronic Pelvic Pain (MAPP) Study 
  #  and view the query results in tabular or graphic representation. Users can obtain summary statistics, including bar charts, boxplots, frequency 
  #  tabulations, and measures of central tendency and dispersion, on single variables, for the entire sample or for subgroups defined by levels of up to 
  #  three stratification variables. The MAPP DataView database is updated regularly to incorporate newly obtained follow-up data."),
  
  #  h2("The MAPP Study"),
  # p("To help better understand the underlying causes of the two most prominent chronic urological pain disorders–interstitial cystitis/ bladder pain 
  #  syndrome (IC/BPS) and chronic prostatitis/chronic pelvic pain syndrome (CP/CPPS), in 2008 the National Institute of Diabetes and Digestive and Kidney 
  #  Diseases (NIDDK) of the National Institutes of Health (NIH) established the Multidisciplinary Approach to the Study of Chronic Pelvic Pain (MAPP) 
  #  Research Network."),
  
  #  p("The MAPP Research Network embraces a systemic– or whole–body–approach in the study of Urologic Chronic Pelvic Pain Syndrome (UCPPS). UCPPS is a term  
  #  adopted by the network to encompass both IC/BPS and CP/CPPS, which are proposed as related based on their similar symptom profiles. 
  #  In addition to moving beyond traditional bladder– and prostate-specific research directions, MAPP Network scientists are 
  #  investigating potential relationships between UCPPS and other chronic conditions that are sometimes seen in IC/PBS and CP/CPPS patients, 
  #  such as irritable bowel syndrome, fibromyalgia, and chronic fatigue syndrome."),
  
  #  p("The primary clinical research effort carried out during the MAPP Network’s the first 5-year project period (MAPP I) was a prospective 
  #  cohort study, the Trans-MAPP Epidemiology/Phenotyping (EP) Study. From 12/14/2009 through 12/14/2012 1,039 men and women were enrolled, 
  #  including persons with UCPPS (n=424); persons with other co-morbid illnesses, including fibromyalgia, irritable bowel syndrome, and 
  #  chronic fatigue syndrome (n=200 for all conditions); and healthy controls (n=415). All study participants were extensively characterized 
  #  (i.e., phenotyped) at baseline, and UCPPS participants were further assessed during an additional 12 month follow-up period."),
  
  # p("Initial analyses of these data have identified a number of provocative findings. There are strong indications those certain subgroups 
  #  of participants (albeit with small sample sizes) with urinary and non-urinary symptoms tend to improve over time; whereas other subgroups 
  #  tend to worsen over time. These patterns of improving or worsening are deferentially expressed according to sex, subtype of bladder pain syndrome (BPS), 
  #  and pain location (e.g., localized to the pelvic region vs pain reported in the pelvic region as well as other body sites)."),
  
  # h2("Let's Get Started!"),
  
  #  tags$ul(
  #    tags$li("Explore MAPP data by clicking the", strong("DATA VISUALIZATION"), "tab. For questions while exploring the MAPP data, look for 
  #           the", strong("INSTRUCTIONS"), "button and click it for more information."),
  #    tags$li("Not sure what to look for? Or, not sure what components are included in MAPP DataView? Use a data dictionary to search for MAPP data 
  #          components by clicking the", strong("DATA DICTIONARY"),  "tab."),
  #    tags$li("For other information or to send us a message, visit the", strong("ABOUT"), "tab."),
  #  ),
  
)

second_panel_1 <- tabPanel(
  "1-Variable Baseline Visualization",
  fluidPage(
    sidebarPanel(width = 3,
                 tags$head(
                   tags$style(
                     HTML(
                       "#analysis_type {
       font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
       font-size: 14px;
       background-color: white;
       white-space: normal;
        }",
                       "#variable_name {
       font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
       font-size: 14px;
       background-color: white;
       white-space: normal;
        }",
                       "#analysis_type_sub {
       font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
       font-size: 14px;
       background-color: white;
       white-space: normal;
        }",
                       "#variable_subgroup_name {
       font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
       font-size: 14px;
       background-color: white;
       white-space: normal;
        }"
                     )
                   )
                 ),
                 h3("Dataset Selection"),
                 radioButtons(
                   inputId = "dataset",
                   label = "Datasets",
                   choices = dataset_choice,
                 ),
                 h3("Selection Menu"),
                 checkboxGroupInput(
                   inputId = "cohort_type",
                   label = "Cohort",
                   choices = NULL
                 ),
                 conditionalPanel(
                   condition = "input.dataset == 'MAPP I Longitudinal' | input.dataset == 'MAPP II Longitudinal'",
                   selectInput(
                     inputId = "visit_select",
                     label = "Visit Number",
                     choices = NULL,
                     width = '25%'
                   )),
                 radioButtons(
                   inputId = "captions",
                   label = "Add Subsample Caption?",
                   choices = caption,
                 ),
                 checkboxInput(
                   inputId = "checking_filter", 
                   label = "Subsample Filtering?", 
                   value = FALSE, 
                 ),
                 conditionalPanel(
                   condition = "input.checking_filter == 1",
                   selectInput(
                     inputId = "variable_filter_group",
                     label = "Choose Variable Group to Filter",
                     choices = NULL
                   ),
                   selectInput(
                     inputId = "variable_filter_choice",
                     label = "Choose Variable to Filter",
                     choices = NULL
                   ),
                   checkboxGroupInput(
                     inputId = "checkbox_filter",
                     label = NULL,
                     choices = NULL
                   )),
                 selectInput(
                   inputId = "variable_group",
                   label = "Variable Group",
                   choices = NULL
                 ),
                 selectInput(
                   inputId = "variable_choice",
                   label = "Variable",
                   choices = NULL
                 ),
                 p(strong("Variable Description")),
                 verbatimTextOutput(
                   outputId = "variable_name"
                 ),
                 p(strong("Analysis Type")),
                 verbatimTextOutput(
                   outputId = "analysis_type"
                 ),
                 conditionalPanel(
                   condition = "output.analysis_type != '1 - Continuous'",
                   sliderInput(
                     inputId = "axis_angle",
                     label = "Angle of Axis Label",
                     min = 0,
                     max = 30,
                     value = 0,
                     round = TRUE,
                     step = 5,
                     ticks = TRUE,
                     width = "100%"
                   )),
                 conditionalPanel(
                   condition = "output.analysis_type != '1 - Continuous'",
                   sliderInput(
                     inputId = "text_size",
                     label = "Axis Label Text Size",
                     min = 10,
                     max = 25,
                     value = 16,
                     round = TRUE,
                     step = 1,
                     ticks = TRUE,
                     width = "100%"
                   )),
                 conditionalPanel(
                   condition = "output.analysis_type == '1 - Continuous'",
                   uiOutput(
                     outputId = "numeric_interval"
                   )),
                 conditionalPanel(
                   condition = "output.analysis_type == '1 - Continuous'",
                   uiOutput(
                     "x_slider"
                   )),
                 conditionalPanel(
                   condition = "output.analysis_type == '1 - Continuous' & input.tab1 == 'Plot'",
                   colourInput(
                     inputId = "fill_color",
                     label = "Fill Color?",
                     value = NULL,
                     returnName = TRUE, 
                     palette = "limited",
                     allowedCols = safe_colorblind_palette,
                     closeOnClick = TRUE,
                     showColour = "background"
                   )),
                 conditionalPanel(
                   condition = "output.analysis_type != '1 - Continuous'",
                   radioButtons(
                     inputId = "display_type",
                     label = "Display Type",
                     choices = display_type
                   )),
                 conditionalPanel(
                   condition = "output.analysis_type != '1 - Continuous'",
                   radioButtons(
                     inputId = "missing_data",
                     label = "Show Missing Data?",
                     choices = missing_data
                   )),
                 conditionalPanel(
                   condition = "input.tab1 == 'Strata' | input.tab2 == 'Strata' | input.tab3 == 'Strata'| input.tab4 == 'Strata' | input.tab5 == 'Strata' 
        | input.tab6 == 'Strata' | input.tab7 == 'Strata' | input.tab1 == 'Subgroup Boxplot' & input.tab1 != 'Plot' | input.tab2 != 'Plot' | input.tab3 != 'Plot'| 
        input.tab4 != 'Plot' | input.tab5 != 'Plot' | input.tab6 != 'Plot' | input.tab7 != 'Plot'",
                   
                   h3("Strata Selection"),
                   
                   selectInput(
                     inputId = "variable_group_2",
                     label = "Stratifying Variable Group",
                     choices = NULL
                   )
                 ),
                 conditionalPanel(
                   condition = "input.tab1 == 'Strata' | input.tab2 == 'Strata' | input.tab3 == 'Strata'| input.tab4 == 'Strata' | input.tab5 == 'Strata' 
        | input.tab6 == 'Strata' | input.tab7 == 'Strata' | input.tab1 == 'Subgroup Boxplot' & input.tab1 != 'Plot' | input.tab2 != 'Plot' | input.tab3 != 'Plot'| 
        input.tab4 != 'Plot' | input.tab5 != 'Plot' | input.tab6 != 'Plot' | input.tab7 != 'Plot'",
                   selectInput(
                     inputId = "variable_subgroup",
                     label = "Stratifying Variable",
                     choices = NULL
                   )),
                 conditionalPanel(
                   condition = "input.tab1 == 'Strata' | input.tab2 == 'Strata' | input.tab3 == 'Strata'| input.tab4 == 'Strata' | input.tab5 == 'Strata' 
        | input.tab6 == 'Strata' | input.tab7 == 'Strata' | input.tab1 == 'Subgroup Boxplot' & input.tab1 != 'Plot' | input.tab2 != 'Plot' | input.tab3 != 'Plot'| 
        input.tab4 != 'Plot' | input.tab5 != 'Plot' | input.tab6 != 'Plot' | input.tab7 != 'Plot'",
                   p(strong("Variable Description")),
                   verbatimTextOutput(
                     outputId = "variable_subgroup_name"
                   )),
                 conditionalPanel(
                   condition = "input.tab1 == 'Strata' | input.tab2 == 'Strata' | input.tab3 == 'Strata'| input.tab4 == 'Strata' | input.tab5 == 'Strata' 
        | input.tab6 == 'Strata' | input.tab7 == 'Strata' | input.tab1 == 'Strata Boxplot' & input.tab1 != 'Plot' | input.tab2 != 'Plot' | input.tab3 != 'Plot'| 
        input.tab4 != 'Plot' | input.tab5 != 'Plot' | input.tab6 != 'Plot' | input.tab7 != 'Plot'",
                   p(strong("Analysis Type")),
                   verbatimTextOutput(
                     outputId = "analysis_type_sub"
                   ),
                   checkboxGroupInput(
                     inputId = "subgroup_options",
                     label = NULL,
                     choices = NULL
                   )),
                 conditionalPanel(
                   condition = "output.analysis_type == '1 - Continuous' & input.tab1 == 'Strata'",
                   radioButtons(
                     inputId = "missing_data_sub",
                     label = "Show Missing Data?",
                     choices = missing_data
                   )),
                 conditionalPanel(
                   condition = "input.tab_1 != 'Summary' & input.tab_2 != 'Summary' & input.tab_3 != 'Summary' & input.tab_4 != 'Summary' & input.tab_5 != 'Summary' & 
                   input.tab_6 != 'Summary' & input.tab_7 != 'Summary' & input.sub_tab1 != 'Summary' & input.sub_tab2 != 'Summary' & input.sub_tab3 != 'Summary' & 
                   input.sub_tab4 != 'Summary' & input.sub_tab5 != 'Summary' & input.sub_tab6 != 'Summary' & input.sub_tab7 != 'Summary'", 
                   radioButtons(
                     inputId = "download_type",
                     label = "Type of Download?",
                     choices = pdf_png
                   )),
                 conditionalPanel(
                   condition = "input.tab_1 != 'Summary' & input.tab_2 != 'Summary' & input.tab_3 != 'Summary' & input.tab_4 != 'Summary' & input.tab_5 != 'Summary' & 
                   input.tab_6 != 'Summary' & input.tab_7 != 'Summary' & input.sub_tab1 != 'Summary' & input.sub_tab2 != 'Summary' & input.sub_tab3 != 'Summary' & 
                   input.sub_tab4 != 'Summary' & input.sub_tab5 != 'Summary' & input.sub_tab6 != 'Summary' & input.sub_tab7 != 'Summary'",
                   downloadButton('download_plot_1', 'Download Plot')
                 ),
                 conditionalPanel(
                   condition = "input.tab_1 == 'Summary' & input.tab1 == 'Plot'",
                   downloadButton('download_t1', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_2 == 'Summary' & input.tab2 == 'Plot'",
                   downloadButton('download_t2', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_3 == 'Summary' & input.tab3 == 'Plot'",
                   downloadButton('download_t3', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_4 == 'Summary' & input.tab4 == 'Plot'",
                   downloadButton('download_t4', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_5 == 'Summary' & input.tab5 == 'Plot'",
                   downloadButton('download_t5', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_6 == 'Summary' & input.tab6 == 'Plot'",
                   downloadButton('download_t6', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_7 == 'Summary' & input.tab7 == 'Plot'",
                   downloadButton('download_t7', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.sub_tab1 == 'Summary' & input.tab1 == 'Strata'",
                   downloadButton('download_t8', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.sub_tab2 == 'Summary' & input.tab2 == 'Strata'",
                   downloadButton('download_t9', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.sub_tab3 == 'Summary' & input.tab3 == 'Strata'",
                   downloadButton('download_t10', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.sub_tab4 == 'Summary' & input.tab4 == 'Strata'",
                   downloadButton('download_t11', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.sub_tab5 == 'Summary' & input.tab5 == 'Strata'",
                   downloadButton('download_t12', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.sub_tab6 == 'Summary' & input.tab6 == 'Strata'",
                   downloadButton('download_t13', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.sub_tab7 == 'Summary' & input.tab7 == 'Strata'",
                   downloadButton('download_t14', 'Download Table')
                 ),
    ),
    
    mainPanel(width = 8,
              conditionalPanel(
                condition = "output.analysis_type == '1 - Continuous'",
                tabsetPanel(id = "tab1", type = "tabs",
                            tabPanel("Plot", 
                                     tabsetPanel(id = "tab_1", type = "tabs",
                                                 tabPanel("Histogram", br(), plotOutput("plot", height = "600px")),
                                                 tabPanel("Density Plot", plotOutput("density_plot2", height = "600px")),
                                                 tabPanel("Summary", gt_output("summary_1")))),
                            tabPanel("Strata",
                                     tabsetPanel(id = "sub_tab1", type = "tabs",
                                                 tabPanel("Histogram", br(), plotOutput("sub_plot1", height = "600px")),
                                                 tabPanel("Density Plot", br(), plotOutput("density_plot1", height = "600px")),
                                                 tabPanel("Frequency Plot", br(), plotOutput("freq_plot1", height = "600px")),
                                                 tabPanel("Boxplot", br(), plotOutput("boxplot_sub1", height = "600px")),
                                                 tabPanel("Summary", br(), gt_output("summary_sub1"))))
                )
              ),
              conditionalPanel(
                condition = "output.analysis_type == '2 - Ordinal'",
                conditionalPanel(
                  condition = "input.display_type == 'Count'",
                  tabsetPanel(id = "tab2", type = "tabs",
                              tabPanel("Plot",
                                       tabsetPanel(id = "tab_2", type = "tabs",
                                                   tabPanel("Bar Graph", br(), plotOutput("plot_2", height = "600px")),
                                                   tabPanel("Summary", br(), gt_output("summary_2")))),
                              tabPanel("Strata",
                                       tabsetPanel(id = "sub_tab2", type = "tabs",
                                                   tabPanel("Grouped", br(), plotOutput("sub_plot2", height = "600px")),
                                                   tabPanel("Stacked", br(), plotOutput("sub_plot8", height = "600px")),
                                                   tabPanel("Summary", br(), gt_output("summary_sub2"))))
                  )),
                conditionalPanel(
                  condition = "input.display_type == 'Percent'",
                  tabsetPanel(id = "tab3", type = "tabs",
                              tabPanel("Plot",
                                       tabsetPanel(id = "tab_3", type = "tabs",
                                                   tabPanel("Bar Graph", br(), plotOutput("plot_3", height = "600px")),
                                                   tabPanel("Summary", br(), gt_output("summary_3")))),
                              tabPanel("Strata", 
                                       tabsetPanel(id = "sub_tab3", type = "tabs",
                                                   tabPanel("Grouped", br(), plotOutput("sub_plot3", height = "600px")),
                                                   tabPanel("Stacked", br(), plotOutput("sub_plot9", height = "600px")),
                                                   tabPanel("Summary", br(), gt_output("summary_sub3"))))
                  ))),
              conditionalPanel(
                condition = "output.analysis_type == '3 - Nominal'",
                conditionalPanel(
                  condition = "input.display_type == 'Count'",
                  tabsetPanel(id = "tab4", type = "tabs",
                              tabPanel("Plot",
                                       tabsetPanel(id = "tab_4", type = "tabs",
                                                   tabPanel("Bar Graph", br(), plotOutput("plot_4", height = "600px")),
                                                   tabPanel("Summary", br(), gt_output("summary_4")))),
                              tabPanel("Strata",
                                       tabsetPanel(id = "sub_tab4", type = "tabs",
                                                   tabPanel("Grouped", br(), plotOutput("sub_plot4", height = "600px")),
                                                   tabPanel("Stacked", br(), plotOutput("sub_plot10", height = "600px")),
                                                   tabPanel("Summary", br(), gt_output("summary_sub4"))))
                  )),
                conditionalPanel(
                  condition = "input.display_type == 'Percent'",
                  tabsetPanel(id = "tab5", type = "tabs",
                              tabPanel("Plot", 
                                       tabsetPanel(id = "tab_5", type = "tabs",
                                                   tabPanel("Bar Graph", br(), plotOutput("plot_5", height = "600px")),
                                                   tabPanel("Summary", br(), gt_output("summary_5")))),
                              tabPanel("Strata",
                                       tabsetPanel(id = "sub_tab5", type = "tabs",
                                                   tabPanel("Grouped", br(), plotOutput("sub_plot5", height = "600px")),
                                                   tabPanel("Stacked", br(), plotOutput("sub_plot11", height = "600px")),
                                                   tabPanel("Summary", br(), plotOutput("summary_sub5"))))
                              
                  ))),
              conditionalPanel(
                condition = "output.analysis_type == '4 - Binary'",
                conditionalPanel(
                  condition = "input.display_type == 'Count'",
                  tabsetPanel(id = "tab6", type = "tabs",
                              tabPanel("Plot",
                                       tabsetPanel(id = "tab_6", type = "tabs",
                                                   tabPanel("Bar Graph", br(), plotOutput("plot_6", height = "600px")),
                                                   tabPanel("Summary", br(), gt_output("summary_6")))),
                              tabPanel("Strata", 
                                       tabsetPanel(id = "sub_tab6", type = "tabs",
                                                   tabPanel("Grouped", br(), plotOutput("sub_plot6", height = "600px")),
                                                   tabPanel("Stacked", br(), plotOutput("sub_plot12", height = "600px")),
                                                   tabPanel("Summary", br(), gt_output("summary_sub6"))))
                  )),
                conditionalPanel(
                  condition = "input.display_type == 'Percent'",
                  tabsetPanel(id = "tab7", type = "tabs",
                              tabPanel("Plot",
                                       tabsetPanel(id = "tab_7", type = "tabs",
                                                   tabPanel("Bar Graph", br(), plotOutput("plot_7", height = "600px")),
                                                   tabPanel("Summary", br(), gt_output("summary_7")))),
                              tabPanel("Strata",
                                       tabsetPanel(id = "sub_tab7", type = "tabs",
                                                   tabPanel("Grouped", br(), plotOutput("sub_plot7", height = "600px")),
                                                   tabPanel("Stacked", br(), plotOutput("sub_plot13", height = "600px")),
                                                   tabPanel("Summary", br(), gt_output("summary_sub7"))))
                  ))),
    ),
  ))

second_panel_2 <- tabPanel(
  "2-Variable Baseline Visualization",
  sidebarLayout(
    sidebarPanel(width = 3,
                 tags$head(
                   tags$style(
                     HTML(
                       "#analysis_type_2x {
       font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
       font-size: 14px;
       background-color: white;
       white-space: normal;
        }",
                       "#analysis_type_2y {
       font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
       font-size: 14px;
       background-color: white;
       white-space: normal;
        }",
                       "#variable_name_2x {
       font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
       font-size: 14px;
       background-color: white;
       white-space: normal;
        }",
                       "#variable_name_2y {
       font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
       font-size: 14px;
       background-color: white;
       white-space: normal;
        }",
                       
        "input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }"
                     )
                   )
                 ),
                 h3("Dataset Selection"),
                 radioButtons(
                   inputId = "dataset_2",
                   label = "Datasets",
                   choices = dataset_choice,
                 ),
                 h3("Selection Menu"),
                 checkboxGroupInput(
                   inputId = "cohort_type_2",
                   label = "Cohort",
                   choices = NULL
                 ),
                 conditionalPanel(
                   condition = "input.dataset_2 == 'MAPP I Longitudinal' | input.dataset_2 == 'MAPP II Longitudinal'",
                   selectInput(
                     inputId = "visit_select_2",
                     label = "Visit Number",
                     choices = NULL,
                     width = '25%'
                   )),
                 radioButtons(
                   inputId = "captions_2",
                   label = "Add Subsample Caption?",
                   choices = caption,
                 ),
                 checkboxInput(
                   inputId = "checking_filter_2", 
                   label = "Subsample Filtering?",
                   value = FALSE, 
                 ),
                 conditionalPanel(
                   condition = "input.checking_filter_2 == 1",
                   selectInput(
                     inputId = "variable_filter_group_2",
                     label = "Choose Variable Group to Filter",
                     choices = NULL
                   ),
                   selectInput(
                     inputId = "variable_filter_choice_2",
                     label = "Choose Variable Group to Filter",
                     choices = NULL
                   ),
                   checkboxGroupInput(
                     inputId = "checkbox_filter_2",
                     label = NULL,
                     choices = NULL
                   )),
                 conditionalPanel(
                   condition = "output.analysis_type_2x == '1 - Continuous' & output.analysis_type_2y == '1 - Continuous' & input.tab_a == 'Graph'",
                   h5("Graphical Options"),
                   checkboxInput(
                     inputId = "intercept_button",
                     label = "Add X & Y Reference Lines?", 
                     value = FALSE
                   ),
                   conditionalPanel(
                     condition = "input.intercept_button == 1",
                     checkboxInput(
                       inputId = "color_button",
                       label = "Add Color Ranges?", 
                       value = FALSE
                     )),
                   checkboxInput(
                     inputId = "lowess_line",
                     label = "Add Lowess Line?", 
                     value = FALSE
                   ),
                   checkboxInput(
                     inputId = "regression_line",
                     label = "Add Regression Line?", 
                     value = FALSE
                   ),
                   checkboxInput(
                     inputId = "continuous_fill",
                     label = "Add Fill Variable?", 
                     value = FALSE
                   )),
                 conditionalPanel(
                   condition = "input.continuous_fill == 1 & input.tab_a == 'Graph'",
                   h5("Fill Variable Selection"),
                   selectInput(
                     inputId = "variable_group_fill",
                     label = "Variable Group Fill",
                     choices = NULL
                   ),
                   selectInput(
                     inputId = "variable_choice_fill",
                     label = "Fill Variable",
                     choices = NULL
                   ),
                   checkboxGroupInput(
                     inputId = "fill_filter_2",
                     label = NULL,
                     choices = NULL
                   )),
                 h4("X-Axis Selection"),
                 selectInput(
                   inputId = "variable_group_2x",
                   label = "Variable Group X-Axis",
                   choices = NULL
                 ),
                 selectInput(
                   inputId = "variable_choice_2x",
                   label = "X-Axis Variable",
                   choices = NULL
                 ),
                 p(strong("Variable Description")),
                 verbatimTextOutput(
                   outputId = "variable_name_2x"
                 ),
                 p(strong("Analysis Type")),
                 verbatimTextOutput(
                   outputId = "analysis_type_2x"
                 ),
                 conditionalPanel(
                   condition = "output.analysis_type_2x == '1 - Continuous' & output.analysis_type_2y == '1 - Continuous' 
                   | output.analysis_type_2x == '1 - Continuous' & output.analysis_type_2y != '1 - Continuous'",
                   conditionalPanel(
                     condition = "output.analysis_type_2x == '1 - Continuous' & input.tab_a == 'Graph'",
                     uiOutput(
                       outputId = "numeric_interval_x"
                     )),
                   conditionalPanel(
                     condition = "input.intercept_button == 1 | input.tab_a == 'Two Way Table' | input.tab_c == 'Two Way Table'",
                     uiOutput(
                       outputId = "numeric_x"
                     )),
                   conditionalPanel(
                     condition = "input.tab_a == 'Graph' | input.tab_a == 'Summary'",
                     uiOutput(
                       outputId = "x_slider_2"
                   ))),
                 conditionalPanel(
                   condition = "output.analysis_type_2x != '1 - Continuous' & output.analysis_type_2y != '1 - Continuous'",
                   sliderInput(
                     inputId = "text_size_2",
                     label = "Axis Label Text Size",
                     min = 8,
                     max = 25,
                     value = 16,
                     round = TRUE,
                     step = 1,
                     ticks = TRUE,
                     width = "100%"
                   )),
                 conditionalPanel(
                   condition = "output.analysis_type_2x != '1 - Continuous' & output.analysis_type_2y != '1 - Continuous' & tab_b != 'Mosaic Plot'",
                   sliderInput(
                     inputId = "axis_angle_2",
                     label = "Angle of Axis Label",
                     min = 0,
                     max = 30,
                     value = 0,
                     round = TRUE,
                     step = 5,
                     ticks = TRUE,
                     width = "100%"
                   )),
                 h4("Y-Axis Selection"),
                 selectInput(
                   inputId = "variable_group_2y",
                   label = "Variable Group Y-Axis",
                   choices = NULL
                 ),
                 selectInput(
                   inputId = "variable_choice_2y",
                   label = "Y-Axis Variable",
                   choices = NULL
                 ),
                 p(strong("Variable Description")),
                 verbatimTextOutput(
                   outputId = "variable_name_2y"
                 ),
                 p(strong("Analysis Type")),
                 verbatimTextOutput(
                   outputId = "analysis_type_2y"
                 ),
                 conditionalPanel(
                   condition = "output.analysis_type_2x == '1 - Continuous' & output.analysis_type_2y == '1 - Continuous'
                   | output.analysis_type_2x != '1 - Continuous' & output.analysis_type_2y == '1 - Continuous'",
                   conditionalPanel(
                     condition = "output.analysis_type_2y == '1 - Continuous' & input.tab_a == 'Graph'",
                     uiOutput(
                       outputId = "numeric_interval_y"
                     )),
                   conditionalPanel(
                     condition = "input.intercept_button == 1 | input.tab_a == 'Two Way Table' | input.tab_c == 'Two Way Table'",
                     uiOutput(
                       outputId = "numeric_y"
                     )),
                   conditionalPanel(
                     condition = "input.tab_a == 'Graph' | input.tab_a == 'Summary'",
                     uiOutput(
                       outputId = "y_slider_2"
                   ))),
                 conditionalPanel(
                   condition = "output.analysis_type_2x != '1 - Continuous' & output.analysis_type_2y != '1 - Continuous'",
                   radioButtons(
                     inputId = "display_type_2",
                     label = "Display Type",
                     choices = display_type
                   )),
                 conditionalPanel(
                   condition = "(output.analysis_type_2x != '1 - Continuous' & output.analysis_type_2y != '1 - Continuous') | (input.continuous_fill == 1 & input.tab_a == 'Graph')",
                   radioButtons(
                     inputId = "missing_data_2",
                     label = "Show Missing Data?",
                     choices = missing_data
                   )),
                 conditionalPanel(
                   condition = "output.analysis_type_2x == '1 - Continuous' & output.analysis_type_2y == '1 - Continuous' & input.tab_a == 'Two Way Table' | 
                                input.tab_c == 'Two Way Table' & output.analysis_type_2y == '1 - Continuous' & output.analysis_type_2x != '1 - Continuous' | 
                                input.tab_c == 'Two Way Table' & output.analysis_type_2y != '1 - Continuous' & output.analysis_type_2x == '1 - Continuous'",
                   radioButtons(
                     inputId = "missing_data_3",
                     label = "Show Missing Data?",
                     choices = missing_data
                   )),
                 conditionalPanel(
                   condition = "input.tab_a != 'Summary' & input.tab_a != 'Two Way Table' & input.tab_b != 'Two Way Table' & input.tab_c != 'Summary'",
                   radioButtons(
                     inputId = "download_type_2",
                     label = "Type of Download?",
                     choices = pdf_png
                   )),
                 conditionalPanel(
                   condition = "input.tab_a != 'Summary' & input.tab_a != 'Two Way Table' & input.tab_b != 'Two Way Table' & input.tab_c != 'Summary' & 
                                input.tab_a != 'Regression Statistics' & input.tab_b != 'Regression Statistics' & input.tab_c != 'Regression Statistics' & 
                                input.tab_c != 'Two Way Table'",
                   downloadButton('download_2', 'Download Plot')
                 ),
                 conditionalPanel(
                   condition = "input.tab_a == 'Two Way Table' & input.variable_choice_2x != input.variable_choice_2y",
                   downloadButton('download_t15', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_a == 'Two Way Table' & input.variable_choice_2x == input.variable_choice_2y",
                   downloadButton('download_t16', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_b == 'Two Way Table' & input.variable_choice_2x != input.variable_choice_2y",
                   downloadButton('download_t17', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_b == 'Two Way Table' & input.variable_choice_2x == input.variable_choice_2y",
                   downloadButton('download_t18', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_a == 'Summary' & input.variable_choice_2x != input.variable_choice_2y",
                   downloadButton('download_t19', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_a == 'Summary' & input.variable_choice_2x == input.variable_choice_2y",
                   downloadButton('download_t20', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_c == 'Summary' & output.analysis_type_2x == '1 - Continuous' & output.analysis_type_2y != '1 - Continuous'",
                   downloadButton('download_t21', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_c == 'Summary' & output.analysis_type_2y == '1 - Continuous' & output.analysis_type_2x != '1 - Continuous'",
                   downloadButton('download_t22', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_a == 'Regression Statistics' & input.variable_choice_2x != input.variable_choice_2y",
                   downloadButton('download_t23', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_b == 'Regression Statistics' & input.variable_choice_2x != input.variable_choice_2y",
                   downloadButton('download_t24', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_c == 'Regression Statistics' & output.analysis_type_2x == '1 - Continuous' & output.analysis_type_2y != '1 - Continuous'",
                   downloadButton('download_t25', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_c == 'Regression Statistics' & output.analysis_type_2y == '1 - Continuous' & output.analysis_type_2x != '1 - Continuous'",
                   downloadButton('download_t26', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_c == 'Two Way Table' & output.analysis_type_2y == '1 - Continuous' & output.analysis_type_2x != '1 - Continuous' | 
                                input.tab_c == 'Two Way Table' & output.analysis_type_2y != '1 - Continuous' & output.analysis_type_2x == '1 - Continuous'",
                   downloadButton('download_tc', 'Download Table')
                ),
                 
    ),
    
    mainPanel(width = 8,
              conditionalPanel(
                condition = "output.analysis_type_2x == '1 - Continuous' & output.analysis_type_2y == '1 - Continuous'",
                tabsetPanel(id = "tab_a", type = "tabs",
                            tabPanel("Graph", br(), plotOutput("graph_1", height = "600px")),
                            tabPanel("Summary", br(), gt_output("summary_d")),
                            tabPanel("Two Way Table", br(), gt_output("summary_a")),
                            tabPanel("Regression Statistics", br(), gt_output("regression_a")))),
              conditionalPanel(
                condition = "output.analysis_type_2x == '2 - Ordinal' & output.analysis_type_2y == '2 - Ordinal' | 
                             output.analysis_type_2x == '2 - Ordinal' & output.analysis_type_2y == '3 - Nominal' |
                             output.analysis_type_2x == '2 - Ordinal' & output.analysis_type_2y == '4 - Binary' |
                             output.analysis_type_2x == '3 - Nominal' & output.analysis_type_2y == '2 - Ordinal' |
                             output.analysis_type_2x == '3 - Nominal' & output.analysis_type_2y == '3 - Nominal' |
                             output.analysis_type_2x == '3 - Nominal' & output.analysis_type_2y == '4 - Binary' |
                             output.analysis_type_2x == '4 - Binary' & output.analysis_type_2y == '2 - Ordinal' |
                             output.analysis_type_2x == '4 - Binary' & output.analysis_type_2y == '3 - Nominal' |
                             output.analysis_type_2x == '4 - Binary' & output.analysis_type_2y == '4 - Binary'",
                tabsetPanel(id = "tab_b", type = "tabs",
                            tabPanel("Bar", br(), plotOutput("graph_4", height = "600px")),
                            tabPanel("Stacked", br(), plotOutput("graph_3", height = "600px")),
                            tabPanel("Two Way Table", br(), gt_output("summary_b")),
                            tabPanel("Mosaic Plot", br(), plotOutput("mosaic_1", height = "600px")),
                            tabPanel("Regression Statistics", br(), gt_output("regression_b")))),
              conditionalPanel(
                condition = "output.analysis_type_2x == '1 - Continuous' & output.analysis_type_2y == '2 - Ordinal' |
                             output.analysis_type_2x == '1 - Continuous' & output.analysis_type_2y == '3 - Nominal' |
                             output.analysis_type_2x == '1 - Continuous' & output.analysis_type_2y == '4 - Binary' |
                             output.analysis_type_2x == '2 - Ordinal' & output.analysis_type_2y == '1 - Continuous' |
                             output.analysis_type_2x == '3 - Nominal' & output.analysis_type_2y == '1 - Continuous' |
                             output.analysis_type_2x == '4 - Binary' & output.analysis_type_2y == '1 - Continuous'",
                tabsetPanel(id = "tab_c", type = "tabs",
                            tabPanel("Graph", br(), plotOutput("graph_2", height = "600px")),
                            tabPanel("Summary", br(), gt_output("summary_e")),
                            tabPanel("Two Way Table", br(), gt_output("summary_c")),
                            tabPanel("Regression Statistics", br(), gt_output("regression_c"))))
    ),
    
  ))

long_panel <- tabPanel(
  "2-Variable Longitudinal Visualization",
  sidebarLayout(
    sidebarPanel(width = 3,
                 tags$head(
                   tags$style(
                     HTML(
                       "#variable_group_long_1 {
       font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
       font-size: 14px;
       background-color: white;
       white-space: normal;
        }",
                       "#variable_choice_long_1 {
       font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
       font-size: 14px;
       background-color: white;
       white-space: normal;
        }",
                       "#variable_name_long_1 {
       font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
       font-size: 14px;
       background-color: white;
       white-space: normal;
        }",
                       "#analysis_type_long_1 {
       font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
       font-size: 14px;
       background-color: white;
       white-space: normal;
        }",
                       "#variable_name_long_2 {
       font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
       font-size: 14px;
       background-color: white;
       white-space: normal;
        }",
                       "#long_analysis_type_2 {
       font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
       font-size: 14px;
       background-color: white;
       white-space: normal;
        }",
                       "#long_cohort {
       font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
       font-size: 14px;
       background-color: white;
       white-space: normal;
        }",
                     )
                   )
                 ),
                 h3("Selection Menu"),
                 radioButtons(
                   inputId = "dataset_long",
                   label = "Datasets",
                   choices = dataset_choice_long
                 ),
                 p(strong("Cohort Type")),
                 verbatimTextOutput(
                   outputId = "long_cohort"
                 ),
                 radioButtons(
                   inputId = "captions_long",
                   label = "Add Subsample Caption?",
                   choices = caption,
                 ),
                 checkboxInput(
                   inputId = "checking_filter_long",
                   label = "Subsample Filtering?",
                   value = FALSE,
                 ),
                 conditionalPanel(
                   condition = "input.checking_filter_long == 1",
                   selectInput(
                     inputId = "variable_filter_group_long",
                     label = "Choose Variable Group to Filter",
                     choices = NULL
                   ),
                   selectInput(
                     inputId = "variable_filter_choice_long",
                     label = "Choose Variable Group to Filter",
                     choices = NULL
                   ),
                   checkboxGroupInput(
                     inputId = "checkbox_filter_long",
                     label = NULL,
                     choices = NULL
                   )),
                 conditionalPanel(
                   condition = "input.tab_long == 'Plot' | input.tab_long == 'Violin Plot'",
                   h5("Graphical Options"),
                   checkboxInput(
                     inputId = "facet_wrap",
                     label = "Stratify Data?",
                     value = FALSE
                   ),
                   conditionalPanel(
                     condition = "input.facet_wrap == 1 & input.tab_long == 'Plot' | input.tab_long == 'Violin Plot'",
                     h5("Stratifying Variable Selection"),
                     selectInput(
                       inputId = "facet_wrap_group",
                       label = "Stratifying Group",
                       choices = NULL
                     ),
                     selectInput(
                       inputId = "facet_wrap_variable",
                       label = "Stratifying Variable",
                       choices = NULL
                     ),
                     checkboxGroupInput(
                       inputId = "facet_filter",
                       label = NULL,
                       choices = NULL
                     )),
                   checkboxInput(
                     inputId = "long_lowess",
                     label = "Add Lowess Line",
                     value = FALSE
                   ),
                   checkboxInput(
                     inputId = "long_mean",
                     label = "Add Mean Values",
                     value = FALSE
                   ),
                   checkboxInput(
                     inputId = "long_median",
                     label = "Add Median Values",
                     value = FALSE
                   )),
                 h4("X-Axis"),
                 p(strong("Variable Group")),
                 verbatimTextOutput(
                   outputId = "variable_group_long_1"
                 ),
                 p(strong("Variable")),
                 verbatimTextOutput(
                   outputId = "variable_choice_long_1"
                 ),
                 p(strong("Variable Description")),
                 verbatimTextOutput(
                   outputId = "variable_name_long_1"
                 ),
                 p(strong("Analysis Type")),
                 verbatimTextOutput(
                   outputId = "analysis_type_long_1"
                 ),
                 conditionalPanel(
                   condition = "input.tab_long != 'Summary'",
                   uiOutput(
                     outputId = "numeric_interval_long_1"
                   )),
                 uiOutput(
                   outputId = "long_slider_1"
                 ),
                 h4("Y-Axis Selection"),
                 selectInput(
                   inputId = "variable_group_long_2",
                   label = "Variable Group Y-Axis",
                   choices = NULL
                 ),
                 selectInput(
                   inputId = "variable_choice_long_2",
                   label = "Y-Axis",
                   choices = NULL
                 ),
                 p(strong("Variable Description")),
                 verbatimTextOutput(
                   outputId = "variable_name_long_2"
                 ),
                 p(strong("Analysis Type")),
                 verbatimTextOutput(
                   outputId = "long_analysis_type_2"
                 ),
                 conditionalPanel(
                   condition = "input.tab_long != 'Summary'",
                   uiOutput(
                     outputId = "numeric_interval_long_2"
                   )),
                 uiOutput(
                   outputId = "long_slider_2"
                 ),
                 conditionalPanel(
                   condition = "output.analysis_type != '1 - Continuous'",
                   radioButtons(
                     inputId = "missing_data_long",
                     label = "Show Missing Data?",
                     choices = missing_data
                   )),
                 conditionalPanel(
                   condition = "input.tab_long != 'Summary'",
                   radioButtons(
                     inputId = "download_long_plot",
                     label = "Type of Download?",
                     choices = pdf_png
                   )),
                 conditionalPanel(
                   condition = "input.tab_long == 'Summary'",
                   downloadButton('download_t_long', 'Download Table')
                 ),
                 conditionalPanel(
                   condition = "input.tab_long == 'Plot' | input.tab_long == 'Violin Plot'",
                   downloadButton('download_plot_long', 'Download Plot')
                 ),
    ),
    
    mainPanel(width = 8,
              tabsetPanel(id = "tab_long", type = "tabs",
                          tabPanel("Plot", br(), plotOutput("long_plot_1", height = 1000)),
                          #tabPanel("Violin Plot", br(), plotOutput("long_violin_1", height = "600px")),
                          tabPanel("Summary", br(), gt_output("summary_long_1")))
    ),
    
  ))

third_panel <- tabPanel(
  "Data Codebook",
  
  titlePanel("MAPP Data Dictionary"),
  
  p("Data Dictionary displays all available data components in MAPP DataView. The 8 columns 
    (Name, Type, Label, Module, Primary Grouping, Primary Code, Secondary Grouping & Secondary Code) are explained as follows."),
  
  tags$ul(
    tags$li(strong("Variable:"), "Variable name used in MAPP analytical datasets. The search box below Name allows you to filter some keywords"),
    tags$li(strong("Type:"), "Categorical or continuous variable types. The search box below Type allows you to 
            select and filter specific types of variables of interest. Click inside the search box to see a drop-down list of all domains."),
    tags$li(strong("Label:"), "Information regarding each variable. The search box below Label allows you to filter some 
            keywords; e.g., type 'pain' in the box and all labels containing 'pain' will be filtered."), 
    tags$li(strong("Description:"), "Descriptive information regarding each variable"), 
    tags$li(strong("Module:"), "Each variable is assigned a module"),
    tags$li(strong("Primary Grouping:"), "Variables are grouped into one of 12 different categories based on use"),
    tags$li(strong("Secondary Grouping:"), "Variables are further broken down into 62 secondary groupings"),
  ),
  
  
  p("The search box search on the upper-right corner allows you to filter some keywords among all four columns. Try 'pain' or 'SYMQ'."),
  
  h3("Dataset Selection"),
  radioButtons(
    inputId = "dataset_choice_dict",
    label = "Datasets",
    choices = dataset_choice_dict
  ),
  
  hr(),
  h2("MAPP Variable Assignments"),
  fluidRow(column(DT::dataTableOutput("mapp_dict"), width = 12)),
)

about <- tabPanel(
  "About",
  tags$ul(
    tags$li(strong("MAPP DataView"), "is built using the shiny package in R."),
    tags$li("Data are updated as of XXX; the current study cutoff year for time-to-event outcomes is XXX"),
    tags$li("To learn more about the MAPP Study, please visit us at XXX."),
    tags$li("Contact XXX for any questions or comments."),
  )
)

ui <-
  navbarPage(
    #header,
    "MAPP Dataview",
    intro_panel,
    second_panel_1,
    second_panel_2,
    long_panel,
    third_panel,
    about
  )