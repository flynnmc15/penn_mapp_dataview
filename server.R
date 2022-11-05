server <- function(input, output, session) {
  
  #Data Table
  output$mapp_dict = DT::renderDataTable({
    if(input$dataset_choice_dict == "MAPP I Baseline") {
    mapp_dict %>% 
        mutate(TYPE = ifelse(ANALYSIS_TYPE == 1, "1 - Continuous", ifelse(ANALYSIS_TYPE == 2, "2 - Ordinal", ifelse(ANALYSIS_TYPE == 3, "3 - Nominal", "4 - Binary")))) %>%
        select(-Sequence_within_Group, -Primary_Code, -Secondary_Code, -ANALYSIS_TYPE) %>% 
        rename(Variable = NAME, Label = Label, Description = Detailed_Label, Module = MODULE)
      
    } else if(input$dataset_choice_dict == "MAPP II Baseline") {
      mapp_dict_II %>% 
        mutate(TYPE = ifelse(ANALYSIS_TYPE == 1, "1 - Continuous", ifelse(ANALYSIS_TYPE == 2, "2 - Ordinal", ifelse(ANALYSIS_TYPE == 3, "3 - Nominal", "4 - Binary")))) %>%
        select(-Sequence_within_Group, -Primary_Code, -Secondary_Code, -ANALYSIS_TYPE) %>% 
        rename(Variable = NAME, Type = TYPE, Label = LABEL, Description = Detailed_Label, Module = MODULE)
      
    } else if(input$dataset_choice_dict == "MAPP I Longitudinal") {
      mapp_long_dict %>% 
        mutate(TYPE = ifelse(analysis_type == 1, "1 - Continuous", ifelse(analysis_type == 2, "2 - Ordinal", ifelse(analysis_type == 3, "3 - Nominal", "4 - Binary")))) %>%
        select(-Sequence_within_Group, -Primary_Code, -Secondary_Code, -analysis_type) %>% 
        rename(Variable = variable, Type = TYPE, Label = label, Description = Detailed_Label, Module = MODULE,  Primary_Grouping = variable_selection)
      
    } else {
      mapp_long_dict_II %>% 
        mutate(TYPE = ifelse(analysis_type == 1, "1 - Continuous", ifelse(analysis_type == 2, "2 - Ordinal", ifelse(analysis_type == 3, "3 - Nominal", "4 - Binary")))) %>%
        select(-Sequence_within_Group, -Primary_Code, -Secondary_Code, -analysis_type) %>% 
        rename(Variable = variable, Type = TYPE, Label = label, Description = Detailed_Label, Module = MODULE,  Primary_Grouping = variable_selection)
    }
    
  })
  
  mapp_dataset <- reactive({
    if(input$dataset == "MAPP I Baseline") {
      mapp_bl_2
    }
    else if(input$dataset == "MAPP II Baseline") {
      mapp_bl_2_II
    }
    else if(input$dataset == "MAPP I Longitudinal") {
      mapp_long_raw
    } 
    else {
      mapp_long_raw_II
    }
  })
  
  dataset_subtitle <- reactive({
    if(input$dataset == "MAPP I Baseline") {
      dataset_choice[1]
    }
    else if(input$dataset == "MAPP II Baseline") {
      dataset_choice[2]
  }
    else if(input$dataset == "MAPP I Longitudinal") {
      paste0(dataset_choice[3], " Visit ", input$visit_select)
    }
    else {
      paste0(dataset_choice[4], " Visit ", input$visit_select)
    }
  })
  
  variable_assignments_3_react <- reactive({
    if(input$dataset == "MAPP I Baseline") {
      variable_assignments_3
    }
    else if(input$dataset == "MAPP II Baseline") {
      variable_assignments_3_II
    }
    else if(input$dataset == "MAPP I Longitudinal") {
      variable_assignments_long_3 %>% 
        filter(variable != "vnum")
    } 
    else {
      variable_assignments_long_3_II %>% 
        filter(variable != "vnum")
    }
  })
  
  variable_assignments_4_react <- reactive({
    if(input$dataset == "MAPP I Baseline") {
      variable_assignments_4
    }
    else if(input$dataset == "MAPP II Longitudinal") {
      variable_assignments_4_II
    }
    else if(input$dataset == "MAPP I Longitudinal") {
      variable_assignments_long_4
    }
    else {
      variable_assignments_long_4_II
    }
  })
  
  mapp_bl_combined_react <- reactive({
    if(input$dataset == "MAPP I Baseline") {
      mapp_bl_combined
    }
    else if(input$dataset == "MAPP II Baseline") {
      mapp_bl_combined_II
    }
    else if(input$dataset == "MAPP I Longitudinal") {
      mapp_long_combined
    }
    else {
      mapp_long_combined_II
    }
  })
  
  mapp_bl_labels_react <- reactive({
    if(input$dataset == "MAPP I Baseline") {
      mapp_bl_labels
    }
    else if(input$dataset == "MAPP II Baseline") {
      mapp_bl_labels_II
    }
    else if(input$dataset == "MAPP I Longitudinal") {
      mapp_long_labels
    }
    else {
      mapp_long_labels_II
    }
  })
  
  full_labels_react <- reactive({
    if(input$dataset == "MAPP I Baseline") {
      full_labels
    }
    else if(input$dataset == "MAPP II Baseline") {
      full_labels_II
    }
    else if(input$dataset == "MAPP I Longitudinal") {
      full_labels_long
    }
    else {
      full_labels_long_II
    }
  })
  
  new_data_cohort <- reactive({
    if(input$dataset == "MAPP I Baseline") {
      mapp_bl_cohort$cohorttype2
    }
    else if(input$dataset == "MAPP II Baseline") {
      mapp_bl_cohort_II$cohorttype2
    }
    else if(input$dataset == "MAPP I Longitudinal") {
      mapp_long_cohort$cohorttype2
    }
    else {
      mapp_long_cohort_II$cohorttype2
    }
  })
  
  new_data_primary_grouping <- reactive({
    if(input$dataset == "MAPP I Baseline") {
      mapp_bl_primary$variable_selection
    }
    else if(input$dataset == "MAPP II Baseline") {
      mapp_bl_primary_II$variable_selection
    }
    else if(input$dataset == "MAPP I Longitudinal") {
      mapp_long_primary$variable_selection
    }
    else {
      mapp_long_primary_II$variable_selection
    }
  })
  
  visit_select_long <- reactive({
    if(input$dataset == "MAPP I Longitudinal") {
      mapp_long_raw %>% 
        select(vnum) %>% 
        distinct() %>% 
        arrange(vnum)
    } 
    else if(input$dataset == "MAPP II Longitudinal") {
      mapp_long_raw_II %>% 
        select(vnum) %>% 
        distinct() %>% 
        arrange(vnum)
    }
    else {
      mapp_long_raw %>% 
        select(vnum) %>% 
        distinct() %>% 
        arrange(vnum)
    }
  })
  
  observeEvent(input$dataset,
               {
                   updateCheckboxGroupInput(session, input = "cohort_type", choices = new_data_cohort(), selected = "1 - UCPPS")
               })
  
  observeEvent(input$dataset,
               {
                 updateSelectInput(session, input = "visit_select", choices = visit_select_long())
               })
  
  observeEvent(input$dataset,
               {
                 updateSelectInput(session, input = "variable_group", choices = new_data_primary_grouping(), selected = "Study Feature")
               })
  
  observeEvent(input$variable_group,
               {
                 updateSelectInput(session, input = "variable_choice",
                                   choices = sort(variable_assignments_3_react()[variable_assignments_3_react()$variable_selection %in% input$variable_group,
                                                                         "variable", drop = TRUE]))
               })
  
  observeEvent(input$dataset,
               {
                 updateSelectInput(session, input = "variable_choice",
                                   choices = sort(variable_assignments_3_react()[variable_assignments_3_react()$variable_selection %in% input$variable_group,
                                                                                 "variable", drop = TRUE]))
               })
  
  output$variable_name <- renderText({
    variable_assignments_3_react()[variable_assignments_3_react()$variable %in% input$variable_choice, "label", drop = TRUE]
  })
  
  output$analysis_type <- renderText({
    variable_assignments_3_react()[variable_assignments_3_react()$variable %in% input$variable_choice, "ANALYSIS_TYPE_2", drop = TRUE]
  })
  
  observeEvent(input$dataset,
               {
                 updateSelectInput(session, input = "variable_group_2", choices = new_data_primary_grouping(), selected = "Study Feature")
               })
  
  observeEvent(input$variable_group_2,
               {
                 updateSelectInput(session, input = "variable_subgroup",
                                   choices = sort(variable_assignments_4_react()[variable_assignments_4_react()$variable_selection %in% input$variable_group_2,
                                                                         "variable", drop = TRUE]))
               })
  
  observeEvent(input$dataset,
               {
                 updateSelectInput(session, input = "variable_subgroup",
                                   choices = sort(variable_assignments_4_react()[variable_assignments_4_react()$variable_selection %in% input$variable_group_2,
                                                                                 "variable", drop = TRUE]))
               })
  
  output$variable_subgroup_name <- renderText({
    variable_assignments_4_react()[variable_assignments_4_react()$variable %in% input$variable_subgroup, "label", drop = TRUE]
  })
  
  
  output$analysis_type_sub <- renderText({
    variable_assignments_4_react()[variable_assignments_4_react()$variable %in% input$variable_subgroup, "ANALYSIS_TYPE_2", drop = TRUE]
  })
  
  
  new_data_0 <- reactive({
    mapp_bl_combined_react() %>%
      gather(columnNames, values) %>% 
      group_by(columnNames) %>% 
      distinct(.keep_all = TRUE) %>% 
      drop_na()
  })
  
  observeEvent(input$variable_subgroup,
               {
                 bb <- new_data_0()
                 
                 updateCheckboxGroupInput(session, input = "subgroup_options", choices = sort(bb[bb$columnNames %in% input$variable_subgroup, "values", drop = TRUE]), 
                                          selected = bb[bb$columnNames %in% input$variable_subgroup, "values", drop = TRUE])
               })
  
  observeEvent(input$dataset,
               {
                 bb <- new_data_0()
                 
                 updateCheckboxGroupInput(session, input = "subgroup_options", choices = sort(bb[bb$columnNames %in% input$variable_subgroup, "values", drop = TRUE]), 
                                          selected = bb[bb$columnNames %in% input$variable_subgroup, "values", drop = TRUE])
               })
  
  observeEvent(input$dataset,
               {
                 updateSelectInput(session, input = "variable_filter_group", choices = new_data_primary_grouping(), selected = "Demographics/Anthropometrics")
               })
  
  observeEvent(input$variable_filter_group,
               {
                 updateSelectInput(session, input = "variable_filter_choice",
                                   choices = sort(variable_assignments_4_react()[variable_assignments_4_react()$variable_selection %in% input$variable_filter_group, "variable", drop = TRUE]))
               })
  
  new_data <- reactive({
    mapp_bl_combined_react() %>%
      gather(columnNames, values) %>% 
      group_by(columnNames) %>% 
      distinct(.keep_all = TRUE) %>% 
      drop_na()
  })

  observeEvent(input$variable_filter_choice,
               {
                 bb <- new_data()
                 
                 updateCheckboxGroupInput(session, input = "checkbox_filter", choices = sort(bb[bb$columnNames %in% input$variable_filter_choice, "values", drop = TRUE]), 
                                          selected = bb[bb$columnNames %in% input$variable_filter_choice, "values", drop = TRUE])
               })
  
  observeEvent(input$checking_filter, {
                 bb <- new_data()
    
                 updateCheckboxGroupInput(session, input = "checkbox_filter", choices = sort(bb[bb$columnNames %in% input$variable_filter_choice, "values", drop = TRUE]), 
                                          selected = bb[bb$columnNames %in% input$variable_filter_choice, "values", drop = TRUE])
               })
  
  
  output$x_slider <- renderUI({
    req(input$variable_choice)
    
    w <- mapp_dataset() %>% 
      pull(input$variable_choice)
    
    max_v <- round(round(max(w, na.rm = TRUE)))
    min_v <- floor(floor(min(w, na.rm = TRUE)))
    
    sliderInput(
      inputId = "x_slider",
      label = "X-Axis Range",
      min = min_v,
      max = max_v,
      value = c(min_v,max_v),
      step = 1,
      ticks = TRUE,
      dragRange = TRUE,
      round = TRUE
    )
  })
  
  output$numeric_interval <- renderUI({
    numericInput(
      inputId = "numeric_interval",
      label = paste("X-Axis Intervals 1-10"),
      value = 2,
      min = 1,
      max = 10
    )
  })
  
  iv <- InputValidator$new()
  iv$add_rule("numeric_interval", sv_between(0, 10))
  iv$enable()
  
  observeEvent(input$variable_choice, {
    updateTabsetPanel(session, "tab1", selected = "Plot")
  })
  
  observeEvent(input$variable_choice, {
    updateTabsetPanel(session, "tab2", selected = "Plot")
  })
  
  observeEvent(input$variable_choice, {
    updateTabsetPanel(session, "tab3", selected = "Plot")
  })
  
  observeEvent(input$variable_choice, {
    updateTabsetPanel(session, "tab4", selected = "Plot")
  })
  
  observeEvent(input$variable_choice, {
    updateTabsetPanel(session, "tab5", selected = "Plot")
  })
  
  observeEvent(input$variable_choice, {
    updateTabsetPanel(session, "tab6", selected = "Plot")
  })
  
  observeEvent(input$variable_choice, {
    updateTabsetPanel(session, "tab7", selected = "Plot")
  })
  
  observeEvent(input$variable_choice, {
    updateTabsetPanel(session, "sub_tab1", selected = "Grouped")
  })
  
  observeEvent(input$variable_choice, {
    updateTabsetPanel(session, "sub_tab2", selected = "Grouped")
  })
  
  observeEvent(input$variable_choice, {
    updateTabsetPanel(session, "sub_tab3", selected = "Grouped")
  })
  
  observeEvent(input$variable_choice, {
    updateTabsetPanel(session, "sub_tab4", selected = "Grouped")
  })
  
  observeEvent(input$variable_choice, {
    updateTabsetPanel(session, "sub_tab5", selected = "Grouped")
  })
  
  observeEvent(input$variable_choice, {
    updateTabsetPanel(session, "sub_tab6", selected = "Grouped")
  })
  
  observeEvent(input$variable_choice, {
    updateTabsetPanel(session, "sub_tab7", selected = "Grouped")
  })
  
  observeEvent(input$tab1, {
    updateTabsetPanel(session, "tab_1", selected = "Histogram")
  })
  
  observeEvent(input$tab1, {
    updateTabsetPanel(session, "sub_tab1", selected = "Histogram")
  })
  
  observeEvent(input$tab2, {
    updateTabsetPanel(session, "tab_2", selected = "Bar Graph")
  })
  
  observeEvent(input$tab2, {
    updateTabsetPanel(session, "sub_tab2", selected = "Grouped")
  })
  
  observeEvent(input$tab3, {
    updateTabsetPanel(session, "tab_3", selected = "Bar Graph")
  })
  
  observeEvent(input$tab3, {
    updateTabsetPanel(session, "sub_tab3", selected = "Grouped")
  })
  
  observeEvent(input$tab4, {
    updateTabsetPanel(session, "tab_4", selected = "Bar Graph")
  })
  
  observeEvent(input$tab4, {
    updateTabsetPanel(session, "sub_tab4", selected = "Grouped")
  })
  
  observeEvent(input$tab5, {
    updateTabsetPanel(session, "tab_5", selected = "Bar Graph")
  })
  
  observeEvent(input$tab5, {
    updateTabsetPanel(session, "sub_tab5", selected = "Grouped")
  })
  
  observeEvent(input$tab6, {
    updateTabsetPanel(session, "tab_6", selected = "Bar Graph")
  })
  
  observeEvent(input$tab6, {
    updateTabsetPanel(session, "sub_tab6", selected = "Grouped")
  })
  
  observeEvent(input$tab7, {
    updateTabsetPanel(session, "tab_7", selected = "Bar Graph")
  })
  
  observeEvent(input$tab7, {
    updateTabsetPanel(session, "sub_tab7", selected = "Grouped")
  })
  
  
  gg <- reactive({
    req(input$variable_filter_choice)
    
    mapp_bl_combined_react() %>% 
      pull(input$variable_filter_choice)
  })
  
  ff <- reactive({
    req(input$variable_subgroup)
    
    mapp_bl_combined_react() %>% 
      pull(input$variable_subgroup)
  })
  
  full_vnum <- reactive({
    if(input$dataset == "MAPP I Longitudinal") {
      mapp_long_raw %>% 
        pull(vnum)
    }
    else {
      mapp_long_raw_II %>% 
        pull(vnum)
    }
  })
  
  
  #Filter by Cohort Selected
  filtered_data <- reactive({
    
    req(input$variable_filter_choice)
    req(input$cohort_type)
    req(input$checkbox_filter)
    req(input$variable_choice)
    req(input$subgroup_options)
    
    if(input$missing_data == "Yes") {
      mapp_bl_combined_react() %>%
        filter(if(input$dataset == "MAPP I Baseline" | input$dataset == "MAPP II Baseline") cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter 
                                                                                            else cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter 
                                                                                                 & full_vnum() %in% input$visit_select)
    }
    else {
      mapp_bl_combined_react() %>%
        filter(if(input$dataset == "MAPP I Baseline" | input$dataset == "MAPP II Baseline") cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter 
                                                                                            else cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter 
                                                                                                 & full_vnum() %in% input$visit_select) %>% 
        select(input$variable_choice) %>% 
        drop_na()
    }
  })
  
  #Used for Continuous Variables
  filtered_data_con <- reactive({
    
    req(input$missing_data)
    req(input$variable_choice)
    req(input$variable_filter_choice)
    req(input$cohort_type)
    req(input$checkbox_filter)
    req(input$subgroup_options)
    
    mapp_bl_combined_react() %>%
      filter(if(input$dataset == "MAPP I Baseline" | input$dataset == "MAPP II Baseline") cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter 
             else cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter 
             & full_vnum() %in% input$visit_select) %>% 
      select(input$variable_choice) %>% 
      drop_na()
  })
  
  filtered_data_conrow <- reactive({
    
    req(input$missing_data)
    req(input$variable_choice)
    req(input$variable_filter_choice)
    req(input$cohort_type)
    req(input$checkbox_filter)
    req(input$subgroup_options)
    if(input$dataset == "MAPP I Baseline" | input$dataset == "MAPP II Baseline") {
    mapp_bl_combined_react()[mapp_bl_combined_react()[input$variable_choice] >= min(input$x_slider) & mapp_bl_combined_react()[input$variable_choice] <= max(input$x_slider) & 
                               (mapp_bl_combined_react()$cohorttype2 %in% input$cohort_type) & (gg() %in% input$checkbox_filter),] %>% 
      drop_na(input$variable_choice)
    }
    else {
      mapp_bl_combined_react()[mapp_bl_combined_react()[input$variable_choice] >= min(input$x_slider) & mapp_bl_combined_react()[input$variable_choice] <= max(input$x_slider) & 
                                 (mapp_bl_combined_react()$cohorttype2 %in% input$cohort_type) & (gg() %in% input$checkbox_filter) & (full_vnum() %in% input$visit_select),] %>% 
        drop_na(input$variable_choice)
    }
  })
  
  #Strata for Continuous
  filtered_data_con_sub <- reactive({
    
    req(input$missing_data)
    req(input$variable_choice)
    req(input$variable_filter_choice)
    req(input$cohort_type)
    req(input$checkbox_filter)
    req(input$subgroup_options)
  
   if(input$dataset == "MAPP I Baseline" | input$dataset == "MAPP II Baseline") {
    if(input$missing_data_sub == "Yes") {
    mapp_bl_combined_react()[mapp_bl_combined_react()[input$variable_choice] >= min(input$x_slider) & mapp_bl_combined_react()[input$variable_choice] <= max(input$x_slider) & 
                               (mapp_bl_combined_react()$cohorttype2 %in% input$cohort_type) & (gg() %in% input$checkbox_filter) & (ff() %in% input$subgroup_options),] %>% 
      select(input$variable_choice, input$variable_subgroup) %>% 
      drop_na(input$variable_choice) %>% 
      mutate_if(is.factor, fct_explicit_na, na_level = "Missing Observations")
    }
    else {
      mapp_bl_combined_react()[mapp_bl_combined_react()[input$variable_choice] >= min(input$x_slider) & mapp_bl_combined_react()[input$variable_choice] <= max(input$x_slider) & 
                                 (mapp_bl_combined_react()$cohorttype2 %in% input$cohort_type) & (gg() %in% input$checkbox_filter) & (ff() %in% input$subgroup_options),] %>% 
        select(input$variable_choice, input$variable_subgroup) %>% 
        drop_na()
    }
  } 
  else {
    if(input$missing_data_sub == "Yes") {
      mapp_bl_combined_react()[mapp_bl_combined_react()[input$variable_choice] >= min(input$x_slider) & mapp_bl_combined_react()[input$variable_choice] <= max(input$x_slider) & 
                                 (mapp_bl_combined_react()$cohorttype2 %in% input$cohort_type) & (gg() %in% input$checkbox_filter) & (ff() %in% input$subgroup_options) & 
                                 (full_vnum() %in% input$visit_select),] %>% 
        select(input$variable_choice, input$variable_subgroup) %>% 
        drop_na(input$variable_choice) %>% 
        mutate_if(is.factor, fct_explicit_na, na_level = "Missing Observations")
    }
    else {
      mapp_bl_combined_react()[mapp_bl_combined_react()[input$variable_choice] >= min(input$x_slider) & mapp_bl_combined_react()[input$variable_choice] <= max(input$x_slider) & 
                                 (mapp_bl_combined_react()$cohorttype2 %in% input$cohort_type) & (gg() %in% input$checkbox_filter) & (ff() %in% input$subgroup_options) & 
                                 (full_vnum() %in% input$visit_select),] %>% 
        select(input$variable_choice, input$variable_subgroup) %>% 
        drop_na()
    }
  }
  })
  
  filtered_data_2 <- reactive({
    
    req(input$missing_data)
    req(input$variable_choice)
    req(input$variable_filter_choice)
    req(input$cohort_type)
    req(input$checkbox_filter)
    req(input$subgroup_options)
    
   if(input$dataset == "MAPP I Baseline" | input$dataset == "MAPP II Baseline") {
    if(input$missing_data == "Yes") {
      mapp_bl_combined_react()[(mapp_bl_combined_react()$cohorttype2 %in% input$cohort_type) & (gg() %in% input$checkbox_filter) & (ff() %in% input$subgroup_options),]
    }
    else{
      mapp_bl_combined_react() %>%
        filter(cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter & ff() %in% input$subgroup_options) %>% 
        select(input$variable_choice) %>% 
        drop_na()
    }
   }
   else {
     if(input$missing_data == "Yes") {
       mapp_bl_combined_react()[(mapp_bl_combined_react()$cohorttype2 %in% input$cohort_type) & (gg() %in% input$checkbox_filter) & (ff() %in% input$subgroup_options) &
                                  (full_vnum() %in% input$visit_select),]
     }
     else {
       mapp_bl_combined_react() %>%
         filter(cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter & ff() %in% input$subgroup_options & full_vnum() %in% input$visit_select) %>% 
         select(input$variable_choice) %>% 
         drop_na()
     }
   }
  })
  
  filtered_data_2a <- reactive({
    
    req(input$variable_choice)
    req(input$x_slider)
    req(input$cohort_type)
    req(input$checkbox_filter)
    req(input$subgroup_options)
    
   if(input$dataset == "MAPP I Baseline" | input$dataset == "MAPP II Baseline") {
    if(input$missing_data == "Yes") {
      mapp_bl_combined_react()[mapp_bl_combined_react()[input$variable_choice] >= min(input$x_slider) & mapp_bl_combined_react()[input$variable_choice] <= max(input$x_slider) & 
                  (mapp_bl_combined_react()$cohorttype2 %in% input$cohort_type) & (gg() %in% input$checkbox_filter),]
    }
    else {
      mapp_bl_combined_react()[mapp_bl_combined_react()[input$variable_choice] >= min(input$x_slider) & mapp_bl_combined_react()[input$variable_choice] <= max(input$x_slider) & 
                                 (mapp_bl_combined_react()$cohorttype2 %in% input$cohort_type) & (gg() %in% input$checkbox_filter),] %>% 
        drop_na(input$variable_choice)
    }
   }
    else {
      if(input$missing_data == "Yes") {
        mapp_bl_combined_react()[mapp_bl_combined_react()[input$variable_choice] >= min(input$x_slider) & mapp_bl_combined_react()[input$variable_choice] <= max(input$x_slider) & 
                                   (mapp_bl_combined_react()$cohorttype2 %in% input$cohort_type) & (gg() %in% input$checkbox_filter) &
                                   (full_vnum() %in% input$visit_select),]
      }
      else {
        mapp_bl_combined_react()[mapp_bl_combined_react()[input$variable_choice] >= min(input$x_slider) & mapp_bl_combined_react()[input$variable_choice] <= max(input$x_slider) & 
                                   (mapp_bl_combined_react()$cohorttype2 %in% input$cohort_type) & (gg() %in% input$checkbox_filter) &
                                   (full_vnum() %in% input$visit_select),] %>% 
          drop_na(input$variable_choice)
      }
    }
    
  })
  
  filtered_data_2b <- reactive({
    
    req(input$variable_choice)
    req(input$variable_filter_choice)
    req(input$cohort_type)
    req(input$checkbox_filter)
    
    if(input$missing_data == "Yes") {
      mapp_bl_combined_react() %>%
        filter(if(input$dataset == "MAPP I Baseline" | input$dataset == "MAPP II Baseline") cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter 
               else cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter & full_vnum() %in% input$visit_select)
    }
    else {
      mapp_bl_combined_react() %>%
        filter(if(input$dataset == "MAPP I Baseline" | input$dataset == "MAPP II Baseline") cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter 
               else cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter & full_vnum() %in% input$visit_select) %>% 
        select(input$variable_choice) %>% 
        drop_na()
    }
    
  })
  
  #Filter by Subgroup
  filtered_data_3 <- reactive({
    
    req(input$variable_choice)
    req(input$variable_filter_choice)
    req(input$cohort_type)
    req(input$checkbox_filter)
    
    if(input$missing_data == "Yes") {
      mapp_bl_combined_react() %>% 
        filter(if(input$dataset == "MAPP I Baseline" | input$dataset == "MAPP II Baseline") cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter 
               else cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter & ff() %in% input$subgroup_options & full_vnum() %in% input$visit_select)
    }
    
    else {
      mapp_bl_combined_react() %>% 
        filter(if(input$dataset == "MAPP I Baseline" | input$dataset == "MAPP II Baseline") cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter 
               else cohorttype2 %in% input$cohort_type & gg() %in% input$checkbox_filter & ff() %in% input$subgroup_options & full_vnum() %in% input$visit_select) %>% 
        select(input$variable_subgroup, input$variable_choice) %>% 
        drop_na()
    }
  })
  
  cohort_label <- reactive({
    req(input$cohort_type)
    
    if(input$captions == "Yes") {
      paste("Cohort: ", paste(unique(input$cohort_type[input$cohort_type %in% mapp_bl_combined_react()$cohorttype2]), collapse = ", "))
    }
    else {
      return("")
    }
  })
  
  filter_label <- reactive({
    
    req(input$variable_filter_choice)
    req(input$checkbox_filter)
    
    v <- paste(full_labels_react()[full_labels_react()$variable %in% input$variable_filter_choice, "variable_label"], ":")
    
    if(input$checking_filter == 1 & input$captions == "Yes") {
      paste(v, paste(unique(input$checkbox_filter[input$checkbox_filter %in% gg()]), collapse = ", "))
    }
    else {
      return("")
    }
  })
  
  download_type_1 <- reactive(
    if(input$download_type == ".pdf") {
      ".pdf"
    }
    else {
      ".png"
    }
  )
  
  download_type_2 <- reactive(
    if(input$download_type == ".pdf") {
      "pdf"
    }
    else {
      "png"
    }
  )
  
  output$download_plot_1 <- downloadHandler(
    filename = function() {
      paste("plot-", Sys.Date(), download_type_1(), sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = download_type_2(), dpi = 600, width = 10, height = 8, units = "in")
    }
  )
  
  #Main Plots
  output$plot <- renderPlot({
    
    validate(need(nrow(filtered_data_con()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_conrow()) > 0, message = FALSE))
    
    req(input$numeric_interval)
    req(input$x_slider)
    req(input$variable_choice)
    req(input$fill_color)
    req(input$variable_filter_choice)
    req(input$cohort_type)
    req(input$checkbox_filter)
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    
    a <- min(input$x_slider) - 1
    b <- max(input$x_slider)
    c <- min(input$x_slider)
    d <- max(input$x_slider) + 1
    
    con_hist <- ggplot(filtered_data_con(), aes_string(input$variable_choice)) + 
      geom_histogram(color = "black", binwidth = 1, fill = input$fill_color) +
      geom_density(aes(y=..count..), color = "black", adjust = 4) +
      theme_bw() +
      scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval)) +
      labs(x = "", y = "Count", title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_conrow()), "|", dataset_subtitle()), 
           caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "none", axis.text = element_text(size = 18), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
    
    con_box <- ggplot(filtered_data_con(), aes_string(input$variable_choice)) +
      geom_boxplot() +
      geom_jitter(aes_string(input$variable_choice, 0), color = input$fill_color) +
      theme_minimal() +
      coord_cartesian(ylim = c(-1, 1)) +
      scale_x_continuous(limits = c(a, b), breaks = seq(a, b, by = input$numeric_interval)) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      labs(x = "", y = "")
    
    print_1 <- plot_grid(con_hist, con_box, ncol = 1, rel_heights = c(5, 2), align = 'v', axis = 'lr')
    
    print_1
  })
  
  
  output$plot_2 <- renderPlot({
    
    validate(need(nrow(filtered_data()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data(), aes_string(input$variable_choice, fill = input$variable_choice)) +
      geom_bar() +
      theme_bw() +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data()), "|", dataset_subtitle()), 
           caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "none", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$plot_3 <- renderPlot({
    
    validate(need(nrow(filtered_data()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data(), aes_string(input$variable_choice, fill = input$variable_choice)) +
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      scale_y_continuous(limits = c(0, 1)) +
      theme_bw() +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data()), "|", dataset_subtitle()), 
           caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "none", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$plot_4 <- renderPlot({
    
    validate(need(nrow(filtered_data()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data(), aes_string(input$variable_choice, fill = input$variable_choice)) +
      geom_bar() +
      theme_bw() +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data()), "|", dataset_subtitle()), 
           caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "none", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$plot_5 <- renderPlot({
    
    validate(need(nrow(filtered_data()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data(), aes_string(input$variable_choice, fill = input$variable_choice)) +
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      scale_y_continuous(limits = c(0, 1)) +
      theme_bw() +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data()), "|", dataset_subtitle()), 
           caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "none", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$plot_6 <- renderPlot({
    
    validate(need(nrow(filtered_data()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data(), aes_string(input$variable_choice, fill = input$variable_choice)) + 
      geom_bar() +
      theme_bw() +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data()), "|", dataset_subtitle()), 
           caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "none", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$plot_7 <- renderPlot({
    
    validate(need(nrow(filtered_data()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data(), aes_string(input$variable_choice, fill = input$variable_choice)) +
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      scale_y_continuous(limits = c(0, 1)) +
      theme_bw() +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data()), "|", dataset_subtitle()), 
           caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "none", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$sub_plot1 <- renderPlot({
    
    validate(need(nrow(filtered_data_con_sub()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    
    a <- min(input$x_slider) - 1
    b <- max(input$x_slider)
    c <- min(input$x_slider)
    d <- max(input$x_slider) + 1
    
    ggplot(filtered_data_con_sub(), aes_string(input$variable_choice, color = input$variable_subgroup, fill = input$variable_subgroup)) + 
      geom_histogram(binwidth = 1, position = "stack", alpha = 0.5) +
      scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval)) +
      theme_bw() +
      labs(x = "", y = "Count", fill = str_wrap(y, 25), color = str_wrap(y, 25), title = str_wrap(x, 70), 
           subtitle = paste("Number of Observations: ", nrow(filtered_data_con_sub()), "|", dataset_subtitle()), 
           caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
            legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$sub_plot2 <- renderPlot({
    
    
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_2()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data_3(), aes_string(input$variable_choice, fill = input$variable_subgroup)) +
      geom_bar(position = "dodge") +
      theme_bw() +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_2()), "|", dataset_subtitle()), fill = str_wrap(y, 25), 
           color = str_wrap(y, 25), caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16), legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$sub_plot3 <- renderPlot({
    
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_2()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data_3(), aes_string(input$variable_choice, fill = input$variable_subgroup)) +
      geom_bar(aes(y = (..count..)/sum(..count..)), position = "dodge") +
      theme_bw() +
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_2()), "|", dataset_subtitle()), fill = str_wrap(y, 25), 
           color = str_wrap(y, 25), caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16), legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$sub_plot4 <- renderPlot({
    
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_2()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data_3(), aes_string(input$variable_choice, fill = input$variable_subgroup)) +
      geom_bar(position = "dodge") +
      theme_bw() +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_2()), "|", dataset_subtitle()), fill = str_wrap(y, 25), 
           color = str_wrap(y, 25), caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16),legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$sub_plot5 <- renderPlot({
    
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_2()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data_3(), aes_string(input$variable_choice, fill = input$variable_subgroup)) +
      geom_bar(aes(y = (..count..)/sum(..count..)), position = "dodge", axis.text = element_text(size = 18)) +
      theme_bw() +
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_2()), "|", dataset_subtitle()), fill = str_wrap(y, 25), 
           color = str_wrap(y, 25), caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16), legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$sub_plot6 <- renderPlot({
    
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_2()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data_3(), aes_string(input$variable_choice, fill = input$variable_subgroup)) +
      geom_bar(position = "dodge") +
      theme_bw() +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_2()), "|", dataset_subtitle()), fill = str_wrap(y, 25), 
           color = str_wrap(y, 25), caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16),legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$sub_plot7 <- renderPlot({
    
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_2()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data_3(), aes_string(input$variable_choice, fill = input$variable_subgroup)) +
      geom_bar(aes(y = (..count..)/sum(..count..)), position = "dodge") +
      theme_bw() +
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_2()), "|", dataset_subtitle()), fill = str_wrap(y, 25), 
           color = str_wrap(y, 25), caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16), legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$sub_plot8 <- renderPlot({
    
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_2()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data_3(), aes_string(input$variable_choice, fill = input$variable_subgroup)) +
      geom_bar(position = "stack") +
      theme_bw() +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_2()), "|", dataset_subtitle()), fill = str_wrap(y, 25), 
           color = str_wrap(y, 25), caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16), legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$sub_plot9 <- renderPlot({
    
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_2()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data_3(), aes_string(input$variable_choice, fill = input$variable_subgroup)) +
      geom_bar(aes(y = (..count..)/sum(..count..)), position = "stack") +
      theme_bw() +
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_2()), "|", dataset_subtitle()), fill = str_wrap(y, 25), 
           color = str_wrap(y, 25), caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16), legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$sub_plot10 <- renderPlot({
    
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_2()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data_3(), aes_string(input$variable_choice, fill = input$variable_subgroup)) +
      geom_bar(position = "stack") +
      theme_bw() +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_2()), "|", dataset_subtitle()), fill = str_wrap(y, 25), 
           color = str_wrap(y, 25), caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16), legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$sub_plot11 <- renderPlot({
    
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_2()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data_3(), aes_string(input$variable_choice, fill = input$variable_subgroup)) +
      geom_bar(aes(y = (..count..)/sum(..count..)), position = "stack") +
      theme_bw() +
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_2()), "|", dataset_subtitle()), fill = str_wrap(y, 25), 
           color = str_wrap(y, 25), caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16),legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$sub_plot12 <- renderPlot({
    
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_2()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data_3(), aes_string(input$variable_choice, fill = input$variable_subgroup)) +
      geom_bar(position = "stack") +
      theme_bw() +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_2()), "|", dataset_subtitle()), fill = str_wrap(y, 25), 
           color = str_wrap(y, 25), caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16), legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$sub_plot13 <- renderPlot({
    
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_2()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    axis_angle <- input$axis_angle
    text_size <- input$text_size
    
    ggplot(filtered_data_3(), aes_string(input$variable_choice, fill = input$variable_subgroup)) +
      geom_bar(aes(y = (..count..)/sum(..count..)), position = "stack") +
      scale_y_continuous(limits = c(0, 1)) +
      theme_bw() +
      labs(x = "", y = input$display_type, title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_2()), "|", dataset_subtitle()), fill = str_wrap(y, 25), 
           color = str_wrap(y, 25), caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size), 
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16), legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$boxplot_sub1 <- renderPlot({
    
    validate(need(nrow(filtered_data_con_sub()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    a <- min(input$x_slider)
    b <- max(input$x_slider)
    
    ggplot(filtered_data_con_sub(), aes_string(input$variable_choice, fill = input$variable_subgroup)) +
      geom_boxplot() +
      geom_jitter(aes_string(input$variable_choice, 0, width = 0.25)) +
      theme_bw() +
      coord_cartesian(ylim = c(-1, 1)) +
      scale_x_continuous(limits = c(a, b), breaks = seq(a, b, by = input$numeric_interval)) +
      labs(x = "", y = "", title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_con_sub()), "|", dataset_subtitle()), fill = str_wrap(y, 25), 
           color = str_wrap(y, 25), caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title = element_text(size = 18, face = "bold"), 
            legend.text = element_text(size = 16), legend.title = element_text(size = 16), legend.position = "bottom", 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
      
  })
  
  output$density_plot1 <- renderPlot({
    
    validate(need(nrow(filtered_data_con_sub()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    a <- min(input$x_slider)
    b <- max(input$x_slider)
    
    ggplot(filtered_data_con_sub(), aes_string(input$variable_choice, fill = input$variable_subgroup)) +
      geom_density(alpha = 0.5) +
      theme_bw() +
      scale_x_continuous(limits = c(a, b), breaks = seq(a, b, by = input$numeric_interval)) +
      labs(x = "", y = "Density", title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_con_sub()), "|", dataset_subtitle()), fill = str_wrap(y, 25), 
           color = str_wrap(y, 25), caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
            legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  output$density_plot2 <- renderPlot({
    
    validate(need(nrow(filtered_data_con()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_conrow()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    a <- min(input$x_slider)
    b <- max(input$x_slider)
    
    con_dens <- ggplot(filtered_data_con(), aes_string(input$variable_choice)) +
      geom_density(fill = input$fill_color) +
      theme_bw() +
      scale_x_continuous(limits = c(a, b), breaks = seq(a, b, by = input$numeric_interval)) +
      labs(x = "", y = "Density", title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_conrow()), "|", dataset_subtitle()), 
           caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = 18), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
    
    con_box <- ggplot(filtered_data_con(), aes_string(input$variable_choice)) +
      geom_boxplot() +
      geom_jitter(aes_string(input$variable_choice, 0), color = input$fill_color) +
      theme_minimal() +
      scale_x_continuous(limits = c(a, b), breaks = seq(a, b, by = input$numeric_interval)) +
      coord_cartesian(ylim = c(-1, 1)) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      labs(y = "", x = "")
    
    plot_grid(con_dens, con_box, ncol = 1, rel_heights = c(5, 2), align = 'v', axis = 'lr')
  })
  
  output$freq_plot1 <- renderPlot({
    
    validate(need(nrow(filtered_data_con_sub()) > 0, message = FALSE))
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    a <- min(input$x_slider)
    b <- max(input$x_slider)
    
    ggplot(filtered_data_con_sub(), aes_string(input$variable_choice, color = input$variable_subgroup)) +
      geom_freqpoly(binwidth = 1) +
      theme_bw() +
      scale_x_continuous(limits = c(a, b), breaks = seq(a, b, by = input$numeric_interval)) +
      labs(x = "", y = "Count", title = str_wrap(x, 70), subtitle = paste("Number of Observations: ", nrow(filtered_data_con_sub()), "|", dataset_subtitle()), fill = str_wrap(y, 25), 
           color = str_wrap(y, 25), caption = paste("", cohort_label(), "\n", str_wrap(filter_label(), 100))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
            legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  })
  
  
  #SUMMARY 1, 2, 3, 4, 5, 6, 7
  
  ##################################
  my_table_1 <- reactive({
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    
    filtered_data_2a() %>% 
      select(input$variable_choice) %>% 
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice) ~ x,
        type = list(where(is.numeric) ~ "continuous2"),
        statistic = all_continuous() ~ c("{N_nonmiss}",
                                         "{median} ({p25}, {p75})", 
                                         "{min}, {max}",
                                         "{mean} ({sd})"),
        digits = all_continuous() ~ c(0, 2, 2, 2, 0, 0, 2, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>%
      modify_header(label = "**Variable**") %>%
      modify_caption("Summary Table") %>%
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>%
      as_gt()
  })
  
  my_image_1 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_1(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t1 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_1(), file)
    },
    )
  
  output$summary_1 <- render_gt({
    validate(need(nrow(filtered_data_2a()) > 0, message = FALSE))
    my_table_1()
  })
  
  ##################################
  my_table_2 <- reactive({
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    
    filtered_data() %>% 
      select(input$variable_choice) %>% 
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice) ~ x,
        type = all_categorical() ~ "categorical",
        statistic = all_categorical() ~ "{n} ({p}%)",
        digits = all_categorical() ~ c(0, 2)) %>%
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>%
      modify_header(label = "**Variable**") %>%
      modify_caption("Summary Table") %>%
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>%
      as_gt()
  })
  
  my_image_2 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_2(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t2 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_2(), file)
      
    },
  )
  output$summary_2 <- render_gt({
    
    validate(need(nrow(filtered_data()) > 0, message = FALSE))
    my_table_2()
  })
  #########################################################################
  my_table_3 <- reactive({
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    
    filtered_data() %>% 
      select(input$variable_choice) %>%
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice) ~ x,
        type = all_categorical() ~ "categorical",
        statistic = all_categorical() ~ "{n} ({p}%)",
        digits = all_categorical() ~ c(0, 2)) %>%
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>% 
      modify_header(label = "**Variable**") %>%
      modify_caption("Summary Table") %>%
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>%
      as_gt()
  })
  
  my_image_3 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_3(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t3 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_3(), file)
      
    },
  )
  
  output$summary_3 <- render_gt({
    
    validate(need(nrow(filtered_data()) > 0, message = FALSE))
    my_table_3()
    
  })
  
  ##################################################################
  my_table_4 <- reactive({
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    
    filtered_data() %>% 
      select(input$variable_choice) %>% 
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice) ~ x,
        type = all_categorical() ~ "categorical",
        statistic = all_categorical() ~ "{n} ({p}%)",
        digits = all_categorical() ~ c(0, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>% 
      modify_header(label = "**Variable**") %>%
      modify_caption("Summary Table") %>%
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
      as_gt()
  })
  
  my_image_4 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_4(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)

    outfile
  })
  
  output$download_t4 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_4(), file)
      
    },
  )
  
  output$summary_4 <- render_gt({
    
    validate(need(nrow(filtered_data()) > 0, message = FALSE))
    my_table_4()

  })
  
  ########################################################################
  my_table_5 <- reactive({
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    
    filtered_data() %>% 
      select(input$variable_choice) %>% 
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice) ~ x,
        type = all_categorical() ~ "categorical",
        statistic = all_categorical() ~ "{n} ({p}%)",
        digits = all_categorical() ~ c(0, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>% 
      modify_header(label = "**Variable**") %>%
      modify_caption("Summary Table") %>% 
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
      as_gt()
  })
  
  my_image_5 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_5(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t5 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_5(), file)
      
    },
  )
  
  output$summary_5 <- render_gt({
    
    validate(need(nrow(filtered_data()) > 0, message = FALSE))
    my_table_5()
    
  })
  
  #####################################################################
  my_table_6 <- reactive({
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    
    filtered_data() %>% 
      select(input$variable_choice) %>% 
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice) ~ x,
        type = all_categorical() ~ "categorical",
        statistic = all_categorical() ~ "{n} ({p}%)",
        digits = all_categorical() ~ c(0, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>% 
      modify_header(label = "**Variable**") %>%
      modify_caption("Summary Table") %>% 
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
      as_gt()
  })
  
  my_image_6 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_6(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t6 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_6(), file)
      
    },
  )
  
  output$summary_6 <- render_gt({
    
    validate(need(nrow(filtered_data()) > 0, message = FALSE))
    my_table_6()

  })
  
  #########################################################
  my_table_7 <- reactive({
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    
    filtered_data() %>% 
      select(input$variable_choice) %>% 
      tbl_summary(
        missing = "ifany",
        label = !!sym(input$variable_choice) ~ x,
        missing_text = "Missing Observations",
        type = all_categorical() ~ "categorical",
        statistic = all_categorical() ~ "{n} ({p}%)",
        digits = all_categorical() ~ c(0, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>% 
      modify_header(label = "**Variable**") %>%
      modify_caption("Summary Table") %>% 
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
      as_gt()
  })
  
  my_image_7 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_7(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t7 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_7(), file)
      
    },
  )
  
  output$summary_7 <- render_gt({
    
    validate(need(nrow(filtered_data()) > 0, message = FALSE))
    my_table_7()

  })
  ########################################################################################
  
  
  #Subgroup Summaries
  #########################################################################
  jj <- reactive({
    factored %>% 
      select(input$variable_subgroup) %>% 
      distinct() %>% 
      gather(columnNames, values) %>% 
      mutate(stat_count = substr(values, 1, 1)) %>% 
      select(stat_count) %>% 
      drop_na()
  })
  
  oo <- reactive({
  
    `%notin%` <- Negate(`%in%`)
    req(input$subgroup_options)
    req(input$variable_subgroup)
    req(input$variable_filter_choice)
    req(input$checkbox_filter)
  
   if(0 %notin% jj()$stat_count) {
    if(input$variable_subgroup == input$variable_filter_choice) {
      factored %>% 
        select(input$variable_subgroup) %>% 
        distinct() %>% 
        gather(columnNames, values) %>% 
        mutate(stat_count = substr(values, 1, 1)) %>% 
        filter(values %notin% input$subgroup_options | values %notin% input$checkbox_filter) %>% 
        select(stat_count) %>% 
        drop_na()
      
    } else if (input$variable_subgroup == "cohorttype2") {
      factored %>% 
        select(input$variable_subgroup) %>% 
        distinct() %>% 
        gather(columnNames, values) %>% 
        mutate(stat_count = substr(values, 1, 1)) %>% 
        filter(values %notin% input$cohort_type | values %notin% input$subgroup_options) %>% 
        select(stat_count) %>% 
        drop_na()
      
    } else {
      factored %>% 
        select(input$variable_subgroup) %>% 
        distinct() %>% 
        gather(columnNames, values) %>% 
        mutate(stat_count = substr(values, 1, 1)) %>% 
        filter(values %notin% input$subgroup_options) %>% 
        select(stat_count) %>% 
        drop_na()
    }
     
  } else {
    if(input$variable_subgroup == input$variable_filter_choice) {
      factored %>% 
        select(input$variable_subgroup) %>% 
        distinct() %>% 
        gather(columnNames, values) %>% 
        mutate(stat_count = substr(values, 1, 1)) %>% 
        filter(values %notin% input$subgroup_options | values %notin% input$checkbox_filter) %>% 
        select(stat_count) %>% 
        drop_na() %>% 
        mutate(stat_count = as.integer(stat_count) + 1)
      
    } else if (input$variable_subgroup == "cohorttype2") {
      factored %>% 
        select(input$variable_subgroup) %>% 
        distinct() %>% 
        gather(columnNames, values) %>% 
        mutate(stat_count = substr(values, 1, 1)) %>% 
        filter(values %notin% input$cohort_type | values %notin% input$subgroup_options) %>% 
        select(stat_count) %>% 
        drop_na() %>% 
        mutate(stat_count = as.integer(stat_count) + 1)
      
    } else {
      factored %>% 
        select(input$variable_subgroup) %>% 
        distinct() %>% 
        gather(columnNames, values) %>% 
        mutate(stat_count = substr(values, 1, 1)) %>% 
        filter(values %notin% input$subgroup_options) %>% 
        select(stat_count) %>% 
        drop_na() %>% 
        mutate(stat_count = as.integer(stat_count) + 1)
    }
   }
  })
  
  gt_stat_count <- reactive({
    
    ww <- NULL
    for (i in nrow(oo())) {
      ww <- paste0("stat_", oo()$stat_count)
    }
    
    ww
    
  })
  
  my_table_8 <- reactive({
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    
    if(gt_stat_count() == "stat_") {
      
      filtered_data_con_sub() %>%
        tbl_summary(
          by = input$variable_subgroup,
          missing = "ifany",
          missing_text = "Missing Observations",
          label = !!sym(input$variable_choice) ~ x,
          type = list(where(is.numeric) ~ "continuous2"),
          statistic = all_continuous() ~ c("{N_nonmiss}",
                                           "{median} ({p25}, {p75})",
                                           "{min}, {max}",
                                           "{mean} ({sd})"),
          digits = all_continuous() ~ c(0, 2, 2, 2, 0, 0, 2, 2)) %>%
        modify_spanning_header(all_stat_cols() ~ paste(y)) %>% 
        modify_header(label = "**Variable**") %>% 
        modify_caption("Subgroup Summary Table") %>%
        modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
        add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
        as_gt()
      
    } else {
      
      filtered_data_con_sub() %>%
        tbl_summary(
          by = input$variable_subgroup,
          missing = "ifany",
          missing_text = "Missing Observations",
          label = !!sym(input$variable_choice) ~ x,
          type = list(where(is.numeric) ~ "continuous2"),
          statistic = all_continuous() ~ c("{N_nonmiss}",
                                           "{median} ({p25}, {p75})",
                                           "{min}, {max}",
                                           "{mean} ({sd})"),
          digits = all_continuous() ~ c(0, 2, 2, 2, 0, 0, 2, 2)) %>%
        modify_spanning_header(all_stat_cols() ~ paste(y)) %>% 
        modify_header(label = "**Variable**") %>% 
        modify_caption("Subgroup Summary Table") %>%
        modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
        modify_column_hide(columns = all_of(gt_stat_count())) %>%
        add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
        as_gt()
    }
    
  })
  
  my_image_8 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_8(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t8 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_8(), file)
      
    },
  )
  
  output$summary_sub1 <- render_gt({
    
    validate(need(nrow(filtered_data_con_sub()) > 0, message = FALSE))
    my_table_8()
    
  })
  #################################################################
  my_table_9 <- reactive({
  
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    
   if(input$variable_choice != input$variable_subgroup) {
    if(gt_stat_count() == "stat_") {
    filtered_data_3() %>% 
      select(input$variable_choice, input$variable_subgroup) %>% 
      tbl_cross(
        row = input$variable_choice,
        col = input$variable_subgroup,
        percent = "cell",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice) ~ x,
        digits = c(0, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ paste(y)) %>% 
      modify_header(label = "**Variable**") %>%
      modify_caption("Subgroup Summary Table") %>% 
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
      add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
      as_gt()
      
    } else {
      filtered_data_3() %>% 
        select(input$variable_choice, input$variable_subgroup) %>% 
        tbl_cross(
          row = input$variable_choice,
          col = input$variable_subgroup,
          percent = "cell",
          missing_text = "Missing Observations",
          label = !!sym(input$variable_choice) ~ x,
          digits = c(0, 2)) %>% 
        modify_spanning_header(all_stat_cols() ~ paste(y)) %>% 
        modify_header(label = "**Variable**") %>%
        modify_caption("Subgroup Summary Table") %>% 
        modify_column_hide(columns = all_of(gt_stat_count())) %>%
        modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
        add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
        as_gt()
    }
 } else {
     
       filtered_data() %>% 
         select(input$variable_choice) %>% 
         tbl_summary(
           missing = "ifany",
           label = !!sym(input$variable_choice) ~ x,
           missing_text = "Missing Observations",
           type = all_categorical() ~ "categorical",
           statistic = all_categorical() ~ "{n} ({p}%)",
           digits = all_categorical() ~ c(0, 2)) %>%
         modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>%
         modify_header(label = "**Variable**") %>%
         modify_caption("Summary Table") %>%
         modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>%
         as_gt()
   }
  })
  
  my_image_9 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_9(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t9 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_9(), file)
      
    },
  )
  
  output$summary_sub2 <- render_gt({
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    my_table_9()
  })
  ###################################################################################
  my_table_10 <- reactive({
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    
  if(input$variable_choice != input$variable_subgroup) {
    if(gt_stat_count() == "stat_") {
      
    filtered_data_3() %>% 
      select(input$variable_choice, input$variable_subgroup) %>% 
      tbl_cross(
        row = input$variable_choice,
        col = input$variable_subgroup,
        percent = "cell",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice) ~ x,
        digits = c(0, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ paste(y)) %>% 
      modify_header(label = "**Variable**") %>%
      modify_caption("Subgroup Summary Table") %>% 
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
      add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
      as_gt()
  
    } else {
    
    filtered_data_3() %>% 
      select(input$variable_choice, input$variable_subgroup) %>% 
      tbl_cross(
        row = input$variable_choice,
        col = input$variable_subgroup,
        percent = "cell",
        missing_text = "Missing Observations",
        digits = c(0, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ paste(y)) %>% 
      modify_header(label = "**Variable**") %>%
      modify_caption("Subgroup Summary Table") %>% 
      modify_column_hide(columns = all_of(gt_stat_count())) %>% 
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
      add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
      as_gt()
  }
    
} else {
    filtered_data() %>% 
      select(input$variable_choice) %>% 
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        type = all_categorical() ~ "categorical",
        label = !!sym(input$variable_choice) ~ x,
        statistic = all_categorical() ~ "{n} ({p}%)",
        digits = all_categorical() ~ c(0, 2)) %>%
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>%
      modify_header(label = "**Variable**") %>%
      modify_caption("Summary Table") %>%
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>%
      as_gt()
  }
  })
  
  my_image_10 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_10(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t10 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_10(), file)
      
    },
  )
  
  output$summary_sub3 <- render_gt({
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    my_table_10()
  })
  ########################################################################################
  my_table_11 <- reactive({
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
  
  if(input$variable_choice != input$variable_subgroup) {
    if(gt_stat_count() == "stat_") {
    filtered_data_3() %>% 
      select(input$variable_choice, input$variable_subgroup) %>% 
      tbl_cross(
        row = input$variable_choice,
        col = input$variable_subgroup,
        percent = "cell",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice) ~ x,
        digits = c(0, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ paste(y)) %>% 
      modify_header(label = "**Variable**") %>%
      modify_caption("Subgroup Summary Table") %>% 
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
      add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
      as_gt()
      
    } else {
      filtered_data_3() %>% 
        select(input$variable_choice, input$variable_subgroup) %>% 
        tbl_cross(
          row = input$variable_choice,
          col = input$variable_subgroup,
          percent = "cell",
          missing_text = "Missing Observations",
          label = !!sym(input$variable_choice) ~ x,
          digits = c(0, 2)) %>% 
        modify_spanning_header(all_stat_cols() ~ paste(y)) %>% 
        modify_header(label = "**Variable**") %>%
        modify_caption("Subgroup Summary Table") %>% 
        modify_column_hide(columns = all_of(gt_stat_count())) %>%
        modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
        add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
        as_gt()
    }
      
  } else {
      filtered_data() %>% 
        select(input$variable_choice) %>% 
        tbl_summary(
          missing = "ifany",
          missing_text = "Missing Observations",
          label = !!sym(input$variable_choice) ~ x,
          type = all_categorical() ~ "categorical",
          statistic = all_categorical() ~ "{n} ({p}%)",
          digits = all_categorical() ~ c(0, 2)) %>%
        modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>%
        modify_header(label = "**Variable**") %>%
        modify_caption("Summary Table") %>%
        modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>%
        as_gt()
    }
  })
  
  my_image_11 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_11(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t11 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_11(), file)
      
    },
  )
  
  output$summary_sub4 <- render_gt({
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    my_table_11()
  })
  ##########################################################################################
  my_table_12 <- reactive({
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    
  if(input$variable_choice != input$variable_subgroup) {
    if(gt_stat_count() == "stat_") {
    filtered_data_3() %>% 
      dplyr::select(input$variable_choice, input$variable_subgroup) %>% 
      tbl_cross(
        row = input$variable_choice,
        col = input$variable_subgroup,
        percent = "cell",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice) ~ x,
        digits = c(0, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ paste(y)) %>% 
      modify_header(label = "**Variable**") %>%
      modify_caption("Subgroup Summary Table") %>% 
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
      add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
      as_gt()
      
    } else {
      filtered_data_3() %>% 
        dplyr::select(input$variable_choice, input$variable_subgroup) %>% 
        tbl_cross(
          row = input$variable_choice,
          col = input$variable_subgroup,
          percent = "cell",
          missing_text = "Missing Observations",
          label = !!sym(input$variable_choice) ~ x,
          digits = c(0, 2)) %>% 
        modify_spanning_header(all_stat_cols() ~ paste(y)) %>% 
        modify_header(label = "**Variable**") %>%
        modify_caption("Subgroup Summary Table") %>% 
        modify_column_hide(columns = all_of(gt_stat_count())) %>%
        modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
        add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
        as_gt()
    }
    
  } else {
    filtered_data() %>% 
      select(input$variable_choice) %>% 
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice) ~ x,
        type = all_categorical() ~ "categorical",
        statistic = all_categorical() ~ "{n} ({p}%)",
        digits = all_categorical() ~ c(0, 2)) %>%
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>%
      modify_header(label = "**Variable**") %>%
      modify_caption("Summary Table") %>%
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>%
      as_gt()
  }
  })
  
  my_image_12 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_12(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t12 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_12(), file)
      
    },
  )
  
  output$summary_sub5 <- render_gt({
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    my_table_12()
  })
  ########################################################################################
  my_table_13 <- reactive({
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    
  if(input$variable_choice != input$variable_subgroup) {
   if(gt_stat_count() == "stat_") {
    filtered_data_3() %>% 
      select(input$variable_choice, input$variable_subgroup) %>% 
      tbl_cross(
        row = input$variable_choice,
        col = input$variable_subgroup,
        percent = "cell",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice) ~ x,
        digits = c(0, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ paste(y)) %>% 
      modify_header(label = "**Variable**") %>%
      modify_caption("Subgroup Summary Table") %>% 
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
      add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
      as_gt()
     
   } else {
     filtered_data_3() %>% 
       select(input$variable_choice, input$variable_subgroup) %>% 
       tbl_cross(
         row = input$variable_choice,
         col = input$variable_subgroup,
         percent = "cell",
         missing_text = "Missing Observations",
         label = !!sym(input$variable_choice) ~ x,
         digits = c(0, 2)) %>% 
       modify_spanning_header(all_stat_cols() ~ paste(y)) %>% 
       modify_header(label = "**Variable**") %>%
       modify_caption("Subgroup Summary Table") %>% 
       modify_column_hide(columns = all_of(gt_stat_count())) %>%
       modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
       add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
       as_gt()
   }
  } else {
    filtered_data() %>% 
      select(input$variable_choice) %>% 
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice) ~ x,
        type = all_categorical() ~ "categorical",
        statistic = all_categorical() ~ "{n} ({p}%)",
        digits = all_categorical() ~ c(0, 2)) %>%
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>%
      modify_header(label = "**Variable**") %>%
      modify_caption("Summary Table") %>%
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>%
      as_gt()
  }
  })
  
  my_image_13 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_13(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t13 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_13(), file)
      
    },
  )
  
  output$summary_sub6 <- render_gt({
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    my_table_13()
  })
  ##################################################################
  my_table_14 <- reactive({
    
    x <- full_labels_react()[full_labels_react()$variable %in% input$variable_choice, "variable_label"]
    y <- full_labels_react()[full_labels_react()$variable %in% input$variable_subgroup, "variable_label"]
    
  if(input$variable_choice != input$variable_subgroup) {
   if(gt_stat_count() == "stat_") {
    filtered_data_3() %>% 
      select(input$variable_choice, input$variable_subgroup) %>% 
      tbl_cross(
        row = input$variable_choice,
        col = input$variable_subgroup,
        percent = "cell",
        label = !!sym(input$variable_choice) ~ x,
        missing_text = "Missing Observations",
        digits = c(0, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ paste(y)) %>% 
      modify_header(label = "**Variable**") %>%
      modify_caption("Subgroup Summary Statistics") %>% 
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
      add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
      as_gt()
     
   } else {
     filtered_data_3() %>% 
       select(input$variable_choice, input$variable_subgroup) %>% 
       tbl_cross(
         row = input$variable_choice,
         col = input$variable_subgroup,
         percent = "cell",
         missing_text = "Missing Observations",
         label = !!sym(input$variable_choice) ~ x,
         digits = c(0, 2)) %>% 
       modify_spanning_header(all_stat_cols() ~ paste(y)) %>% 
       modify_header(label = "**Variable**") %>%
       modify_caption("Subgroup Summary Statistics") %>% 
       modify_column_hide(columns = all_of(gt_stat_count())) %>%
       modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>% 
       add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
       as_gt()
   }
    
  } else {
    filtered_data() %>% 
      select(input$variable_choice) %>% 
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice) ~ x,
        type = all_categorical() ~ "categorical",
        statistic = all_categorical() ~ "{n} ({p}%)",
        digits = all_categorical() ~ c(0, 2)) %>%
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>%
      modify_header(label = "**Variable**") %>%
      modify_caption("Summary Table") %>%
      modify_footnote(all_stat_cols() ~ paste(cohort_label(), " | ", filter_label(), "|", dataset_subtitle())) %>%
      as_gt()
  }
  })
  
  my_image_14 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_14(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t14 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_14(), file)
      
    },
  )
  
  output$summary_sub7 <- render_gt({
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    my_table_14()
  })
  ##############################################################

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#############2 VARIABLE VISUALIZATION##############
mapp_dataset_2 <- reactive({
  if(input$dataset_2 == "MAPP I Baseline") {
      mapp_bl_2
    }
  else if(input$dataset_2 == "MAPP II Baseline") {
      mapp_bl_2_II
  }
  else if(input$dataset_2 == "MAPP I Longitudinal") {
    mapp_long_raw
  }
  else {
    mapp_long_raw_II
  }
  })

dataset_subtitle_II <- reactive({
  if(input$dataset_2 == "MAPP I Baseline") {
    dataset_choice[1]
  }
  else if(input$dataset_2 == "MAPP II Baseline") {
    dataset_choice[2]
  }
  else if(input$dataset_2 == "MAPP I Longitudinal") {
    paste0(dataset_choice[3], " Visit ", input$visit_select_2)
  }
  else {
    paste0(dataset_choice[4], " Visit ", input$visit_select_2)
  }
})

variable_assignments_3_react_II <- reactive({
  if(input$dataset_2 == "MAPP I Baseline") {
    variable_assignments_3
  }
  else if(input$dataset_2 == "MAPP II Baseline") {
    variable_assignments_3_II
  }
  else if(input$dataset_2 == "MAPP I Longitudinal") {
    variable_assignments_long_3 %>% 
      filter(variable != "vnum")
  }
  else {
    variable_assignments_long_3_II %>% 
      filter(variable != "vnum")
  }
})

variable_assignments_4_react_II <- reactive({
  if(input$dataset_2 == "MAPP I Baseline") {
    variable_assignments_4
  }
  else if(input$dataset_2 == "MAPP II Baseline") {
    variable_assignments_4_II
  }
  else if(input$dataset_2 == "MAPP I Longitudinal") {
    variable_assignments_long_4
  }
  else {
    variable_assignments_long_4_II
  }
})

variable_assignments_5_react_II <- reactive({
  if(input$dataset_2 == "MAPP I Baseline") {
    variable_assignments_5
  }
  else if(input$dataset_2 == "MAPP II Baseline") {
    variable_assignments_5_II
  }
  else if(input$dataset_2 == "MAPP I Longitudinal") {
    variable_assignments_long_5 %>% 
      filter(variable != "vnum")
  }
  else {
    variable_assignments_long_5_II %>% 
      filter(variable != "vnum")
  }
})

mapp_bl_combined_react_II <- reactive({
  if(input$dataset_2 == "MAPP I Baseline") {
    mapp_bl_combined
  }
  else if(input$dataset_2 == "MAPP II Baseline") {
    mapp_bl_combined_II
  }
  else if(input$dataset_2 == "MAPP I Longitudinal") {
    mapp_long_combined
  }
  else {
    mapp_long_combined_II
  }
})

mapp_bl_labels_react_II <- reactive({
  if(input$dataset_2 == "MAPP I Baseline") {
    mapp_bl_labels
  }
  else if(input$dataset_2 == "MAPP II Baseline") {
    mapp_bl_labels_II
  }
  else if(input$dataset_2 == "MAPP I Longitudinal") {
    mapp_long_labels
  }
  else {
    mapp_long_labels_II
  }
})

full_labels_react_II <- reactive({
  if(input$dataset_2 == "MAPP I Baseline") {
    full_labels
  }
  else if(input$dataset_2 == "MAPP II Baseline") {
    full_labels_II
  }
  else if(input$dataset_2 == "MAPP I Longitudinal") {
    full_labels_long
  }
  else {
    full_labels_long_II
  }
})

new_data_cohort_II <- reactive({
  if(input$dataset_2 == "MAPP I Baseline") {
    mapp_bl_cohort$cohorttype2
  }
  else if(input$dataset_2 == "MAPP II Baseline") {
    mapp_bl_cohort_II$cohorttype2
  }
  else if(input$dataset_2 == "MAPP I Longitudinal") {
    mapp_long_cohort$cohorttype2
  }
  else {
    mapp_long_cohort_II$cohorttype2
  }
})

new_data_primary_grouping_II <- reactive({
  if(input$dataset_2 == "MAPP I Baseline") {
    mapp_bl_primary$variable_selection
  }
  else if(input$dataset_2 == "MAPP II Baseline") {
    mapp_bl_primary_II$variable_selection
  }
  else if(input$dataset_2 == "MAPP I Longitudinal") {
    mapp_long_primary$variable_selection
  }
  else {
    mapp_long_primary_II$variable_selection
  }
})

visit_select_long_2 <- reactive({
  if(input$dataset_2 == "MAPP I Longitudinal") {
    mapp_long_raw %>% 
      select(vnum) %>% 
      distinct() %>% 
      arrange(vnum)
  } 
  else if(input$dataset_2 == "MAPP II Longitudinal") {
    mapp_long_raw_II %>% 
      select(vnum) %>% 
      distinct() %>% 
      arrange(vnum)
  }
  else {
    mapp_long_raw %>% 
      select(vnum) %>% 
      distinct() %>% 
      arrange(vnum)
  }
})

observeEvent(input$dataset_2,
             {
               updateCheckboxGroupInput(session, input = "cohort_type_2", choices = new_data_cohort_II(), selected = "1 - UCPPS")
             })

observeEvent(input$dataset_2,
             {
               updateSelectInput(session, input = "visit_select_2", choices = visit_select_long_2())
             })

observeEvent(input$dataset_2,
             {
               updateSelectInput(session, input = "variable_group_2x", choices = new_data_primary_grouping_II(), selected = "Study Feature")
             })
  
  observeEvent(input$variable_group_2x,
               {
                 updateSelectInput(session, input = "variable_choice_2x",
                                   choices = sort(variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable_selection %in% input$variable_group_2x,
                                                                         "variable", drop = TRUE]))
               })
  
  observeEvent(input$dataset_2,
               {
                 updateSelectInput(session, input = "variable_choice_2x",
                                   choices = sort(variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable_selection %in% input$variable_group_2x,
                                                                         "variable", drop = TRUE]))
               })
  
  output$variable_name_2x <- renderText({
    variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_2x, "label", drop = TRUE]
  })
  
  output$analysis_type_2x <- renderText({
    variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_2x, "ANALYSIS_TYPE_2", drop = TRUE]
  })
  
  output$x_slider_2 <- renderUI({
    
    w <- mapp_dataset_2() %>% 
      pull(input$variable_choice_2x)
    
    max_v <- round(max(w, na.rm = TRUE))
    min_v <- floor(min(w, na.rm = TRUE))
    
    sliderInput(
      inputId = "x_slider_2",
      label = paste0("Filter Observations by ", input$variable_choice_2x),
      min = min_v,
      max = max_v,
      value = c(min_v,max_v),
      step = 1,
      ticks = TRUE,
      dragRange = TRUE
    )
  })
  
  output$numeric_x <- renderUI({
    
    w <- mapp_dataset_2() %>% 
      pull(input$variable_choice_2x)
    
    f <- median(w, na.rm = TRUE)
    g <- floor(min(w, na.rm = TRUE))
    h <- round(max(w, na.rm = TRUE))
    
    numericInput(
      inputId = "numeric_x",
      label = paste("X Reference Line Between", g, "-", h),
      value = f
    )
  })
  
  output$numeric_interval_x <- renderUI({
    numericInput(
      inputId = "numeric_interval_x",
      label = paste("X-Axis Intervals 1-10"),
      value = 2,
      min = 1,
      max = 10
    )
  })
  
  output$numeric_interval_y <- renderUI({
    numericInput(
      inputId = "numeric_interval_y",
      label = paste("Y-Axis Intervals 1-10"),
      value = 2,
      min = 1,
      max = 10
    )
  })
  
  iv <- InputValidator$new()
  iv$add_rule("numeric_interval_x", sv_between(1, 10))
  iv$enable()
  
  iv <- InputValidator$new()
  iv$add_rule("numeric_interval_y", sv_between(1, 10))
  iv$enable()
  
  observeEvent(input$dataset_2,
               {
                 updateSelectInput(session, input = "variable_group_2y", choices = new_data_primary_grouping_II(), selected = "Study Feature")
               })
  
  observeEvent(input$variable_group_2y,
               {
                 updateSelectInput(session, input = "variable_choice_2y",
                                   choices = sort(variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable_selection %in% input$variable_group_2y,
                                                                         "variable", drop = TRUE]))
               })
  
  observeEvent(input$dataset_2,
               {
                 updateSelectInput(session, input = "variable_choice_2y",
                                   choices = sort(variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable_selection %in% input$variable_group_2y,
                                                                         "variable", drop = TRUE]))
               })
  
  output$variable_name_2y <- renderText({
    variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_2y, "label", drop = TRUE]
  })
  
  output$analysis_type_2y <- renderText({
    variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_2y, "ANALYSIS_TYPE_2", drop = TRUE]
  })
  
  output$y_slider_2 <- renderUI({
    
    w <- mapp_dataset_2() %>% 
      pull(input$variable_choice_2y)
    
    max_v <- round(max(w, na.rm = TRUE))
    min_v <- floor(min(w, na.rm = TRUE))
    
    sliderInput(
      inputId = "y_slider_2",
      label = paste0("Filter Observations by ", input$variable_choice_2y),
      min = min_v,
      max = max_v,
      value = c(min_v,max_v),
      step = 1,
      ticks = TRUE,
      dragRange = TRUE
    )
  })
  
  output$numeric_y <- renderUI({
    
    w <- mapp_dataset_2() %>% 
      pull(input$variable_choice_2y)
    
    f <- median(w, na.rm = TRUE)
    g <- floor(min(w, na.rm = TRUE))
    h <- round(max(w, na.rm = TRUE))
    
    numericInput(
      inputId = "numeric_y",
      label = paste("Y Reference Line Between", g, "-", h),
      value = f
    )
  })
  
  new_data_2 <- reactive({
    mapp_bl_combined_react_II() %>%
      gather(columnNames, values) %>% 
      group_by(columnNames) %>% 
      distinct(.keep_all = TRUE) %>% 
      drop_na()
  })
  
  observeEvent(input$dataset_2,
               {
                 updateSelectInput(session, input = "variable_group_fill", choices = new_data_primary_grouping_II(), selected = "Demographics/Anthropometrics")
               })
  
  observeEvent(input$variable_group_fill,
               {
                 updateSelectInput(session, input = "variable_choice_fill",
                                   choices = sort(variable_assignments_4_react_II()[variable_assignments_4_react_II()$variable_selection %in% input$variable_group_fill,
                                                                         "variable", drop = TRUE]))
               })
  
  observeEvent(input$dataset_2,
               {
                 updateSelectInput(session, input = "variable_choice_fill",
                                   choices = sort(variable_assignments_4_react_II()[variable_assignments_4_react_II()$variable_selection %in% input$variable_group_fill,
                                                                         "variable", drop = TRUE]))
               })
  
  observeEvent(input$variable_choice_fill,
               {
                 bb_2 <- new_data_2()
                 
                 updateCheckboxGroupInput(session, input = "fill_filter_2", choices = sort(bb_2[bb_2$columnNames %in% input$variable_choice_fill, "values", drop = TRUE]), 
                                          selected = bb_2[bb_2$columnNames %in% input$variable_choice_fill, "values", drop = TRUE])
               })
  
  observeEvent(input$continuous_fill, 
               {
                 bb_2 <- new_data_2()
                 
                 updateCheckboxGroupInput(session, input = "fill_filter_2", choices = sort(bb_2[bb_2$columnNames %in% input$variable_choice_fill, "values", drop = TRUE]), 
                                          selected = bb_2[bb_2$columnNames %in% input$variable_choice_fill, "values", drop = TRUE])
               })
  
  ####################################
  observeEvent(input$dataset_2,
               {
                 updateSelectInput(session, input = "variable_filter_group_2", choices = new_data_primary_grouping_II(), selected = "Demographics/Anthropometrics")
               })
  
  observeEvent(input$variable_filter_group_2,
               {
                 updateSelectInput(session, input = "variable_filter_choice_2",
                                   choices = sort(variable_assignments_4_react_II()[variable_assignments_4_react_II()$variable_selection %in% input$variable_filter_group_2, 
                                                                          "variable", drop = TRUE]))
               })
  
  observeEvent(input$dataset_2,
               {
                 updateSelectInput(session, input = "variable_filter_choice_2",
                                   choices = sort(variable_assignments_4_react_II()[variable_assignments_4_react_II()$variable_selection %in% input$variable_filter_group_2, 
                                                                          "variable", drop = TRUE]))
               })
  
  
  observeEvent(input$variable_filter_choice_2,
               {
                 bb_2 <- new_data_2()
                 
                 updateCheckboxGroupInput(session, input = "checkbox_filter_2", choices = sort(bb_2[bb_2$columnNames %in% input$variable_filter_choice_2, "values", drop = TRUE]), 
                                          selected = bb_2[bb_2$columnNames %in% input$variable_filter_choice_2, "values", drop = TRUE])
               })
  
  observeEvent(input$checking_filter_2, 
               {
                bb_2 <- new_data_2()
    
                updateCheckboxGroupInput(session, input = "checkbox_filter_2", choices = sort(bb_2[bb_2$columnNames %in% input$variable_filter_choice_2, "values", drop = TRUE]), 
                                         selected = bb_2[bb_2$columnNames %in% input$variable_filter_choice_2, "values", drop = TRUE])
  })
  
#############################
  
  observeEvent(input$variable_group_2x, {
    updateCheckboxInput(session, "continuous_fill", value = FALSE)
  })
  
  observeEvent(input$variable_choice_2x, {
    updateCheckboxInput(session, "continuous_fill", value = FALSE)
  })
  
  observeEvent(input$variable_group_2y, {
    updateCheckboxInput(session, "continuous_fill", value = FALSE)
  })
  
  observeEvent(input$variable_choice_2y, {
    updateCheckboxInput(session, "continuous_fill", value = FALSE)
  })
  
  observeEvent(input$dataset_2, {
    updateCheckboxInput(session, "continuous_fill", value = FALSE)
  })
  
  download_type_3 <- reactive(
    if(input$download_type_2 == ".pdf") {
      ".pdf"
    }
    else {
      ".png"
    }
  )
  
  download_type_4 <- reactive(
    if(input$download_type_2 == ".pdf") {
      "pdf"
    }
    else {
      "png"
    }
  )
  
  output$download_2 <- downloadHandler(
    filename = function() {
      paste("plot-", Sys.Date(), download_type_3(), sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = download_type_4(), dpi = 600, width = 12, height = 10, units = "in")
    }
  )

  
  #2-Variable Visualization Datasets
  gg_2 <- reactive({
    mapp_bl_combined_react_II() %>% 
      pull(input$variable_filter_choice_2)
  })
  
  ff_2 <- reactive({
    mapp_bl_combined_react_II() %>% 
      pull(input$variable_choice_fill)
  })
  
  cohort_label_2 <- reactive({
    req(input$cohort_type_2)
    
    if(input$captions_2 == "Yes") {
      paste("Cohorts: ", paste(unique(input$cohort_type_2[input$cohort_type_2 %in% mapp_bl_combined_react_II()$cohorttype2]), collapse = ", "))
    }
    else {
      return("")
    }
  })
  
  filter_label_2 <- reactive({
    
    req(input$variable_filter_choice_2)
    req(input$checkbox_filter_2)
    
    v <- paste(full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_filter_choice_2, "variable_label"], ":")
    
    if(input$checking_filter_2 == 1 & input$captions_2 == "Yes") {
      paste(v, paste(unique(input$checkbox_filter_2[input$checkbox_filter_2 %in% gg_2()]), collapse = ", "))
    }
    else {
      return("")
    }
  })
  
  full_vnum_2 <- reactive({
    if(input$dataset_2 == "MAPP I Longitudinal") {
      mapp_long_raw %>% 
        pull(vnum)
    }
    else {
      mapp_long_raw_II %>% 
        pull(vnum)
    }
  })
  
  #Used for Continuous Variables
  filtered_data_5 <- reactive({
    if(input$missing_data_2 == "Yes") {
      mapp_bl_combined_react_II() %>%
        filter(if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & ff_2() %in% input$fill_filter_2
               else cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & ff_2() %in% input$fill_filter_2 & full_vnum_2() %in% input$visit_select_2) %>% 
        select(input$variable_choice_2x, input$variable_choice_2y, input$variable_choice_fill) %>% 
        drop_na(input$variable_choice_2x, input$variable_choice_2y)
    }
    else{
      mapp_bl_combined_react_II() %>%
        filter(if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & ff_2() %in% input$fill_filter_2
               else cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & ff_2() %in% input$fill_filter_2 & full_vnum_2() %in% input$visit_select_2) %>% 
        select(input$variable_choice_2x, input$variable_choice_2y, input$variable_choice_fill) %>% 
        drop_na()
    }
  })
  
  #Used for Categorical Variables
  filtered_data_6 <- reactive({
    if(input$missing_data_2 == "Yes") {
      mapp_bl_combined_react_II() %>%
        filter(if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2
               else cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & full_vnum_2() %in% input$visit_select_2)
    }
    else {
      mapp_bl_combined_react_II() %>%
        filter(if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2
               else cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & full_vnum_2() %in% input$visit_select_2) %>% 
        dplyr::select(input$variable_choice_2x, input$variable_choice_2y) %>% 
        drop_na()
    }
  })
  
  #Used for Combined
  filtered_data_7 <- reactive({
    req(input$variable_choice_2x)
    req(input$variable_choice_2x)
    
    if(input$missing_data_2 == "Yes") {
      if(input$variable_choice_2x %in% variable_assignments_5$variable) {
          mapp_bl_combined_react_II() %>% 
            filter(if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2
                   else cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & full_vnum_2() %in% input$visit_select_2) %>% 
            drop_na(input$variable_choice_2x) %>% 
            mutate_if(is.factor, fct_explicit_na, na_level = "Missing Observations")
      }
      else{
        mapp_bl_combined_react_II() %>% 
          filter(if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2
                 else cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & full_vnum_2() %in% input$visit_select_2) %>% 
          drop_na(input$variable_choice_2y) %>% 
          mutate_if(is.factor, fct_explicit_na, na_level = "Missing Observations")
      }
    }
    
    else {
      mapp_bl_combined_react_II() %>% 
        filter(if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2
               else cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & full_vnum_2() %in% input$visit_select_2) %>% 
        dplyr::select(input$variable_choice_2x, input$variable_choice_2y) %>% 
        drop_na()
    }
  })
  
  #Used for Row Counts Combined
  filtered_data_7a <- reactive({
    
    req(input$checkbox_filter_2)
    req(input$cohort_type_2)

 if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") {
  if(input$variable_choice_2x %in% variable_assignments_5_react_II()$variable) {
    
    req(input$variable_choice_2x)
    req(input$variable_choice_2y)
    req(input$x_slider_2)
    
    if(input$missing_data_2 == "Yes") {
      if(input$variable_choice_2x %in% variable_assignments_5$variable) {
        mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) &
                                    mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) &
                  (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2),] %>% 
          drop_na(input$variable_choice_2x)
      }
      else{
        mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) &
                                    mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) &
                  (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2),] %>% 
          drop_na(input$variable_choice_2y)
      }
    }
    else{
      mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                    mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) &
                  (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2),] %>% 
        dplyr::select(input$variable_choice_2x, input$variable_choice_2y) %>% 
        drop_na()
      
    }
  }
    else {
      
      req(input$variable_choice_2x)
      req(input$variable_choice_2y)
      req(input$y_slider_2)
      
      if(input$missing_data_2 == "Yes") {
        mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & 
                                      mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                    (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2),]
      }
      else{
        mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & 
                                      mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                    (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2),] %>% 
          dplyr::select(input$variable_choice_2x, input$variable_choice_2y) %>% 
          drop_na()
        
      }
    }
} 
 else {
   if(input$variable_choice_2x %in% variable_assignments_5_react_II()$variable) {
     
     req(input$variable_choice_2x)
     req(input$variable_choice_2y)
     req(input$x_slider_2)
     
     if(input$missing_data_2 == "Yes") {
       if(input$variable_choice_2x %in% variable_assignments_5$variable) {
         mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) &
                                       mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) &
                                       (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2) & (full_vnum_2() %in% input$visit_select_2),] %>% 
           drop_na(input$variable_choice_2x)
       }
       else{
         mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) &
                                       mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) &
                                       (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2) & (full_vnum_2() %in% input$visit_select_2),] %>% 
           drop_na(input$variable_choice_2y)
       }
     }
     else{
       mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                     mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) &
                                     (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2) & (full_vnum_2() %in% input$visit_select_2),] %>% 
         dplyr::select(input$variable_choice_2x, input$variable_choice_2y) %>% 
         drop_na()
       
     }
   }
   else {
     
     req(input$variable_choice_2x)
     req(input$variable_choice_2y)
     req(input$y_slider_2)
     
     if(input$missing_data_2 == "Yes") {
       mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & 
                                     mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                     (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2) & (full_vnum_2() %in% input$visit_select_2),]
     }
     else{
       mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & 
                                     mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                     (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2) & (full_vnum_2() %in% input$visit_select_2),] %>% 
         dplyr::select(input$variable_choice_2x, input$variable_choice_2y) %>% 
         drop_na()
       
     }
   }
 }
  })
  
  #Used for Row Counts
  filtered_data_5a <- reactive({
    
    req(input$variable_choice_2x)
    req(input$variable_choice_2y)
    req(input$y_slider_2)
    req(input$x_slider_2)
  
   if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") {
    if(input$missing_data_2 == "Yes") {
      mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                    mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                  (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2),]
    }
    else{
      mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                    mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                  (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2),]
    }
   }
    else {
      if(input$missing_data_2 == "Yes") {
        mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                      mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                      (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2) & (full_vnum_2() %in% input$visit_select_2),]
      }
      else{
        mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                      mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                      (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2) & (full_vnum_2() %in% input$visit_select_2),]
      }
    }
  })
  
  filtered_data_5x <- reactive({
    
    req(input$variable_choice_2x)
    req(input$x_slider_2)
    
    if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") {
      if(input$missing_data_2 == "Yes") {
        mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                      (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2),]
      }
      else{
        mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                     (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2),]
      }
    }
    else {
      if(input$missing_data_2 == "Yes") {
        mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                      (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2) & (full_vnum_2() %in% input$visit_select_2),]
      }
      else{
        mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                      (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2) & (full_vnum_2() %in% input$visit_select_2),]
      }
    }
  })
  
  filtered_data_5y <- reactive({
    
    req(input$variable_choice_2y)
    req(input$y_slider_2)
    
    if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") {
      if(input$missing_data_2 == "Yes") {
        mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) & 
                                      (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2),]
      }
      else{
        mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                      (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2),]
      }
    }
    else {
      if(input$missing_data_2 == "Yes") {
        mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                      (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2) & (full_vnum_2() %in% input$visit_select_2),]
      }
      else{
        mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                      (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type_2) & (gg_2() %in% input$checkbox_filter_2) & (full_vnum_2() %in% input$visit_select_2),]
      }
    }
  })
  
  #Point Plot
  output$graph_1 <- renderPlot({
    
    validate(need(nrow(filtered_data_5()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_5a()) > 0, message = FALSE))
    
    req(input$variable_choice_2x)
    req(input$variable_choice_2y)
    req(input$x_slider_2)
    req(input$y_slider_2)
    req(input$cohort_type_2)
    req(input$checkbox_filter_2)

    w <- mapp_dataset_2() %>% 
      select(input$variable_choice_2x)
    
    z <- mapp_dataset_2() %>% 
      select(input$variable_choice_2y)
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
    
    a <- min(input$x_slider_2) - 1
    b <- max(input$x_slider_2)
    c <- min(input$x_slider_2)
    d <- max(input$x_slider_2) + 1
    
    e <- min(input$y_slider_2) - 1
    f <- max(input$y_slider_2)
    g <- min(input$y_slider_2)
    h <- max(input$y_slider_2) + 1
    
    i <- input$numeric_x
    j <- input$numeric_y
    
    xx <- mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                   mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                  (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type),] %>% 
      dplyr::select(input$variable_choice_2x)
    
    xx <- as.vector(xx[,1])
    
    yy <- mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                   mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                  (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type),] %>% 
      dplyr::select(input$variable_choice_2y)
    
    yy <- as.vector(yy[,1])
    
    model1 <- summary(lm(yy ~ xx))
    model_coef <- model1$coefficients
    cor_value <- round(sqrt(model1$r.squared), 2)
    slope <- round(model_coef[2,1], 2)
    intercept <- round(model_coef[1,1], 2)
    
    annotation <- data.frame(
      x = c(c - 1, c - 1, c - 1),
      y = c(f + 1, f + 1, f + 1),
      label = c(paste0("r = ", cor_value), paste0("slope = ", slope), paste0("intercept = ", intercept)))
    
    if(input$continuous_fill == FALSE) {
     if(input$regression_line == FALSE) {
      if(input$intercept_button == TRUE & input$lowess_line == TRUE) {
        if(input$color_button == TRUE) {
          plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y)) +
            geom_rect(aes(xmin = i, xmax = Inf, ymin = j, ymax = Inf), fill = "#F19086") + #TR
            geom_rect(aes(xmin = c, xmax = i, ymin = g, ymax = j), fill = "#bdf78a") + #BL
            geom_rect(aes(xmin = i, xmax = Inf, ymin = g, ymax = j), fill = "#BEB3ED") + #BR
            geom_rect(aes(xmin = c, xmax = i, ymin = j, ymax = Inf), fill = "#F8B781") + #TL
            geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
            geom_hline(yintercept = g, linetype = "dashed", color = "black") +
            geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
            geom_vline(xintercept = c, linetype = "dashed", color = "black") +
            geom_count(show.legend = TRUE) +
            theme_bw() +
            geom_smooth(se = FALSE, color = "black") +
            scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
            scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
            labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                 caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
            theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                  axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                  legend.title = element_text(size = 16), 
                  plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
        }
        
        else {
          plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y)) +
            geom_count(show.legend = TRUE) +
            theme_bw() +
            geom_smooth(se = FALSE) +
            scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
            scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
            labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                 caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
            theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                  axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                  legend.title = element_text(size = 16), 
                  plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
            geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
            geom_hline(yintercept = g, linetype = "dashed", color = "black") +
            geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
            geom_vline(xintercept = c, linetype = "dashed", color = "black")
        }
        
        plot_int <- plot_color
        
      }
      
      else if(input$intercept_button == TRUE & input$lowess_line == FALSE) {
        if(input$color_button == TRUE){
          plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y)) +
            geom_rect(aes(xmin = i, xmax = Inf, ymin = j, ymax = Inf), fill = "#F19086") + #TR
            geom_rect(aes(xmin = c, xmax = i, ymin = g, ymax = j), fill = "#bdf78a") + #BL
            geom_rect(aes(xmin = i, xmax = Inf, ymin = g, ymax = j), fill = "#BEB3ED") + #BR
            geom_rect(aes(xmin = c, xmax = i, ymin = j, ymax = Inf), fill = "#F8B781") + #TL
            geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
            geom_hline(yintercept = g, linetype = "dashed", color = "black") +
            geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
            geom_vline(xintercept = c, linetype = "dashed", color = "black") +
            geom_count(show.legend = TRUE) +
            theme_bw() +
            scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
            scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
            labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                 caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
            theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                  axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                  legend.title = element_text(size = 16), 
                  plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
        }
        
        else{
          plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y)) +
            geom_count(show.legend = TRUE) +
            theme_bw() +
            scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
            scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
            labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                 caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
            theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                  axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                  legend.title = element_text(size = 16), 
                  plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
            geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
            geom_hline(yintercept = g, linetype = "dashed", color = "black") +
            geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
            geom_vline(xintercept = c, linetype = "dashed", color = "black")
        }
        plot_int <- plot_color
      }
      
      else if(input$intercept_button == FALSE & input$lowess_line == TRUE) {
        plot_int <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y)) +
          geom_count(show.legend = TRUE) +
          theme_bw() +
          geom_smooth(se = FALSE) +
          scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
          scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
          labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
               caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
          theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                legend.title = element_text(size = 16), 
                plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
      }
      
      else {
        plot_int <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y)) +
          geom_count(show.legend = TRUE) +
          theme_bw() +
          scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
          scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
          labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
               caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
          theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                legend.title = element_text(size = 16), 
                plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
      }
      
    }
    
    else {
      if(input$intercept_button == TRUE & input$lowess_line == TRUE) {
        if(input$color_button == TRUE) {
          plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y)) +
            geom_rect(aes(xmin = i, xmax = Inf, ymin = j, ymax = Inf), fill = "#F19086") + #TR
            geom_rect(aes(xmin = c, xmax = i, ymin = g, ymax = j), fill = "#bdf78a") + #BL
            geom_rect(aes(xmin = i, xmax = Inf, ymin = g, ymax = j), fill = "#BEB3ED") + #BR
            geom_rect(aes(xmin = c, xmax = i, ymin = j, ymax = Inf), fill = "#F8B781") + #TL
            geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
            geom_hline(yintercept = g, linetype = "dashed", color = "black") +
            geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
            geom_vline(xintercept = c, linetype = "dashed", color = "black") +
            geom_count(show.legend = TRUE) +
            geom_smooth(method = "lm", se = FALSE, color = "black") +
            theme_bw() +
            geom_smooth(se = FALSE, color = "black") +
            scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
            scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
            labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                 caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
            theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                  axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                  legend.title = element_text(size = 16), 
                  plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
            geom_label_repel(data = annotation, aes(x = x, y = y, label = label), color = "black", size = 4.2, angle = 0, fontface = "bold", segment.color = 'transparent')
        }
        
        else{
          plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y)) +
            geom_count(show.legend = TRUE) +
            geom_smooth(method = "lm", se = FALSE, color = "black") +
            theme_bw() +
            geom_smooth(se = FALSE) +
            scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
            scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
            labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                 caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
            theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                  axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                  legend.title = element_text(size = 16), 
                  plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
            geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
            geom_hline(yintercept = g, linetype = "dashed", color = "black") +
            geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
            geom_vline(xintercept = c, linetype = "dashed", color = "black") +
            geom_label_repel(data = annotation, aes(x = x, y = y, label = label), color = "black", size = 4.2, angle = 0, fontface = "bold", segment.color = 'transparent')
        }
        plot_int <- plot_color
      }
      
      else if(input$intercept_button == TRUE & input$lowess_line == FALSE) {
        if(input$color_button == TRUE){
          plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y)) +
            geom_rect(aes(xmin = i, xmax = Inf, ymin = j, ymax = Inf), fill = "#F19086") + #TR
            geom_rect(aes(xmin = c, xmax = i, ymin = g, ymax = j), fill = "#bdf78a") + #BL
            geom_rect(aes(xmin = i, xmax = Inf, ymin = g, ymax = j), fill = "#BEB3ED") + #BR
            geom_rect(aes(xmin = c, xmax = i, ymin = j, ymax = Inf), fill = "#F8B781") + #TL
            geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
            geom_hline(yintercept = g, linetype = "dashed", color = "black") +
            geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
            geom_vline(xintercept = c, linetype = "dashed", color = "black") +
            geom_count(show.legend = TRUE) +
            geom_smooth(method = "lm", se = FALSE, color = "black") +
            theme_bw() +
            scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
            scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
            labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                 caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
            theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                  axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                  legend.title = element_text(size = 16), 
                  plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
            geom_label_repel(data = annotation, aes(x = x, y = y, label = label), color = "black", size = 4.2, angle = 0, fontface = "bold", segment.color = 'transparent')
        }
        
        else {
          plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y)) +
            geom_count(show.legend = TRUE) +
            geom_smooth(method = "lm", se = FALSE, color = "black") +
            theme_bw() +
            scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
            scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
            labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                 caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
            theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                  axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                  legend.title = element_text(size = 16), 
                  plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
            geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
            geom_hline(yintercept = g, linetype = "dashed", color = "black") +
            geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
            geom_vline(xintercept = c, linetype = "dashed", color = "black") +
            geom_label_repel(data = annotation, aes(x = x, y = y, label = label), color = "black", size = 4.2, angle = 0, fontface = "bold", segment.color = 'transparent')
        }
        plot_int <- plot_color
      }
      
      else if(input$intercept_button == FALSE & input$lowess_line == TRUE) {
        plot_int <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y)) +
          geom_count(show.legend = TRUE) +
          geom_smooth(method = "lm", se = FALSE, color = "black") +
          theme_bw() +
          geom_smooth(se = FALSE) +
          scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
          scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
          labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
               caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
          theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                legend.title = element_text(size = 16), 
                plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
          geom_label_repel(data = annotation, aes(x = x, y = y, label = label), color = "black", size = 4.2, angle = 0, fontface = "bold", segment.color = 'transparent')
      }
      
      else {
        plot_int <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y)) +
          geom_count(show.legend = TRUE) +
          geom_smooth(method = "lm", se = FALSE, color = "black") +
          theme_bw() +
          scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
          scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
          labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
               caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
          theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                legend.title = element_text(size = 16), 
                plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
          geom_label_repel(data = annotation, aes(x = x, y = y, label = label), color = "black", size = 4.2, angle = 0, fontface = "bold", segment.color = 'transparent')
      }
      
    }
    
    plot_int
    
    }
    
    else {
      if(input$regression_line == FALSE) {
        if(input$intercept_button == TRUE & input$lowess_line == TRUE) {
          if(input$color_button == TRUE) {
            plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y, color = input$variable_choice_fill)) +
              geom_rect(aes(xmin = i, xmax = Inf, ymin = j, ymax = Inf), fill = "#F19086") + #TR
              geom_rect(aes(xmin = c, xmax = i, ymin = g, ymax = j), fill = "#bdf78a") + #BL
              geom_rect(aes(xmin = i, xmax = Inf, ymin = g, ymax = j), fill = "#BEB3ED") + #BR
              geom_rect(aes(xmin = c, xmax = i, ymin = j, ymax = Inf), fill = "#F8B781") + #TL
              geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
              geom_hline(yintercept = g, linetype = "dashed", color = "black") +
              geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
              geom_vline(xintercept = c, linetype = "dashed", color = "black") +
              geom_count(show.legend = TRUE) +
              theme_bw() +
              geom_smooth(se = FALSE, color = "black") +
              scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
              scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
              labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                   caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70)), 
                   color = variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_fill, "label", drop = TRUE]) +
              theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                    axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                    legend.title = element_text(size = 16), 
                    plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
              guides(color = guide_legend(override.aes = list(size = 5)))
          }
          
          else {
            plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y, color = input$variable_choice_fill)) +
              geom_count(show.legend = TRUE) +
              theme_bw() +
              geom_smooth(se = FALSE) +
              scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
              scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
              labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                   caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70)), 
                   color = variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_fill, "label", drop = TRUE]) +
              theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                    axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                    legend.title = element_text(size = 16), 
                    plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
              geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
              geom_hline(yintercept = g, linetype = "dashed", color = "black") +
              geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
              geom_vline(xintercept = c, linetype = "dashed", color = "black") +
              guides(color = guide_legend(override.aes = list(size = 5)))
          }
          
          plot_int <- plot_color
          
        }
        
        else if(input$intercept_button == TRUE & input$lowess_line == FALSE) {
          if(input$color_button == TRUE) {
            plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y, color = input$variable_choice_fill)) +
              geom_rect(aes(xmin = i, xmax = Inf, ymin = j, ymax = Inf), fill = "#F19086") + #TR
              geom_rect(aes(xmin = c, xmax = i, ymin = g, ymax = j), fill = "#bdf78a") + #BL
              geom_rect(aes(xmin = i, xmax = Inf, ymin = g, ymax = j), fill = "#BEB3ED") + #BR
              geom_rect(aes(xmin = c, xmax = i, ymin = j, ymax = Inf), fill = "#F8B781") + #TL
              geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
              geom_hline(yintercept = g, linetype = "dashed", color = "black") +
              geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
              geom_vline(xintercept = c, linetype = "dashed", color = "black") +
              geom_count(show.legend = TRUE) +
              theme_bw() +
              scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
              scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
              labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                   caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70)), 
                   color = variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_fill, "label", drop = TRUE]) +
              theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                    axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                    legend.title = element_text(size = 16), 
                    plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
              guides(color = guide_legend(override.aes = list(size = 5)))
          }
          
          else {
            plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y, color = input$variable_choice_fill)) +
              geom_count(show.legend = TRUE) +
              theme_bw() +
              scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
              scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
              labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                   caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70)), 
                   color = variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_fill, "label", drop = TRUE]) +
              theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                    axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                    legend.title = element_text(size = 16), 
                    plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
              geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
              geom_hline(yintercept = g, linetype = "dashed", color = "black") +
              geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
              geom_vline(xintercept = c, linetype = "dashed", color = "black") +
              guides(color = guide_legend(override.aes = list(size = 5)))
          }
          plot_int <- plot_color
        }
        
        else if(input$intercept_button == FALSE & input$lowess_line == TRUE) {
          plot_int <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y, color = input$variable_choice_fill)) +
            geom_count(show.legend = TRUE) +
            theme_bw() +
            geom_smooth(se = FALSE) +
            scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
            scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
            labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                 caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70)), 
                 color = variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_fill, "label", drop = TRUE]) +
            theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                  axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                  legend.title = element_text(size = 16)) +
            guides(color = guide_legend(override.aes = list(size = 5)))
        }
        
        #, legend.key.size = unit(2, 'cm')
        
        else {
          plot_int <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y, color = input$variable_choice_fill)) +
            geom_count(show.legend = TRUE) +
            theme_bw() +
            scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
            scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
            labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                 caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70)), 
                 color = variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_fill, "label", drop = TRUE]) +
            theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                  axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                  legend.title = element_text(size = 16), 
                  plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
            guides(color = guide_legend(override.aes = list(size = 5)))
        }
        
      }
      
      else {
        if(input$intercept_button == TRUE & input$lowess_line == TRUE) {
          if(input$color_button == TRUE) {
            plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y, color = input$variable_choice_fill)) +
              geom_rect(aes(xmin = i, xmax = Inf, ymin = j, ymax = Inf), fill = "#F19086") + #TR
              geom_rect(aes(xmin = c, xmax = i, ymin = g, ymax = j), fill = "#bdf78a") + #BL
              geom_rect(aes(xmin = i, xmax = Inf, ymin = g, ymax = j), fill = "#BEB3ED") + #BR
              geom_rect(aes(xmin = c, xmax = i, ymin = j, ymax = Inf), fill = "#F8B781") + #TL
              geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
              geom_hline(yintercept = g, linetype = "dashed", color = "black") +
              geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
              geom_vline(xintercept = c, linetype = "dashed", color = "black") +
              geom_smooth(method = "lm", se = FALSE, color = "black") + ##################################################################################################
              theme_bw() +
              geom_smooth(se = FALSE, color = "black") +
              geom_count(show.legend = TRUE) +
              scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
              scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
              labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                   caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70)), 
                   color = variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_fill, "label", drop = TRUE]) +
              theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                    axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                    legend.title = element_text(size = 16), 
                    plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
              geom_label_repel(data = annotation, aes(x = x, y = y, label = label), color = "black", size = 4.2, angle = 0, fontface = "bold", segment.color = 'transparent') +
              guides(color = guide_legend(override.aes = list(size = 5)))
          }
          
          else {
            plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y, color = input$variable_choice_fill)) +
              geom_count(show.legend = TRUE) +
              geom_smooth(method = "lm", se = FALSE, color = "black") +
              theme_bw() +
              geom_smooth(se = FALSE) +
              scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
              scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
              labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                   caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70)), 
                   color = variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_fill, "label", drop = TRUE]) +
              theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                    axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                    legend.title = element_text(size = 16), 
                    plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
              geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
              geom_hline(yintercept = g, linetype = "dashed", color = "black") +
              geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
              geom_vline(xintercept = c, linetype = "dashed", color = "black") +
              geom_label_repel(data = annotation, aes(x = x, y = y, label = label), color = "black", size = 4.2, angle = 0, fontface = "bold", segment.color = 'transparent') +
              guides(color = guide_legend(override.aes = list(size = 5)))
          }
          plot_int <- plot_color
        }
        
        else if(input$intercept_button == TRUE & input$lowess_line == FALSE) {
          if(input$color_button == TRUE) {
            plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y, color = input$variable_choice_fill)) +
              geom_rect(aes(xmin = i, xmax = Inf, ymin = j, ymax = Inf), fill = "#F19086") + #TR
              geom_rect(aes(xmin = c, xmax = i, ymin = g, ymax = j), fill = "#bdf78a") + #BL
              geom_rect(aes(xmin = i, xmax = Inf, ymin = g, ymax = j), fill = "#BEB3ED") + #BR
              geom_rect(aes(xmin = c, xmax = i, ymin = j, ymax = Inf), fill = "#F8B781") + #TL
              geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
              geom_hline(yintercept = g, linetype = "dashed", color = "black") +
              geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
              geom_vline(xintercept = c, linetype = "dashed", color = "black") +
              geom_count(show.legend = TRUE) +
              geom_smooth(method = "lm", se = FALSE, color = "black") +
              theme_bw() +
              scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
              scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
              labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                   caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70)), 
                   color = variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_fill, "label", drop = TRUE]) +
              theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                    axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                    legend.title = element_text(size = 16), 
                    plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
              geom_label_repel(data = annotation, aes(x = x, y = y, label = label), color = "black", size = 4.2, angle = 0, fontface = "bold", segment.color = 'transparent') +
              guides(color = guide_legend(override.aes = list(size = 5)))
          }
          
          else {
            plot_color <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y, color = input$variable_choice_fill)) +
              geom_count(show.legend = TRUE) +
              geom_smooth(method = "lm", se = FALSE, color = "black") +
              theme_bw() +
              scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
              scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
              labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                   caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70)), 
                   color = variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_fill, "label", drop = TRUE]) +
              theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                    axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                    legend.title = element_text(size = 16), 
                    plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
              geom_hline(yintercept = input$numeric_y, linetype = "dashed", color = "black") +
              geom_hline(yintercept = g, linetype = "dashed", color = "black") +
              geom_vline(xintercept = input$numeric_x, linetype = "dashed", color = "black") +
              geom_vline(xintercept = c, linetype = "dashed", color = "black") +
              geom_label_repel(data = annotation, aes(x = x, y = y, label = label), color = "black", size = 4.2, angle = 0, fontface = "bold", segment.color = 'transparent') +
              guides(color = guide_legend(override.aes = list(size = 5)))
          }
          plot_int <- plot_color
        }
        
        else if(input$intercept_button == FALSE & input$lowess_line == TRUE) {
          plot_int <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y, color = input$variable_choice_fill)) +
            geom_count(show.legend = TRUE) +
            geom_smooth(method = "lm", se = FALSE, color = "black") +
            theme_bw() +
            geom_smooth(se = FALSE) +
            scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
            scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
            labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                 caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70)), 
                 color = variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_fill, "label", drop = TRUE]) +
            theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                  axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                  legend.title = element_text(size = 16), 
                  plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
            geom_label_repel(data = annotation, aes(x = x, y = y, label = label), color = "black", size = 4.2, angle = 0, fontface = "bold", segment.color = 'transparent')
        }
        
        else {
          plot_int <- ggplot(filtered_data_5(), aes_string(input$variable_choice_2x, input$variable_choice_2y, color = input$variable_choice_fill)) +
            geom_count(show.legend = TRUE) +
            geom_smooth(method = "lm", se = FALSE, color = "black") +
            theme_bw() +
            scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
            scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
            labs(x = x, y = y, title = str_wrap(paste(x, "vs", y), 85), subtitle = paste("Number of Observations: ", nrow(filtered_data_5()), "|", dataset_subtitle_II()), 
                 caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70)), 
                 color = variable_assignments_3_react_II()[variable_assignments_3_react_II()$variable %in% input$variable_choice_fill, "label", drop = TRUE]) +
            theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
                  axis.title = element_text(size = 18, face = "bold"), legend.position = "right", axis.text = element_text(size = 18), legend.text = element_text(size = 16), 
                  legend.title = element_text(size = 16),  
                  plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
            geom_label_repel(data = annotation, aes(x = x, y = y, label = label), color = "black", size = 4.2, angle = 0, fontface = "bold", segment.color = 'transparent') +
            guides(color = guide_legend(override.aes = list(size = 5)))
        }
        
      }
    }
    
    plot_int
  })
  
  output$graph_2 <- renderPlot({
    
    validate(need(nrow(filtered_data_7()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_7a()) > 0, message = FALSE))
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]

  if(input$variable_choice_2x %in% variable_assignments_5_react_II()$variable) {
    
    req(input$variable_choice_2x)
    req(input$variable_choice_2y)
    req(input$x_slider_2)
    
    a <- min(input$x_slider_2) - 1
    b <- max(input$x_slider_2)
    c <- min(input$x_slider_2)
    d <- max(input$x_slider_2) + 1
    
  q <- ggplot(filtered_data_7(), aes_string(input$variable_choice_2x, color = input$variable_choice_2y, fill = input$variable_choice_2y)) +
    geom_histogram(binwidth = 1, position = "stack", alpha = 0.5) +
    scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_x)) +
    theme_bw() +
    labs(x = "", y = "Count", fill = str_wrap(y, 25), color = str_wrap(y, 25), title = str_wrap(x, 70), 
         subtitle = paste("Number of Observations: ", nrow(filtered_data_7a()), "|", dataset_subtitle_II()), 
         caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16),
          axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = 18), legend.text = element_text(size = 16),
          legend.title = element_text(size = 16), 
          plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  }

  else {
    
    req(input$variable_choice_2x)
    req(input$variable_choice_2y)
    req(input$y_slider_2)
    
    e <- min(input$y_slider_2) - 1
    f <- max(input$y_slider_2)
    g <- min(input$y_slider_2)
    h <- max(input$y_slider_2) + 1
    
  q <- ggplot(filtered_data_7(), aes_string(input$variable_choice_2y, color = input$variable_choice_2x, fill = input$variable_choice_2x)) +
    geom_histogram(binwidth = 1, position = "stack", alpha = 0.5) +
    scale_x_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_y)) +
    theme_bw() +
    labs(x = "", y = "Count", fill = str_wrap(x, 25), color = str_wrap(x, 25), title = str_wrap(y, 85), 
         subtitle = paste("Number of Observations: ", nrow(filtered_data_7a()), "|", dataset_subtitle_II()), 
         caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16),
          axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = 18), legend.text = element_text(size = 16),
          legend.title = element_text(size = 16), 
          plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
  }

  q

  })
  
  output$graph_3 <- renderPlot({
    
    validate(need(nrow(filtered_data_6()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
    
    axis_angle <- input$axis_angle_2
    text_size <- input$text_size_2

    if(input$display_type_2 == "Count") {
    q <- ggplot(filtered_data_6(), aes_string(input$variable_choice_2x, fill = input$variable_choice_2y)) +
      geom_bar(position = position_stack()) +
      theme_bw() +
      labs(x = "", y = input$display_type_2, title = str_wrap(x, 70), 
           subtitle = paste("Number of Observations: ", nrow(filtered_data_6()), "|", dataset_subtitle_II()), fill = str_wrap(y, 25), color = str_wrap(y, 25), 
           caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16),
            axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size),
            axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16), legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
    }

    else {
      q <- ggplot(filtered_data_6(), aes_string(input$variable_choice_2x, fill = input$variable_choice_2y)) +
        geom_bar(aes(y = (..count..)/sum(..count..)), position = position_stack()) +
        theme_bw() +
        scale_y_continuous(limits = c(0, 1)) +
        labs(x = "", y = input$display_type_2, title = str_wrap(x, 70), 
             subtitle = paste("Number of Observations: ", nrow(filtered_data_5a()), "|", dataset_subtitle_II()), fill = str_wrap(y, 25), color = str_wrap(y, 25), 
             caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16),
              axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size),
              axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16),legend.title = element_text(size = 16), 
              plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
    }

    q

  })
  
  output$graph_4 <- renderPlot({
    
    validate(need(nrow(filtered_data_6()) > 0, message = FALSE))
    validate(need(nrow(filtered_data_3()) > 0, message = FALSE))
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
    axis_angle <- input$axis_angle_2
    text_size <- input$text_size_2
    
    if(input$display_type_2 == "Count") {
      q <- ggplot(filtered_data_6(), aes_string(input$variable_choice_2x, fill = input$variable_choice_2y)) +
        geom_bar(position = "dodge") +
        theme_bw() +
        labs(x = "", y = input$display_type_2, title = str_wrap(x, 70), 
             subtitle = paste("Number of Observations: ", nrow(filtered_data_6()), "|", dataset_subtitle_II()), fill = str_wrap(y, 25), color = str_wrap(y, 25), 
             caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16),
              axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size),
              axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16), legend.title = element_text(size = 16), 
              plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
    }
    
    else {
      q <- ggplot(filtered_data_6(), aes_string(input$variable_choice_2x, fill = input$variable_choice_2y)) +
        geom_bar(aes(y = (..count..)/sum(..count..)), position = "dodge") +
        theme_bw() +
        scale_y_continuous(limits = c(0, 1)) +
        labs(x = "", y = input$display_type_2, title = str_wrap(x, 70), 
             subtitle = paste("Number of Observations: ", nrow(filtered_data_6()), "|", dataset_subtitle_II()), fill = str_wrap(y, 25), color = str_wrap(y, 25), 
             caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16),
              axis.title = element_text(size = 16, face = "bold"), legend.position = "bottom", axis.text = element_text(size = text_size),
              axis.text.x = element_text(angle = axis_angle, hjust = 0.5, vjust = 0.25), legend.text = element_text(size = 16),legend.title = element_text(size = 16), 
              plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
    }
    
    q
  })
  
  output$mosaic_1 <- renderPlot({
    
    validate(need(nrow(filtered_data_6()) > 0, message = FALSE))
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
    axis_angle <- input$axis_angle_2
    text_size <- input$text_size_2
    
    ggplot(filtered_data_6()) +
      geom_mosaic(aes(x = product(!!sym(input$variable_choice_2x)), fill = !!sym(input$variable_choice_2y))) +
      theme_bw() +
      labs(x = x, y = y, title = str_wrap(x, 70), fill = str_wrap(y, 25), subtitle = paste("Number of Observations: ", nrow(filtered_data_6()), "|", dataset_subtitle_II()), 
           caption = paste("", cohort_label_2(), "\n", str_wrap(filter_label_2(), 70))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), legend.position = "bottom",
            legend.text = element_text(size = 16), legend.title = element_text(size = 16), axis.text = element_text(size = text_size),
            axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold")) +
      coord_flip()
  })
  
  
  
  filtered_data_a <- reactive({
    if(input$missing_data_3 == "Yes" & input$variable_choice_2x != input$variable_choice_2y) {
      mapp_bl_combined_react_II() %>%
        filter(if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2
               else cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & full_vnum_2() %in% input$visit_select_2) %>% 
        mutate(!!sym(input$variable_choice_2x) := as.factor(ifelse(!!sym(input$variable_choice_2x) < input$numeric_x, 
                                                                   paste0("< ", input$numeric_x), 
                                                                   paste0("\u2265 ", input$numeric_x))),
               !!sym(input$variable_choice_2y) := as.factor(ifelse(!!sym(input$variable_choice_2y) < input$numeric_y, 
                                                                   paste0("< ", input$numeric_y), 
                                                                   paste0("\u2265 ", input$numeric_y)))) %>% 
        select(!!sym(input$variable_choice_2x), !!sym(input$variable_choice_2y))
    }
    
    else if(input$missing_data_3 == "No" & input$variable_choice_2x != input$variable_choice_2y) {
      mapp_bl_combined_react_II() %>%
        filter(if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2
               else cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & full_vnum_2() %in% input$visit_select_2) %>% 
        mutate(!!sym(input$variable_choice_2x) := as.factor(ifelse(!!sym(input$variable_choice_2x) < input$numeric_x, 
                                                                   paste0("< ", input$numeric_x), 
                                                                   paste0("\u2265 ", input$numeric_x))),
               !!sym(input$variable_choice_2y) := as.factor(ifelse(!!sym(input$variable_choice_2y) < input$numeric_y, 
                                                                   paste0("< ", input$numeric_y), 
                                                                   paste0("\u2265 ", input$numeric_y)))) %>% 
        select(!!sym(input$variable_choice_2x), !!sym(input$variable_choice_2y)) %>% 
        drop_na()
      
    } else if (input$missing_data_3 == "Yes" & input$variable_choice_2x == input$variable_choice_2y) {
      mapp_bl_combined_react_II() %>%
        filter(if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2
               else cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & full_vnum_2() %in% input$visit_select_2) %>% 
        mutate(!!sym(input$variable_choice_2x) := as.factor(ifelse(!!sym(input$variable_choice_2x) < input$numeric_x, 
                                                                   paste0("< ", input$numeric_x), 
                                                                   paste0("\u2265 ", input$numeric_x)))) %>% 
        select(!!sym(input$variable_choice_2x))
      
    } else {
      mapp_bl_combined_react_II() %>%
        filter(if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2
               else cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & full_vnum_2() %in% input$visit_select_2) %>% 
        mutate(!!sym(input$variable_choice_2x) := as.factor(ifelse(!!sym(input$variable_choice_2x) < input$numeric_x, 
                                                                   paste0("< ", input$numeric_x), 
                                                                   paste0("\u2265 ", input$numeric_x)))) %>% 
        select(!!sym(input$variable_choice_2x)) %>% 
        drop_na()
    }
  })
  
  filtered_data_c <- reactive({
    if(input$missing_data_3 == "Yes" & !!sym(input$variable_choice_2x) %in% variable_assignments_5_react_II()$variable) {
      mapp_bl_combined_react_II() %>%
        filter(if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2
               else cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & full_vnum_2() %in% input$visit_select_2) %>% 
        mutate(!!sym(input$variable_choice_2x) := as.factor(ifelse(!!sym(input$variable_choice_2x) < input$numeric_x, 
                                                                   paste0("< ", input$numeric_x), 
                                                                   paste0("\u2265 ", input$numeric_x)))) %>% 
        select(!!sym(input$variable_choice_2x), !!sym(input$variable_choice_2y))
    }
    
    else if(input$missing_data_3 == "No" & !!sym(input$variable_choice_2x) %in% variable_assignments_5_react_II()$variable) {
      mapp_bl_combined_react_II() %>%
        filter(if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2
               else cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & full_vnum_2() %in% input$visit_select_2) %>% 
        mutate(!!sym(input$variable_choice_2x) := as.factor(ifelse(!!sym(input$variable_choice_2x) < input$numeric_x, 
                                                                   paste0("< ", input$numeric_x), 
                                                                   paste0("\u2265 ", input$numeric_x)))) %>% 
        select(!!sym(input$variable_choice_2x), !!sym(input$variable_choice_2y)) %>% 
        drop_na()
      
    } else if (input$missing_data_3 == "Yes" & !!sym(input$variable_choice_2y) %in% variable_assignments_5_react_II()$variable) {
      mapp_bl_combined_react_II() %>%
        filter(if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2
               else cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & full_vnum_2() %in% input$visit_select_2) %>% 
        mutate(!!sym(input$variable_choice_2y) := as.factor(ifelse(!!sym(input$variable_choice_2y) < input$numeric_y, 
                                                                   paste0("< ", input$numeric_y), 
                                                                   paste0("\u2265 ", input$numeric_y)))) %>% 
        select(!!sym(input$variable_choice_2x), !!sym(input$variable_choice_2y))
      
    } else {
      mapp_bl_combined_react_II() %>%
        filter(if(input$dataset_2 == "MAPP I Baseline" | input$dataset_2 == "MAPP II Baseline") cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2
               else cohorttype2 %in% input$cohort_type_2 & gg_2() %in% input$checkbox_filter_2 & full_vnum_2() %in% input$visit_select_2) %>% 
        mutate(!!sym(input$variable_choice_2y) := as.factor(ifelse(!!sym(input$variable_choice_2y) < input$numeric_y, 
                                                                   paste0("< ", input$numeric_y), 
                                                                   paste0("\u2265 ", input$numeric_y)))) %>% 
        select(!!sym(input$variable_choice_2x), !!sym(input$variable_choice_2y)) %>% 
        drop_na()
    }
  })
  
  
  
  #2 Variable Summary
  ################################################
  my_table_15 <- reactive({
      x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
      y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
      
      filtered_data_a() %>%
        tbl_cross(
          row = !!sym(input$variable_choice_2x),
          col = !!sym(input$variable_choice_2y),
          percent = "cell",
          label = !!sym(input$variable_choice_2x) ~ x,
          missing_text = "Missing Observations",
          missing = "ifany",
          digits = c(0, 2)) %>%
        modify_spanning_header(all_stat_cols() ~ y) %>%
        modify_header(label ~ "**Variable**") %>%
        modify_caption("Summary Statistics") %>%
        modify_footnote(all_stat_cols() ~ paste(cohort_label_2(), " | ", filter_label_2(), "|", dataset_subtitle_II())) %>% 
        add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
        as_gt()
  })
  
  my_image_15 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_15(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t15 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_15(), file)
      
    },
  )
  
  my_table_16 <- reactive({
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
    
    filtered_data_a() %>%
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice_2x) ~ x,
        type = all_categorical() ~ "categorical",
        statistic = all_categorical() ~ "{n} ({p}%)",
        digits = all_categorical() ~ c(0, 2)) %>%
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>%
      modify_header(label ~ "**Variable**") %>%
      modify_caption("Summary Statistics ~ ", a) %>%
      modify_footnote(all_stat_cols() ~ paste(cohort_label_2(), " | ", filter_label_2(), "|", dataset_subtitle_II())) %>% 
      as_gt()
  })
  
  my_image_16 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_16(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t16 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_16(), file)
      
    },
  )
  
  output$summary_a <- render_gt({
    
    req(input$variable_choice_2x)
    req(input$variable_choice_2y)
    req(input$cohort_type_2)
    req(input$checkbox_filter_2)
    req(input$numeric_x)
    req(input$numeric_y)
    
    if(input$variable_choice_2x != input$variable_choice_2y) {
      my_table_15()
    }
    
    else {
      my_table_16()
    }

  })
  
  ##############################################################################
  my_table_17 <- reactive({
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
    
    filtered_data_6() %>% 
      dplyr::select(input$variable_choice_2x, input$variable_choice_2y) %>% 
      tbl_cross(
        row = input$variable_choice_2y,
        col = input$variable_choice_2x,
        percent = "cell",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice_2y) ~ y,
        digits = c(0, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ paste(x)) %>% 
      modify_caption("Summary Statistics") %>% 
      modify_header(label = "**Variable**") %>%
      modify_footnote(all_stat_cols() ~ paste(cohort_label_2(), " | ", filter_label_2(), "|", dataset_subtitle_II())) %>% 
      add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
      as_gt()
  })
  
  my_image_17 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_17(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t17 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_17(), file)
      
    },
  )
  
  my_table_18 <- reactive({
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
    
    filtered_data_6() %>% 
      dplyr::select(input$variable_choice_2x) %>% 
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        type = all_categorical() ~ "categorical",
        statistic = all_categorical() ~ "{n} ({p}%)",
        label = !!sym(input$variable_choice_2x) ~ x,
        digits = all_categorical() ~ c(0, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>% 
      modify_header(label = "**Variable**") %>%
      modify_caption("Summary Table") %>% 
      modify_footnote(all_stat_cols() ~ paste(cohort_label_2(), " | ", filter_label_2(), "|", dataset_subtitle_II())) %>% 
      as_gt()
  })
  
  my_image_18 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_18(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t18 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_18(), file)
      
    },
  )
  
  output$summary_b <- render_gt({
    
    validate(need(nrow(filtered_data_6()) > 0, message = FALSE))
    
    if(input$variable_choice_2x != input$variable_choice_2y) {
      my_table_17()
    }
    
    else {
      my_table_18()
    }
    
  })
  
  ######################################################################################
  my_table_c <- reactive({
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]

    if(!!sym(input$variable_choice_2x) %in% variable_assignments_5_react_II()$variable) {

      filtered_data_c() %>%
        tbl_cross(
          row = !!sym(input$variable_choice_2x),
          col = !!sym(input$variable_choice_2y),
          percent = "cell",
          label = !!sym(input$variable_choice_2x) ~ x,
          missing_text = "Missing Observations",
          missing = "ifany",
          digits = c(0, 2)) %>%
        modify_spanning_header(all_stat_cols() ~ y) %>%
        modify_header(label ~ "**Variable**") %>%
        modify_caption("Summary Statistics") %>%
        modify_footnote(all_stat_cols() ~ paste(cohort_label_2(), " | ", filter_label_2(), "|", dataset_subtitle_II())) %>%
        as_gt()
      
    }

    else {
      
      filtered_data_c() %>%
        tbl_cross(
          row = !!sym(input$variable_choice_2y),
          col = !!sym(input$variable_choice_2x),
          percent = "cell",
          label = !!sym(input$variable_choice_2x) ~ x,
          missing_text = "Missing Observations",
          missing = "ifany",
          digits = c(0, 2)) %>%
        modify_spanning_header(all_stat_cols() ~ y) %>%
        modify_header(label ~ "**Variable**") %>%
        modify_caption("Summary Statistics") %>%
        modify_footnote(all_stat_cols() ~ paste(cohort_label_2(), " | ", filter_label_2(), "|", dataset_subtitle_II())) %>%
        as_gt()
    }

  })
  
  my_image_c <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_c(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_tc <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_c(), file)
      
    },
  )
  
  output$summary_c <- render_gt({
    
    req(input$variable_choice_2x)
    req(input$variable_choice_2y)
    req(input$cohort_type_2)
    req(input$numeric_x)
    req(input$numeric_y)
    
    my_table_c()
    
  })
  
  
  ##########################################################################################
  my_table_19 <- reactive({
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
    
    t1 <- filtered_data_5x() %>% 
      select(input$variable_choice_2x) %>% 
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice_2x) ~ x,
        type = list(where(is.numeric) ~ "continuous2"),
        statistic = all_continuous() ~ c("{N_nonmiss}",
                                         "{median} ({p25}, {p75})", 
                                         "{min}, {max}",
                                         "{mean} ({sd})"),
        digits = all_continuous() ~ c(0, 2, 2, 2, 0, 0, 2, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>% 
      modify_header(label ~ "**Variable**") %>%
      modify_caption(paste("Summary Statistics"))
    
    t2 <- filtered_data_5y() %>% 
      select(input$variable_choice_2y) %>% 
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice_2y) ~ y,
        type = list(where(is.numeric) ~ "continuous2"),
        statistic = all_continuous() ~ c("{N_nonmiss}",
                                         "{median} ({p25}, {p75})", 
                                         "{min}, {max}",
                                         "{mean} ({sd})"),
        digits = all_continuous() ~ c(0, 2, 2, 2, 0, 0, 2, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>% 
      modify_header(label ~ "**Variable**") %>%
      modify_caption("Summary Statistics")
    
    
    tbl_stack(tbls = list(t1, t2)) %>%
      modify_spanning_header(everything() ~ NA_character_) %>%
      modify_footnote(all_stat_cols() ~ paste(cohort_label_2(), " | ", filter_label_2(), "|", dataset_subtitle_II())) %>%
      as_gt()
  })
  
  my_image_19 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_19(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t19 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_19(), file)
      
    },
  )
  
  my_table_20 <- reactive({
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
    
    filtered_data_5a() %>% 
      select(input$variable_choice_2x) %>% 
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice_2x) ~ x,
        type = list(where(is.numeric) ~ "continuous2"),
        statistic = all_continuous() ~ c("{N_nonmiss}",
                                         "{median} ({p25}, {p75})", 
                                         "{min}, {max}",
                                         "{mean} ({sd})"),
        digits = all_continuous() ~ c(0, 2, 2, 2, 0, 0, 2, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>% 
      modify_header(label ~ "**Variable**") %>%
      modify_caption(paste("Summary Statistics ~ ", input$variable_choice_2x)) %>% 
      modify_footnote(all_stat_cols() ~ paste(cohort_label_2(), " | ", filter_label_2(), "|", dataset_subtitle_II())) %>% 
      as_gt()
  })
  
  my_image_20 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_20(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t20 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_20(), file)
      
    },
  )
  
  output$summary_d <- render_gt({
    
    validate(need(nrow(filtered_data_5a()) > 0, message = FALSE))
    
    if(input$variable_choice_2x != input$variable_choice_2y) {
      my_table_19()
    }
    
    else {
      my_table_20()
    }
    
  })
  
  #######################################################################################
  my_table_21 <- reactive({
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
    
    filtered_data_7() %>%
      dplyr::select(input$variable_choice_2x, input$variable_choice_2y) %>%
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        by = input$variable_choice_2y,
        type = list(where(is.numeric) ~ "continuous2"),
        label = !!sym(input$variable_choice_2x) ~ x,
        statistic = all_continuous() ~ c("{N_nonmiss}",
                                         "{median} ({p25}, {p75})",
                                         "{min}, {max}",
                                         "{mean} ({sd})"),
        digits = all_continuous() ~ c(0, 2, 2, 2, 0, 0, 2, 2)) %>%
      modify_spanning_header(all_stat_cols() ~ y) %>% 
      modify_header(label = "**Variable**") %>% 
      modify_caption("Subgroup Summary Table") %>%
      modify_footnote(all_stat_cols() ~ paste(cohort_label_2(), " | ", filter_label_2(), "|", dataset_subtitle_II())) %>% 
      add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
      as_gt()
  })
  
  my_image_21 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_21(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t21 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_21(), file)
      
    },
  )
  
  my_table_22 <- reactive({
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
    
    filtered_data_7() %>%
      dplyr::select(input$variable_choice_2x, input$variable_choice_2y) %>%
      tbl_summary(
        missing = "ifany",
        missing_text = "Missing Observations",
        by = input$variable_choice_2x,
        type = list(where(is.numeric) ~ "continuous2"),
        label = !!sym(input$variable_choice_2x) ~ x,
        statistic = all_continuous() ~ c("{N_nonmiss}",
                                         "{median} ({p25}, {p75})",
                                         "{min}, {max}",
                                         "{mean} ({sd})"),
        digits = all_continuous() ~ c(0, 2, 2, 2, 0, 0, 2, 2)) %>%
      modify_spanning_header(all_stat_cols() ~ x) %>% 
      modify_header(label = "**Variable**") %>% 
      modify_caption("Subgroup Summary Table") %>%
      modify_footnote(all_stat_cols() ~ paste(cohort_label_2(), " | ", filter_label_2(), "|", dataset_subtitle_II())) %>% 
      as_gt()
  })
  
  my_image_22 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_22(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t22 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_22(), file)
      
    },
  )
  
  
  output$summary_e <- render_gt({
    
    validate(need(nrow(filtered_data_7()) > 0, message = FALSE))
    
    req(input$variable_choice_2x)
    req(input$variable_choice_2y)
    
    if(input$variable_choice_2x %in% variable_assignments_5$variable) {
      my_table_21()
    }
    
    else {
      my_table_22()
    }

  })
  
  #############################################################################################################
  my_table_23 <- reactive({
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
    
    xx <- mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                        (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type) &
                                        (gg_2() %in% input$checkbox_filter_2),] %>% 
      pull(input$variable_choice_2x)
    
    yy <- mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                        (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type) &
                                        (gg_2() %in% input$checkbox_filter_2),] %>% 
      pull(input$variable_choice_2y)
    
    pp <- mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                        (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type) &
                                        (gg_2() %in% input$checkbox_filter_2),]
    
    
    
    reg_a <- lm(yy ~ xx, pp)
    
    reg_a %>%
      tbl_regression(
        pvalue_fun = ~style_pvalue(.x, digits = 3),
        estimate_fun = ~style_sigfig(.x, digits = 2),
        intercept = TRUE
      ) %>%
      bold_labels() %>%
      as_gt()
    
    # filtered_data_7() %>%
    #   dplyr::select(input$variable_choice_2x, input$variable_choice_2y) %>%
    #   tbl_summary(
    #     missing = "ifany",
    #     missing_text = "Missing Observations",
    #     by = input$variable_choice_2y,
    #     type = list(where(is.numeric) ~ "continuous2"),
    #     statistic = all_continuous() ~ c("{N_nonmiss}",
    #                                      "{median} ({p25}, {p75})",
    #                                      "{min}, {max}",
    #                                      "{mean} ({sd})"),
    #     digits = all_continuous() ~ c(0, 2, 2, 2, 0, 0, 2, 2)) %>%
    #   modify_spanning_header(all_stat_cols() ~ input$variable_choice_2y) %>% 
    #   modify_caption("Subgroup Summary Table") %>%
    #   modify_footnote(all_stat_cols() ~ paste(cohort_label_2(), " | ", filter_label_2(), "|", dataset_subtitle_II())) %>% 
    #   add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
    #   as_gt()
  })
  
  my_image_23 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_23(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t23 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_23(), file)
      
    },
  )
  
  output$regression_a <- render_gt({
    validate(need(nrow(filtered_data_7()) > 0, message = FALSE))
    
    req(input$variable_choice_2x)
    req(input$variable_choice_2y)
    
    if(input$variable_choice_2x != input$variable_choice_2y) {
      my_table_23()
    
    } else {
      print("Error: Same Response and Explanatory Variables")
    }
  })
  
 ###############################################################################################################
  my_table_24 <- reactive({
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
    
    xx <- mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                        (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type),] %>% 
      pull(input$variable_choice_2x)
    
    yy <- mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                        (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type),] %>% 
      pull(input$variable_choice_2y)
    
    pp <- mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                        (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type),]
    
    
    
    reg_b <- glm(yy ~ xx, pp)
    
    reg_b %>%
      tbl_regression(
        pvalue_fun = ~style_pvalue(.x, digits = 3),
        intercept = TRUE,
        exponentiate = TRUE
      ) %>%
      bold_labels() %>%
      as_gt()
  })
  
  my_image_24 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_24(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t24 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_24(), file)
      
    },
  )
  
  output$regression_b <- render_gt({
    validate(need(nrow(filtered_data_7()) > 0, message = FALSE))
    
    req(input$variable_choice_2x)
    req(input$variable_choice_2y)
    
    if(input$variable_choice_2x != input$variable_choice_2y) {
      my_table_24()
      
    } else {
      print("Error: Same Response and Explanatory Variables")
    }
  })
  
 ###############################################################################################################
  my_table_25 <- reactive({
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
    
    xx <- mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                        (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type),] %>% 
      pull(input$variable_choice_2x)
    
    yy <- mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                        (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type),] %>% 
      pull(input$variable_choice_2y)
    
    pp <- mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                        (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type),]
    
    
    
    reg_c1 <- glm(yy ~ xx, pp)
    
    reg_c1 %>%
      tbl_regression(
        pvalue_fun = ~style_pvalue(.x, digits = 3),
        intercept = TRUE,
        exponentiate = TRUE
      ) %>%
      bold_labels() %>%
      as_gt()
  })
  
  my_image_25 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_25(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t25 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_25(), file)
      
    },
  )
  
  my_table_26 <- reactive({
    
    x <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2x, "variable_label"]
    y <- full_labels_react_II()[full_labels_react_II()$variable %in% input$variable_choice_2y, "variable_label"]
    
    xx <- mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                        (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type),] %>% 
      pull(input$variable_choice_2x)
    
    yy <- mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                        (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type),] %>% 
      pull(input$variable_choice_2y)
    
    pp <- mapp_bl_combined_react_II()[mapp_bl_combined_react_II()[input$variable_choice_2x] >= min(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2x] <= max(input$x_slider_2) & 
                                        mapp_bl_combined_react_II()[input$variable_choice_2y] >= min(input$y_slider_2) & mapp_bl_combined_react_II()[input$variable_choice_2y] <= max(input$y_slider_2) &
                                        (mapp_bl_combined_react_II()$cohorttype2 %in% input$cohort_type),]
    
    
    
    reg_c2 <- lm(yy ~ xx, pp)
    
    reg_c2 %>%
      tbl_regression(
        pvalue_fun = ~style_pvalue(.x, digits = 3),
        intercept = TRUE,
        exponentiate = TRUE
      ) %>%
      bold_labels() %>%
      as_gt()
  })
  
  my_image_26 <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_26(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t26 <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_26(), file)
      
    },
  )
  
  output$regression_c <- render_gt({
    validate(need(nrow(filtered_data_7()) > 0, message = FALSE))
    
    req(input$variable_choice_2x)
    req(input$variable_choice_2y)
    
    if(input$variable_choice_2x %in% variable_assignments_5$variable) {
      my_table_25()
      
    } else {
      my_table_26()
    }
  })
  
 ###############################################################################
 #######################       LONGITUDUNAL DATA        ########################
 ###############################################################################

  mapp_dataset_long <- reactive({
    if(input$dataset_long == "MAPP I Longitudinal") {
      mapp_long_raw
    }
    else {
      mapp_long_raw_II
    }
  })

  dataset_subtitle_long <- reactive({
    if(input$dataset_long == "MAPP I Longitudinal") {
      dataset_choice_long[1]
    }
    else{
      dataset_choice_long[2]
    }
  })

  variable_assignments_long_3_react <- reactive({
    if(input$dataset_long == "MAPP I Longitudinal") {
      variable_assignments_long_3
    }
    else {
      variable_assignments_long_3_II
    }
  })

  variable_assignments_long_4_react <- reactive({
    if(input$dataset_long == "MAPP I Longitudinal") {
      variable_assignments_long_4
    }
    else {
      variable_assignments_long_4_II
    }
  })
  
  variable_assignments_long_5_react <- reactive({
    if(input$dataset_long == "MAPP I Longitudinal") {
      variable_assignments_long_5
    }
    else {
      variable_assignments_long_5_II
    }
  })

  mapp_long_combined_react <- reactive({
    if(input$dataset_long == "MAPP I Longitudinal") {
      mapp_long_combined
    }
    else {
      mapp_long_combined_II
    }
  })

  mapp_long_labels_react <- reactive({
    if(input$dataset_long == "MAPP I Longitudinal") {
      mapp_long_labels
    }
    else {
      mapp_long_labels_II
    }
  })

  full_labels_long_react <- reactive({
    if(input$dataset_long == "MAPP I Longitudinal") {
      full_labels_long
    }
    else {
      full_labels_long_II
    }
  })

  primary_grouping_long_react <- reactive({
    if(input$dataset_long == "MAPP I Longitudinal") {
      mapp_long_primary$variable_selection
    }
    else {
      mapp_long_primary$variable_selection
    }
  })
  
  primary_grouping_long_react_con <- reactive({
    if(input$dataset_long == "MAPP I Longitudinal") {
      mapp_long_primary_con$variable_selection
    }
    else {
      mapp_long_primary_con$variable_selection
    }
  })

  output$long_cohort <- renderText({
    "1 - UCPPS"
  })
  
  output$variable_group_long_1 <- renderText({
    "Study Feature"
  })
  
  output$variable_choice_long_1 <- renderText({
    "vnum"
  })
  
  output$variable_name_long_1 <- renderText({
    "Visit Number"
  })
  
  output$analysis_type_long_1 <- renderText({
    "1 - Continuous"
  })
  
  output$numeric_interval_long_1 <- renderUI({
    numericInput(
      inputId = "numeric_interval_long_1",
      label = paste("Visit Number X-Axis Intervals"),
      value = 1,
      min = 1,
      max = 10
    )
  })
  
  output$long_slider_1 <- renderUI({
    
    w <- mapp_dataset_long() %>% 
      pull(vnum)
    
    max_v <- round(round(max(w, na.rm = TRUE)))
    min_v <- floor(floor(min(w, na.rm = TRUE)))
    
    sliderInput(
      inputId = "long_slider_1",
      label = "Visit Number Range",
      min = min_v,
      max = max_v,
      value = c(min_v, max_v),
      step = 1,
      ticks = TRUE,
      dragRange = TRUE,
      round = TRUE
    )
  })

  output$variable_name_long_2 <- renderText({
    variable_assignments_long_5_react()[variable_assignments_long_5_react()$variable %in% input$variable_choice_long_2, "label", drop = TRUE]
  })
  
  output$long_analysis_type_2 <- renderText({
    variable_assignments_long_5_react()[variable_assignments_long_5_react()$variable %in% input$variable_choice_long_2, "ANALYSIS_TYPE_2", drop = TRUE]
  })
  
  output$numeric_interval_long_2 <- renderUI({
    numericInput(
      inputId = "numeric_interval_long_2",
      label = paste("Y-Axis Intervals"),
      value = 2,
      min = 1,
      max = 10
    )
  })
  
  output$long_slider_2 <- renderUI({
    req(input$variable_choice_long_2)
    
    w <- mapp_dataset_long() %>% 
      pull(input$variable_choice_long_2)
    
    max_v <- round(round(max(w, na.rm = TRUE)))
    min_v <- floor(floor(min(w, na.rm = TRUE)))
    
    sliderInput(
      inputId = "long_slider_2",
      label = paste0("Filter Observations by ", input$variable_choice_long_2),
      min = min_v,
      max = max_v,
      value = c(min_v, max_v),
      step = 1,
      ticks = TRUE,
      dragRange = TRUE,
      round = TRUE
    )
  })
  
  new_data_long <- reactive({
    mapp_long_combined_react() %>%
      gather(columnNames, values) %>%
      group_by(columnNames) %>%
      distinct(.keep_all = TRUE) %>%
      drop_na()
  })

  observeEvent(input$dataset_long,
               {
                 updateSelectInput(session, input = "variable_filter_group_long", choices = primary_grouping_long_react(), selected = "Demographics/Anthropometrics")
               })

  observeEvent(input$variable_filter_group_long,
               {
                 updateSelectInput(session, input = "variable_filter_choice_long",
                                   choices = sort(variable_assignments_long_4_react()[variable_assignments_long_4_react()$variable_selection %in% input$variable_filter_group_long,
                                                                                    "variable", drop = TRUE]))
               })

  observeEvent(input$dataset_long,
               {
                 updateSelectInput(session, input = "variable_filter_choice_long",
                                   choices = sort(variable_assignments_long_4_react()[variable_assignments_long_4_react()$variable_selection %in% input$variable_filter_group_long,
                                                                                    "variable", drop = TRUE]))
               })

  observeEvent(input$variable_filter_choice_long,
               {
                 bb_2 <- new_data_long()

                 updateCheckboxGroupInput(session, input = "checkbox_filter_long", choices = sort(bb_2[bb_2$columnNames %in% input$variable_filter_choice_long, "values", drop = TRUE]),
                                          selected = bb_2[bb_2$columnNames %in% input$variable_filter_choice_long, "values", drop = TRUE])
               })

  observeEvent(input$checking_filter_long,
               {
                 bb_2 <- new_data_long()

                 updateCheckboxGroupInput(session, input = "checkbox_filter_long", choices = sort(bb_2[bb_2$columnNames %in% input$variable_filter_choice_long, "values", drop = TRUE]),
                                          selected = bb_2[bb_2$columnNames %in% input$variable_filter_choice_long, "values", drop = TRUE])
               })

  ############################################

  observeEvent(input$dataset_long,
               {
                 updateSelectInput(session, input = "facet_wrap_group", choices = primary_grouping_long_react(), selected = "Demographics/Anthropometrics")
               })

  observeEvent(input$facet_wrap_group,
               {
                 updateSelectInput(session, input = "facet_wrap_variable",
                                   choices = sort(variable_assignments_long_4_react()[variable_assignments_long_4_react()$variable_selection %in% input$facet_wrap_group,
                                                                                    "variable", drop = TRUE]))
               })

  observeEvent(input$dataset_long,
               {
                 updateSelectInput(session, input = "facet_wrap_variable",
                                   choices = sort(variable_assignments_long_4_react()[variable_assignments_long_4_react()$variable_selection %in% input$facet_wrap_group,
                                                                                    "variable", drop = TRUE]))
               })

  observeEvent(input$facet_wrap_variable,
               {
                 bb_2 <- new_data_long()

                 updateCheckboxGroupInput(session, input = "facet_filter", choices = sort(bb_2[bb_2$columnNames %in% input$facet_wrap_variable, "values", drop = TRUE]),
                                          selected = bb_2[bb_2$columnNames %in% input$facet_wrap_variable, "values", drop = TRUE])
               })

  observeEvent(input$facet_wrap,
               {
                 bb_2 <- new_data_long()

                 updateCheckboxGroupInput(session, input = "facet_filter", choices = sort(bb_2[bb_2$columnNames %in% input$facet_wrap_variable, "values", drop = TRUE]),
                                          selected = bb_2[bb_2$columnNames %in% input$facet_wrap_variable, "values", drop = TRUE])
               })

  ##########################################################
  observeEvent(input$dataset_long,
               {
                 updateSelectInput(session, input = "variable_group_long_2", choices = primary_grouping_long_react_con(), selected = "Pelvic/Urological Symptoms")
               })

  observeEvent(input$variable_group_long_2,
               {
                 updateSelectInput(session, input = "variable_choice_long_2",
                                   choices = sort(variable_assignments_long_5_react()[variable_assignments_long_5_react()$variable_selection %in% input$variable_group_long_2,
                                                                                    "variable", drop = TRUE]))
               })

  observeEvent(input$dataset_long,
               {
                 updateSelectInput(session, input = "variable_choice_long_2",
                                   choices = sort(variable_assignments_long_5_react()[variable_assignments_long_5_react()$variable_selection %in% input$variable_group_long_2,
                                                                                    "variable", drop = TRUE]))
               })
  
  gg_long <- reactive({
    req(input$variable_filter_choice_long)
    
    mapp_long_combined_react() %>% 
      pull(input$variable_filter_choice_long)
  })
  
  gg_long_2 <- reactive({
    req(input$facet_wrap_variable)
    
    mapp_long_combined_react() %>% 
      pull(input$facet_wrap_variable)
  })
  
  violin_data <- reactive({
    mapp_long_combined_react() %>% 
      filter(gg_long() %in% input$checkbox_filter_long & gg_long_2() %in% input$facet_filter) %>% 
      select(input$variable_choice_long_2, vnum) %>% 
      mutate(vnum = as.factor(vnum)) %>% 
      drop_na()
  })
  
  output$long_violin_1 <- renderPlot({
      ggplot(violin_data(), aes_string(as.factor("vnum"), input$variable_choice_long_2)) +
       geom_violin(trim = FALSE, na.rm = TRUE) +
       stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "pointrange", color = "black") +
       theme_bw() +
       labs(x = "Visit Number")
  })
  
  #Summary Table Longitudinal Data
  long_summary_data <- reactive({
    
    req(input$long_slider_1)
    req(input$long_slider_2)
    req(input$variable_choice_long_2)
    
    mapp_long_combined_react()[mapp_long_combined_react()["vnum"] >= min(input$long_slider_1) & 
                               mapp_long_combined_react()["vnum"] <= max(input$long_slider_1) & 
                               mapp_long_combined_react()[input$variable_choice_long_2] >= min(input$long_slider_2) & 
                               mapp_long_combined_react()[input$variable_choice_long_2] <= max(input$long_slider_2) &
                               gg_long() %in% input$checkbox_filter_long,]
  })
  
  #Summary Table for Longitudinal Data
  filter_label_long <- reactive({
    
    req(input$variable_choice_long_2)
    req(input$checkbox_filter_long)
    
    v <- paste(full_labels_long_react()[full_labels_long_react()$variable %in% input$variable_choice_long_2, "variable_label"], ":")
    
    if(input$checking_filter_long == 1 & input$captions_long == "Yes") {
      paste(v, paste(unique(input$checkbox_filter_long[input$checkbox_filter_long %in% gg_long()]), collapse = ", "))
    }
    else {
      return("")
    }
  })
  
   facet_label_long <- reactive({
    
    v <- paste(full_labels_long_react()[full_labels_long_react()$variable %in% input$facet_wrap_variable, "variable_label"], ":")
    
    if(input$captions_long == "Yes" & input$facet_wrap == TRUE) {
      paste(v, paste(unique(input$facet_filter[input$facet_filter %in% gg_long_2()]), collapse = ", "))
    }
    else {
      return("")
    }
  })
  
  cohort_label_long <- reactive({
    req(input$cohort_type)
    
    if(input$captions_long == "Yes") {
      paste("Cohort: UCPPS")
    }
    else {
      return("")
    }
  })
  
  my_table_long <- reactive({

    y <- full_labels_long_react()[full_labels_long_react()$variable %in% input$variable_choice_long_2, "variable_label"]
    
    long_summary_data() %>% 
      mutate(vnum = as.factor(vnum)) %>% 
      select(input$variable_choice_long_2, vnum) %>% 
      tbl_summary(
        by = vnum,
        missing = "ifany",
        missing_text = "Missing Observations",
        label = !!sym(input$variable_choice_long_2) ~ y,
        type = list(where(is.numeric) ~ "continuous2"),
        statistic = all_continuous() ~ c("{N_nonmiss}",
                                         "{median} ({p25}, {p75})", 
                                         "{min}, {max}",
                                         "{mean} ({sd})"),
        digits = all_continuous() ~ c(0, 2, 2, 2, 0, 0, 2, 2)) %>% 
      modify_spanning_header(all_stat_cols() ~ "Visit Number") %>%
      modify_header(label ~ "**Variable**") %>%
      modify_caption("Summary Table") %>%
      modify_footnote(all_stat_cols() ~ paste(filter_label_long(), "|", dataset_subtitle_long())) %>% 
      as_gt()
  })
  
  my_image_long <- reactive({
    outfile <- tempfile(fileext = ".pdf")
    gtsave(data = my_table_long(), 
           filename = outfile, 
           vwidth = 800, 
           vheight = 600)
    
    outfile
  })
  
  output$download_t_long <- downloadHandler(
    filename = paste("table-", Sys.Date(), ".pdf", sep = ""),
    
    content = function(file) {
      file.copy(my_image_long(), file)
      
    },
  )
  
  output$summary_long_1 <- render_gt({
    my_table_long()
  })
  
  download_type_long_1 <- reactive(
    if(input$download_type == ".pdf") {
      ".pdf"
    }
    else {
      ".png"
    }
  )
  
  download_type_long_2 <- reactive(
    if(input$download_type == ".pdf") {
      "pdf"
    }
    else {
      "png"
    }
  )
  
  output$download_plot_long <- downloadHandler(
    filename = function() {
      paste("plot-", Sys.Date(), download_type_long_1(), sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = download_type_long_2(), dpi = 600, width = 10, height = 8, units = "in")
    }
  )
  
  long_graph_data <- reactive({
    
    req(input$long_slider_1)
    req(input$long_slider_2)
    req(input$variable_choice_long_2)
    req(input$checkbox_filter_long)
    req(input$facet_filter)
    
    if(input$missing_data_long == "Yes") {
    mapp_long_combined_react()[mapp_long_combined_react()["vnum"] >= min(input$long_slider_1) & 
                                 mapp_long_combined_react()["vnum"] <= max(input$long_slider_1) & 
                                 mapp_long_combined_react()[input$variable_choice_long_2] >= min(input$long_slider_2) & 
                                 mapp_long_combined_react()[input$variable_choice_long_2] <= max(input$long_slider_2) &
                                 gg_long() %in% input$checkbox_filter_long &
                                 gg_long_2() %in% input$facet_filter,] %>% 
      select(pid, vnum, input$variable_choice_long_2, input$facet_wrap_variable)
    }
    else {
      mapp_long_combined_react()[mapp_long_combined_react()["vnum"] >= min(input$long_slider_1) & 
                                   mapp_long_combined_react()["vnum"] <= max(input$long_slider_1) & 
                                   mapp_long_combined_react()[input$variable_choice_long_2] >= min(input$long_slider_2) & 
                                   mapp_long_combined_react()[input$variable_choice_long_2] <= max(input$long_slider_2) &
                                   gg_long() %in% input$checkbox_filter_long &
                                   gg_long_2() %in% input$facet_filter,] %>% 
        select(pid, vnum, input$variable_choice_long_2, input$facet_wrap_variable) %>% 
        drop_na(input$facet_wrap_variable)
    }
  })
  
  base_plot_long <- reactive({
    req(input$variable_choice_long_2)
    req(input$numeric_interval_long_1)
    req(input$numeric_interval_long_2)
    
    
    a <- min(input$long_slider_1) - 1
    b <- max(input$long_slider_1)
    c <- min(input$long_slider_1)
    d <- max(input$long_slider_1) + 1
    
    e <- min(input$long_slider_2) - 1
    f <- max(input$long_slider_2)
    g <- min(input$long_slider_2)
    h <- max(input$long_slider_2) + 1
    
    y <- full_labels_long_react()[full_labels_long_react()$variable %in% input$variable_choice_long_2, "variable_label"]
    
    if(input$long_lowess == TRUE | input$long_mean == TRUE | input$long_median == TRUE) {
    ggplot(long_graph_data(), aes_string("vnum", input$variable_choice_long_2, group = "pid")) +
      theme_bw() +
      labs(x = "Visit Number", y = y, title = str_wrap(paste(y, "by Visit Number")), 
           subtitle = paste("Number of Observations: ", nrow(long_graph_data()), "|", dataset_subtitle_long()), 
                            caption = paste("", facet_label_long(), "\n", cohort_label_long(), "\n", str_wrap(filter_label_long(), 70))) +
      scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_long_1)) +
      scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_long_2)) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
            axis.title = element_text(size = 18, face = "bold"), legend.position = "right", 
            legend.text = element_text(size = 16), legend.title = element_text(size = 16), 
            plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
    }
    else {
      ggplot(long_graph_data(), aes_string("vnum", input$variable_choice_long_2, group = "pid")) +
        geom_line() +
        theme_bw() +
        labs(x = "Visit Number", y = y, title = str_wrap(paste(y, "by Visit Number")), 
             subtitle = paste("Number of Observations: ", nrow(long_graph_data()), "|", dataset_subtitle_long()), 
             caption = paste("", facet_label_long(), "\n", cohort_label_long(), "\n", str_wrap(filter_label_long(), 70))) +
        scale_x_continuous(limits = c(a, d), breaks = seq(c, b, by = input$numeric_interval_long_1)) +
        scale_y_continuous(limits = c(e, h), breaks = seq(g, f, by = input$numeric_interval_long_2)) +
        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16), 
              axis.title = element_text(size = 18, face = "bold"), legend.position = "right", 
              legend.text = element_text(size = 16), legend.title = element_text(size = 16), 
              plot.caption = element_text(hjust = 0, size = 14, face = "bold"))
    }
  })

  
  output$long_plot_1 <- renderPlot(
    
    height = function() ifelse(input$facet_wrap == TRUE, 1000, 600),
    {
    req(input$facet_wrap_variable)
    validate(need(nrow(long_graph_data()) > 0, message = FALSE))
    
    if(input$facet_wrap == TRUE & input$long_lowess == TRUE & input$long_mean == TRUE & input$long_median == TRUE) {
      base_plot_long() +
        geom_line(color = "gray", alpha = 0.7) +
        facet_wrap(. ~ get(input$facet_wrap_variable), ncol = 2) +
        stat_smooth(aes(group = 1), se = FALSE) +
        stat_summary(aes(group = 1), geom = "point", fun = mean, shape = 18, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = mean, size = 0.2, color = "black") +
        stat_summary(aes(group = 1), geom = "point", fun = median, shape = 8, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = median, size = 0.2, color = "black") +
        theme(strip.text.x = element_text(size = 15), axis.text = element_text(size = 14))
    } 
    else if(input$facet_wrap == TRUE & input$long_lowess == FALSE & input$long_mean == TRUE & input$long_median == TRUE) {
      base_plot_long() +
        geom_line(color = "gray", alpha = 0.7) +
        facet_wrap(. ~ get(input$facet_wrap_variable), ncol = 2) +
        stat_summary(aes(group = 1), geom = "point", fun = mean, shape = 18, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "point", fun = median, shape = 8, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = mean, size = 0.2, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = median, size = 0.2, color = "black") +
        theme(strip.text.x = element_text(size = 15), axis.text = element_text(size = 14))
    }
    else if (input$facet_wrap == TRUE & input$long_lowess == TRUE & input$long_mean == FALSE & input$long_median == TRUE) {
      base_plot_long() +
        geom_line(color = "gray", alpha = 0.7) +
        facet_wrap(. ~ get(input$facet_wrap_variable), ncol = 2) +
        stat_smooth(aes(group = 1), se = FALSE) +
        stat_summary(aes(group = 1), geom = "point", fun = median, shape = 8, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = median, size = 0.2, color = "black") +
        theme(strip.text.x = element_text(size = 15), axis.text = element_text(size = 14))
    }
    else if(input$facet_wrap == TRUE & input$long_lowess == TRUE & input$long_mean == TRUE & input$long_median == FALSE) {
      base_plot_long() +
        geom_line(color = "gray", alpha = 0.7) +
        facet_wrap(. ~ get(input$facet_wrap_variable), ncol = 2) +
        stat_smooth(aes(group = 1), se = FALSE) +
        stat_summary(aes(group = 1), geom = "point", fun = mean, shape = 18, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = mean, size = 0.2, color = "black") +
        theme(strip.text.x = element_text(size = 15), axis.text = element_text(size = 14))
    }
    else if(input$facet_wrap == TRUE & input$long_lowess == FALSE & input$long_mean == FALSE & input$long_median == TRUE) {
      base_plot_long() +
        geom_line(color = "gray", alpha = 0.7) +
        facet_wrap(. ~ get(input$facet_wrap_variable), ncol = 2) +
        stat_summary(aes(group = 1), geom = "point", fun = median, shape = 8, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = median, size = 0.2, color = "black") +
        theme(strip.text.x = element_text(size = 15), axis.text = element_text(size = 14))
    }
    else if(input$facet_wrap == TRUE & input$long_lowess == FALSE & input$long_mean == FALSE & input$long_median == FALSE) {
      base_plot_long() +
        geom_line(color = "black", alpha = 0.7) +
        facet_wrap(. ~ get(input$facet_wrap_variable), ncol = 2) +
        theme(strip.text.x = element_text(size = 15), axis.text = element_text(size = 14))
    }
    else if(input$facet_wrap == TRUE & input$long_lowess == TRUE & input$long_mean == FALSE & input$long_median == FALSE) {
      base_plot_long() +
        geom_line(color = "black", alpha = 0.7) +
        facet_wrap(. ~ get(input$facet_wrap_variable), ncol = 2) +
        stat_smooth(aes(group = 1), se = FALSE) +
        theme(strip.text.x = element_text(size = 15), axis.text = element_text(size = 14))
    }
    else if(input$facet_wrap == TRUE & input$long_lowess == FALSE & input$long_mean == TRUE & input$long_median == FALSE) {
      base_plot_long() +
        geom_line(color = "gray", alpha = 0.7) +
        facet_wrap(. ~ get(input$facet_wrap_variable), ncol = 2) +
        stat_summary(aes(group = 1), geom = "point", fun = mean, shape = 18, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = mean, size = 0.2, color = "black") +
        theme(strip.text.x = element_text(size = 15), axis.text = element_text(size = 14))
    }
    else if(input$facet_wrap == FALSE & input$long_lowess == TRUE & input$long_mean == TRUE & input$long_median == TRUE) {
      base_plot_long() +
        geom_line(color = "gray", alpha = 0.7) +
        stat_smooth(aes(group = 1), se = FALSE) +
        stat_summary(aes(group = 1), geom = "point", fun = mean, shape = 18, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "point", fun = median, shape = 8, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = mean, size = 0.2, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = median, size = 0.2, color = "black") +
        theme(axis.text = element_text(size = 18))
    }
    else if(input$facet_wrap == FALSE & input$long_lowess == FALSE & input$long_mean == TRUE & input$long_median == TRUE) {
      base_plot_long() +
        geom_line(color = "gray", alpha = 0.7) +
        stat_summary(aes(group = 1), geom = "point", fun = mean, shape = 18, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "point", fun = median, shape = 8, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = mean, size = 0.2, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = median, size = 0.2, color = "black") +
        theme(axis.text = element_text(size = 18))
    }
    else if(input$facet_wrap == FALSE & input$long_lowess == TRUE & input$long_mean == FALSE & input$long_median == TRUE) {
      base_plot_long() +
        geom_line(color = "gray", alpha = 0.7) +
        stat_smooth(aes(group = 1), se = FALSE) +
        stat_summary(aes(group = 1), geom = "point", fun = median, shape = 8, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = median, size = 0.2, color = "black") +
        theme(axis.text = element_text(size = 18))
    }
    else if(input$facet_wrap == FALSE & input$long_lowess == TRUE & input$long_mean == TRUE & input$long_median == FALSE) {
      base_plot_long() +
        geom_line(color = "gray", alpha = 0.7) +
        stat_smooth(aes(group = 1), se = FALSE) +
        stat_summary(aes(group = 1), geom = "point", fun = mean, shape = 18, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = mean, size = 0.2, color = "black") +
        theme(axis.text = element_text(size = 18))
    }
    else if(input$facet_wrap == FALSE & input$long_lowess == FALSE & input$long_mean == FALSE & input$long_median == TRUE) {
      base_plot_long() +
        geom_line(color = "gray", alpha = 0.7) +
        stat_summary(aes(group = 1), geom = "point", fun = median, shape = 8, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = median, size = 0.2, color = "black") +
        theme(axis.text = element_text(size = 18))
    }
    else if(input$facet_wrap == FALSE & input$long_lowess == TRUE & input$long_mean == FALSE & input$long_median == FALSE) {
      base_plot_long() +
        geom_line(color = "black", alpha = 0.7) +
        stat_smooth(aes(group = 1), se = FALSE) +
        theme(axis.text = element_text(size = 18))
    }
    else if(input$facet_wrap == FALSE & input$long_lowess == FALSE & input$long_mean == TRUE & input$long_median == FALSE) {
      base_plot_long() +
        geom_line(color = "black", alpha = 0.7) +
        stat_summary(aes(group = 1), geom = "point", fun = mean, shape = 18, size = 1.5, color = "black") +
        stat_summary(aes(group = 1), geom = "line", fun = mean, size = 0.2, color = "black") +
        theme(axis.text = element_text(size = 18))
    }
    else {
      base_plot_long() +
        theme(axis.text = element_text(size = 18))
    }
    
    
  })
}