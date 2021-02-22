

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# Version 1.0 
# February 2021
#
# App for estimating how change in SCORE and ASCVD algorithms translate to life years 
# gained or lost
#
# © Nina Mars & Joni Lindbohm
#
# nina.mars@helsinki.fi
# joni.lindbohm@helsinki.fi

# No individual-level data is deposited with the app nor stored when using the app

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(reshape)
library(dplyr)
library(tidyr)
library(readxl)
library(tidyverse)
library(scales)
library(ggthemes)
library(survival)
library(shiny)
library(rms)
library(stats)
library(shinyjs)
library(foreign)
library(shinycssloaders)
library(survminer)
library(shinyWidgets)
library(shiny)
library(htmltools)
library(shinythemes)
library(fresh)
library(bslib)

#-------------------------------------------------------------------------------------------

# Construct UI
ui <- navbarPage(title = span(strong("Select one of two calculators:"),
                              style = "color:#5B5B5B;font-size:14px"), fluid = T, theme = "lumen",

                 header = use_theme(
                   create_theme( theme = "lumen",
                     bs_vars_navbar(
                       default_bg = "#FAFAFA",
                       default_color = "#FBFBFB",
                       default_link_color = "#686868",
                       default_link_active_color = "#D62E35",
                       default_link_active_bg = "#EDEDED", 
                       default_link_hover_color = "#D62E35"), output_file = NULL)),
                 
                 tabPanel(title = strong("Add risk history to algorithm"),
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              tags$style(type = 'text/css', ".selectize-input { font-size: 10px; line-height: 12px; }"),
                              
                              prettyRadioButtons("sex", "Sex:", choices = c("Male" = "male", "Female" = "female"), selected = NULL),
                              
                              fluidRow(column(6, h5(strong("Previous visit:"))),
                                       column(6, h5(strong("Current visit:")))),
                                
                              fluidRow(
                                column(6, numericInput("age0", "Age", value = 50, step = NA, width = NULL)),
                                column(6, numericInput("age1", "Age", value = 55, step = NA, width = NULL))),
                              
                              fluidRow(
                                column(6, prettyRadioButtons("smok0", "Smoking", choices = c("No" = 0, "Yes" = 1), selected = NULL)),
                                column(6, prettyRadioButtons("smok1", "Smoking", choices = c("No" = 0, "Yes" = 1), selected = NULL))),
                              
                              fluidRow(
                                column(6, numericInput("sbp0", "Systolic blood pressure", value = 130, step = NA, width = NULL)),
                                column(6, numericInput("sbp1", "Systolic blood pressure", value = 130, step = NA, width = NULL))),
                              
                              fluidRow(
                                column(6, numericInput("chol0", "Total cholesterol", value = 5.5, step = NA, width = NULL)),
                                column(6, numericInput("chol1", "Total cholesterol", value = 5.5, step = NA, width = NULL))),
                              
                              fluidRow(
                                column(6, numericInput("hdl0", "HDL", value = 2.0, step = NA, width = NULL)),
                                column(6, numericInput("hdl1", "HDL", value = 2.0, step = NA, width = NULL))),
                              
                              fluidRow(
                                column(6, prettyRadioButtons("bpmed0", "Medication for\nhigh blood\npressure", choices = c("No" = 0, "Yes" = 1), selected = NULL)),
                                column(6, prettyRadioButtons("bpmed1", "Medication for\nhigh blood\npressure", choices = c("No" = 0, "Yes" = 1), selected = NULL))),
                              
                              fluidRow(
                                column(6, prettyRadioButtons("diab0", "Diabetes", choices = c("No" = 0, "Yes" = 1), selected = NULL)),
                                column(6, prettyRadioButtons("diab1", "Diabetes", choices = c("No" = 0, "Yes" = 1), selected = NULL))),
                              
                              br(), basicPage(actionButton("plotaction1", "Calculate risk", style="color:#FFFBFB; font-size:13px; background-color:#636363; font-family:OpenSans-Bold; border-color: #766F6E", width = "211px")) ),
                            
                            mainPanel(tabsetPanel(type = "tabs",
                                                  tabPanel("SCORE", plotOutput("scoreplot1", height = "500px")),
                                                  tabPanel("ASCVD", plotOutput("ascvdplot1", height = "500px"))))
                          )),
                 
                 tabPanel(strong("Add intervention goals"),
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              h4("Current visit: "),
                              prettyRadioButtons("sex_lcc", "Sex:", choices = c("Male" = "male", "Female" = "female"), selected = NULL),
                              numericInput("age_lcc", "Age", value = 50, min = NA, max = NA, step = NA, width = NULL),
                              prettyRadioButtons("smok_lcc", "Smoking", choices = c("No" = 0, "Yes" = 1), selected = NULL),
                              numericInput("sbp_lcc", "SBP", value = 130, min = NA, max = NA, step = NA, width = NULL),
                              numericInput("chol_lcc", "TC", value = 5.5, min = NA, max = NA, step = NA, width = NULL),
                              numericInput("hdl_lcc", "HDL", value = 1.2, min = NA, max = NA, step = NA, width = NULL),
                              prettyRadioButtons("bpmed_lcc", "Medication for high blood pressure", choices = c("No" = 0, "Yes" = 1), selected = NULL),
                              prettyRadioButtons("diab_lcc", "Diabetes", choices = c("No" = 0, "Yes" = 1), selected = NULL),
                              h4(""),
                              hr(style = "border-top: 1px solid #B6B6B6;"),
                              prettyRadioButtons("time_goal", "Goal for timeline:",
                                                 choices = c("1 year" = 1, "2 years" = 2, "3 years" = 3, "4 years" = 4, "5 years" = 5), selected = 5),
                              hr(style = "border-top: 1px solid #B6B6B6;"),
                              h4("Select lifestyle changes: "),
                              h5(strong("Chages in medications:")),
                              prettyRadioButtons("int_lipidlowering_med", "Start lipid-lowering medication",
                                                 c("No statin" = "none",
                                                   "Low intensity" = "low",
                                                   "Moderate intensity" = "mod",
                                                   "High intensity" = "high")),
                              prettyRadioButtons("int_bplowering_med", "Start new medication for blood pressure",
                                                 c("No new medication" = "none",
                                                   "One new medication" = "mono",
                                                   "2 or more new medications" = "combi")),
                              h5(strong("Smoking:")),
                              checkboxInput("int_stop_smoke", "Stop smoking", FALSE),
                              h5(strong("Dientary changes:")),
                              checkboxInput("int_salt_reduction", "Reduce 4.4g salt per day", FALSE),
                              checkboxInput("int_dash_diet", "Start DASH diet", FALSE),
                              checkboxInput("int_potassium_diet", "Increase dietary potassium to 3.5-5.0g per day", FALSE),
                              checkboxInput("int_mediterranean_diet", "Start Mediterranean diet", FALSE),
                              checkboxInput("int_grain_diet", "Increase whole grain intake by 100g per day", FALSE),
                              checkboxInput("int_plant_ster_diet", "Increase plant sterols/stanols 2g a day", FALSE),
                              prettyRadioButtons("int_diet_chol_red", "Reduce dietary cholesterol by:",
                                                 c("No reduction" = NA, "Over 500mg per day" = "low", "Over 900mg per day" = "high")),
                              checkboxInput("int_pufa_diet", "Increase polyunsaturated fat intake by 10% of energy consumption", FALSE),
                              checkboxInput("int_red_rice_diet", "Start red yeast rice 1.2 g per day (includes lovastatin)", FALSE),
                              h5(strong("Alcohol intake:")),
                              checkboxInput("int_alco_reduction", "Reduce alcohol intake to <2 (men) and <1 (women) drinks per day", FALSE),
                              h5(strong("Physical activity:")),
                              checkboxInput("int_pa_counceling", "Over 360 minute physical activity counselling", FALSE),
                              checkboxInput("int_pa_dyn_resist", "Dynamic resistance training (3 sessions per week)", FALSE),
                              checkboxInput("int_pa_endurance", "Aerobic endurance training (3 sessions per week)", FALSE),
                              checkboxInput("int_pa_isometric", "Isometric training (3 sessions per week)", FALSE),
                              prettyRadioButtons("int_weight_loss", strong("Weight loss"),
                                                 c("No weight loss" = "none",
                                                   "5kg loss" = "5kg",
                                                   "10kg loss" = "10kg",
                                                   "15kg loss" = "15kg",
                                                   "20kg loss" = "20kg")),
                              
                              br(), basicPage(actionButton("plotaction2", "Calculate risk", style="color:#FFFBFB; font-size:15px; background-color:#636363; border-color: #766F6E", width = "211px")) ),
                            
                            mainPanel(tabsetPanel(type = "tabs",
                                                  tabPanel("SCORE", plotOutput("scoreplot2", height = "500px")),
                                                  tabPanel("ASCVD", plotOutput("ascvdplot2", height = "500px"))))
                          )
                 )
)

############################################################################################################################################
############################################################################################################################################

server <- function(input, output, session) { # session included here
  
  # Function for calculating SCORE:
  calculate_score <- function(age, chol, sbp, smok, sex) {
    
    # Download weight file
    score_coef <- as.data.table(read_excel("score_coef.xlsx", sheet = sex))
    
    # CHD:
    alpha_chd <- score_coef$alpha[1]
    p_chd <- score_coef$p[1]
    bsmok_chd <- score_coef$bsmok[1]
    bchol_chd <- score_coef$bchol[1]
    bsbp_chd <- score_coef$bsbp[1]
    
    s0_chd <- exp( - (exp(alpha_chd)) * ((age-20)^p_chd)  )
    s0_10_chd <- exp( - (exp(alpha_chd)) * ((age-10)^p_chd)  )
    w_chd <- (bchol_chd*(chol-6)) + (bsbp_chd*(sbp-120)) + (bsmok_chd*(smok))
    
    s_chd <- s0_chd^exp(w_chd)
    s_10_chd <- s0_10_chd^exp(w_chd)
    
    div_chd <- s_10_chd/s_chd
    risk10_chd <- 1-div_chd
    
    # Non-CHD CVD:
    alpha_cvd <- score_coef$alpha[2]
    p_cvd <- score_coef$p[2]
    bsmok_cvd <- score_coef$bsmok[2]
    bchol_cvd <- score_coef$bchol[2]
    bsbp_cvd <- score_coef$bsbp[2]
    
    s0_cvd <- exp( - (exp(alpha_cvd)) * ((age-20)^p_cvd)  )
    s0_10_cvd <- exp( - (exp(alpha_cvd)) * ((age-10)^p_cvd)  )
    w_cvd <- (bchol_cvd*(chol-6)) + (bsbp_cvd*(sbp-120)) + (bsmok_cvd*(smok))
    
    s_cvd <- s0_cvd^exp(w_cvd)
    s_10_cvd <- s0_10_cvd^exp(w_cvd)
    
    div_cvd <- s_10_cvd/s_cvd
    risk10_cvd <- 1-div_cvd
    
    return( (risk10_chd + risk10_cvd) * 100 )
    
    rm(score_coef)
    
  }
  
  # Function for calculating ASCVD (PCE)
  calculate_ascvd <- function(age, chol, sbp, smok, sex, hdl, diab, bpmed) {
    
    hdl_ascvd <- hdl*38.67
    chol_ascvd <- chol*38.67
    
    # Download weight file
    ascvd_coef <- as.data.table(read_excel("ascvd_coef.xlsx", sheet = sex))
    
    if(bpmed == 0 & sex == "male") {
      ascvd_raw <- log(age)*ascvd_coef$ln_age+log(age)*log(age)*ascvd_coef$ln_age_squared+
        log(chol_ascvd)*ascvd_coef$ln_total_cholest+log(age)*log(chol_ascvd)*ascvd_coef$ln_age_totcholest+
        log(hdl_ascvd)*ascvd_coef$ln_hdlC+log(age)*log(hdl_ascvd)*ascvd_coef$ln_age_hdlC+
        smok*ascvd_coef$smoker+smok*log(age)*ascvd_coef$ln_age_smoker+
        log(sbp)*ascvd_coef$ln_untreated_BP+
        log(sbp)*log(age)*ascvd_coef$ln_age_ln_untreated_BP+
        diab*ascvd_coef$diabetes
      
    } else if(bpmed == 0 & sex == "female") {
      ascvd_raw <- log(age)*ascvd_coef$ln_age+log(age)*log(age)*ascvd_coef$ln_age_squared+
        log(chol_ascvd)*ascvd_coef$ln_total_cholest+log(age)*log(chol_ascvd)*ascvd_coef$ln_age_totcholest+
        log(hdl_ascvd)*ascvd_coef$ln_hdlC+log(age)*log(hdl_ascvd)*ascvd_coef$ln_age_hdlC+
        smok*ascvd_coef$smoker+smok*log(age)*ascvd_coef$ln_age_smoker+
        log(sbp)*ascvd_coef$ln_untreated_BP+
        log(sbp)*log(age)*ascvd_coef$ln_age_ln_untreated_BP+
        diab*ascvd_coef$diabetes
      
    } else if(bpmed == 1 & sex == "male") {
      ascvd_raw <- log(age)*ascvd_coef$ln_age+log(age)*log(age)*ascvd_coef$ln_age_squared+
        log(chol_ascvd)*ascvd_coef$ln_total_cholest+log(age)*log(chol_ascvd)*ascvd_coef$ln_age_totcholest+
        log(hdl_ascvd)*ascvd_coef$ln_hdlC+log(age)*log(hdl_ascvd)*ascvd_coef$ln_age_hdlC+
        smok*ascvd_coef$smoker+smok*log(age)*ascvd_coef$ln_age_smoker+
        log(sbp)*ascvd_coef$ln_treated_BP+
        log(sbp)*log(age)*ascvd_coef$ln_age_BP+
        diab*ascvd_coef$diabetes
      
    } else if(bpmed == 1 & sex == "female") {
      ascvd_raw <- log(age)*ascvd_coef$ln_age+log(age)*log(age)*ascvd_coef$ln_age_squared+
        log(chol_ascvd)*ascvd_coef$ln_total_cholest+log(age)*log(chol_ascvd)*ascvd_coef$ln_age_totcholest+
        log(hdl_ascvd)*ascvd_coef$ln_hdlC+log(age)*log(hdl_ascvd)*ascvd_coef$ln_age_hdlC+
        smok*ascvd_coef$smoker+smok*log(age)*ascvd_coef$ln_age_smoker+
        log(sbp)*ascvd_coef$ln_treated_BP+
        log(sbp)*log(age)*ascvd_coef$ln_age_BP+
        diab*ascvd_coef$diabetes
    } else {
      print("")
    }
    
    ascvd <- 100 * ( 1 - (ascvd_coef$baseline^exp(ascvd_raw-ascvd_coef$meancoef)) )
    return(ascvd)
    
  }
  
  
  # Functions for calculating change in life years:
  calculate_lifeyears_ascvd <- function(age0 = age0, difference = difference) {
    
    baseline_survival <- (-0.0000009439969)*age0^4 + (0.0001759848)*age0^3 + (-0.0117548)*age0^2 + (0.3232857)*age0 + (-1.986107)
    beta <- (-0.000003047355)*age0^3 + (0.0007145848)*age0^2 + (-0.05448585)*age0 + 1.391871
    
    # Integrate survival under no change and change, and calculate the difference (life years)
    
    ps1 <-function(x) {((baseline_survival))^exp(beta*0)+0*x}
    is1 <- integrate(ps1, lower = age0, upper = 90)
    is1
    
    ps2 <- function(x) {((baseline_survival))^exp(beta*difference)+0*x}
    is2 <- integrate(ps2, lower = age0, upper = 90)
    is2
    
    lifeyears = is2$value - is1$value
    
    return(list("lifeyears" = lifeyears, "is1" = is1$value, "is2" = is2$value))
    
  }
  
  calculate_lifeyears_score <- function(age0 = age0, difference = difference) {
    
    baseline_survival <- (-0.00002201939)*age0^3 + (0.00372206)*age0^2 + (-0.2130739)*age0 + 4.971726
    beta <- (0.0001579506)*age0^2 + (-0.0224293626)*age0 + 0.8253894332
    
    # Integrate survival under no change and change, and calculate the difference (life years)
    
    ps1 <-function(x) {((baseline_survival))^exp(beta*0)+0*x}
    is1 <- integrate(ps1, lower = age0, upper = 90)
    is1
    
    ps2 <- function(x) {((baseline_survival))^exp(beta*difference)+0*x}
    is2 <- integrate(ps2, lower = age0, upper = 90)
    is2
    
    lifeyears = is2$value - is1$value
    
    return(list("lifeyears" = lifeyears, "is1" = is1$value, "is2" = is2$value))
    
  }
  
  
  # Function for plotting risks:
  plot_firstsecond <- function(type = type, first = first, second = second, second_onlyage = second_onlyage, ly = ly,
                               free_of_cvd_withchange = free_of_cvd_withchange, free_of_cvd_withoutchange = free_of_cvd_withoutchange,
                               text_xleft = text_xleft, text_xmiddle = text_xmiddle, text_xright = text_xright,
                               text_annotate1 = text_annotate1, text_annotate2 = text_annotate2, text_annotate3 = text_annotate3) {
    
    risks_names_barplot1 <- c(text_xmiddle, text_xleft)
    risks_barplot1 <- c(second_onlyage, first)
    
    risks_names_barplot2 <- c(text_xright)
    risks_barplot2 <- c(second)
    
    if(type == "score") {
      xlabel <-  "10-year risk of death due to\ncardiovascular disease (%)\n"
    } else if(type == "ascvd") {
      xlabel <-  "10-year risk of\ncardiovascular disease (%)\n"
    }
    
    p_barplot1 <- ggplot(data = data.frame(risks_names_barplot1, risks_barplot1), aes(y = risks_names_barplot1, x = risks_barplot1))+
      geom_bar(stat = "identity", width = 0.5, fill = c("#4350BF", "#3E3B88"))+
      theme_classic()+ labs(y = "", x = xlabel )+ theme(text = element_text(family = "Open Sans", size = 13))+
      theme(axis.text.x = element_text(size = 12, margin = margin(10,0,0,0)))+
      coord_flip(expand = c(0, 0), ylim = c(0.5,2.5), xlim = c(0, ifelse(second_onlyage < 15, 20, ifelse(second_onlyage > 50, 100, 50))))+
      geom_text(aes(label = paste0(round(risks_barplot1, 1), "%"), vjust = -1.2, size = 20, family = "Open Sans Bold"))+
      theme(legend.position = "none")
    
    p_barplot2 <- ggplot(data = data.frame(risks_names_barplot2, risks_barplot2), aes(y = risks_names_barplot2, x = risks_barplot2))+
      geom_bar(stat = "identity", width = 0.35, fill = c("#D62E35"))+
      theme_classic()+ labs(y = "", x = "")+ theme(text = element_text(family = "Open Sans", size = 13))+
      theme(axis.text.x = element_text(size = 12, margin = margin(10,0,0,0)))+
      coord_flip(expand = c(0, 0), ylim = c(0.5,1.5), xlim = c(0, ifelse(second_onlyage < 15, 20,
                                                                         ifelse(second_onlyage > 50, 100, 50))))+
      geom_text(aes(label = paste0(round(risks_barplot2, 1), "%"), vjust = -1.2, size = 20, family = "Open Sans Bold"))+
      theme(legend.position = "none")
    
    p_bottomplot <- ggplot()+geom_blank()+theme_void()+coord_cartesian(xlim = c(0,1), ylim = c(0,1))+
      annotate(geom = "text", x = 0.5, y = 0.8, size = 5.7,
               label = paste0(text_annotate1, ly),
               color = "#141837", family = "Open Sans Bold")+
      annotate(geom = "text", x = 0.5, y = 0.40, size = 5.7,
               label = paste0(text_annotate2,
                              round(free_of_cvd_withchange, 1),
                              text_annotate3,
                              round(free_of_cvd_withoutchange, 1)),
               color = "#141837", family = "Open Sans Bold")
    
    plot(ggarrange(ggarrange(p_barplot1, p_barplot2, widths = c(1.4,1))+theme(plot.margin = margin(1,0.5,0,0.5, "cm")),
                   p_bottomplot, nrow = 2, heights = c(1.5,1)))
    
  }

  observeEvent(input$plotaction1, {
    
    # Isolate variables so that nothing is estimated before all values are filled    
    
    sex <- isolate(input$sex)
    
    # Baseline risk factors
    age0 <- as.numeric(isolate(input$age0))
    chol0 <- as.numeric(isolate(input$chol0))
    sbp0 <- as.numeric(isolate(input$sbp0))
    smok0 <- as.numeric(isolate(input$smok0))
    hdl0 <- as.numeric(isolate(input$hdl0))
    bpmed0 <- as.numeric(isolate(input$bpmed0))
    diab0 <- as.numeric(isolate(input$diab0))
    
    # Follow-up risk factors
    age1 <- as.numeric(isolate(input$age1))
    chol1 <- as.numeric(isolate(input$chol1))
    sbp1 <- as.numeric(isolate(input$sbp1))
    smok1 <- as.numeric(isolate(input$smok1))
    hdl1 <- as.numeric(isolate(input$hdl1))
    bpmed1 <- as.numeric(isolate(input$bpmed1))
    diab1 <- as.numeric(isolate(input$diab1))
    
    # Calculate SCORE:
    score_first <- calculate_score(age = age0, chol = chol0, sbp = sbp0, smok = smok0, sex = sex)
    score_second <- calculate_score(age = age1, chol = chol1, sbp = sbp1, smok = smok1, sex = sex)
    score_second_only_age_changes <- calculate_score(age = age1, chol = chol0, sbp = sbp0, smok = smok0, sex = sex)
    score_difference <- score_second - score_first
    score_lifeyears <- calculate_lifeyears_score(age0 = age0, difference = score_difference)$lifeyears
    score_is1 <- calculate_lifeyears_score(age0 = age0, difference = score_difference)$is1
    score_is2 <- calculate_lifeyears_score(age0 = age0, difference = score_difference)$is2
    score_free_of_cvd_withchange <- age0+score_is1
    score_free_of_cvd_withoutchange <- age0+score_is2
    
    # Calculate ASCVD:
    ascvd_first <- calculate_ascvd(age = age0, chol = chol0, sbp = sbp0, smok = smok0, sex = sex, hdl = hdl0, bpmed = bpmed0, diab = diab0)
    ascvd_second <- calculate_ascvd(age = age1, chol = chol1, sbp = sbp1, smok = smok1, sex = sex, hdl = hdl1, bpmed = bpmed1, diab = diab1)
    ascvd_second_only_age_changes <- calculate_ascvd(age = age1, chol = chol0, sbp = sbp0, smok = smok0, sex = sex, hdl = hdl0, bpmed = bpmed0, diab = diab0)
    ascvd_difference <- ascvd_second - ascvd_first
    ascvd_lifeyears <- calculate_lifeyears_ascvd(age0 = age0, difference = ascvd_difference)$lifeyears
    ascvd_is1 <- calculate_lifeyears_ascvd(age0 = age0, difference = ascvd_difference)$is1
    ascvd_is2 <- calculate_lifeyears_ascvd(age0 = age0, difference = ascvd_difference)$is2
    
    ascvd_free_of_cvd_withchange <- age0+ascvd_is1
    ascvd_free_of_cvd_withoutchange <- age0+ascvd_is2
    
    # Plot labels:
    text_xleft <- "Previous\nvisit\n"
    text_xmiddle <- "Previous visit with\nage at current visit\n"
    text_xright <- "Current\nvisit\n" 
    
    text_annotate1 <- "Change in CVD-free life years between visits: "
    text_annotate2 <- "CVD-free life expectancy (years)\nPrevious visit: "
    text_annotate3 <- "\nCurrent visit: "
    
    
    output$scoreplot1 <- renderPlot({
      
      plot_firstsecond(type = "score", first = score_first, second = score_second,
                       second_onlyage = score_second_only_age_changes, ly = round(score_lifeyears, 1),
                       free_of_cvd_withchange = score_free_of_cvd_withchange,
                       free_of_cvd_withoutchange = score_free_of_cvd_withoutchange,
                       text_xleft = text_xleft, text_xmiddle = text_xmiddle, text_xright = text_xright,
                       text_annotate1 = text_annotate1, text_annotate2 = text_annotate2, text_annotate3 = text_annotate3)
      
    })
    
    output$ascvdplot1 <- renderPlot({
      
      plot_firstsecond(type = "ascvd", first = ascvd_first, second = ascvd_second,
                       second_onlyage = ascvd_second_only_age_changes, ly = round(ascvd_lifeyears, 1),
                       free_of_cvd_withchange = ascvd_free_of_cvd_withchange,
                       free_of_cvd_withoutchange = ascvd_free_of_cvd_withoutchange,
                       text_xleft = text_xleft, text_xmiddle = text_xmiddle, text_xright = text_xright,
                       text_annotate1 = text_annotate1, text_annotate2 = text_annotate2, text_annotate3 = text_annotate3)
      
    })
    
    
    
  })
  
  
  observeEvent(input$plotaction2, {
    
    sex_lcc <- isolate(input$sex_lcc)
    age_lcc <- as.numeric(isolate(input$age_lcc))
    chol_lcc <- as.numeric(isolate(input$chol_lcc))
    sbp_lcc <- as.numeric(isolate(input$sbp_lcc))
    smok_lcc <- as.numeric(isolate(input$smok_lcc))
    hdl_lcc <- as.numeric(isolate(input$hdl_lcc))
    bpmed_lcc <- as.numeric(isolate(input$bpmed_lcc))
    diab_lcc <- as.numeric(isolate(input$diab_lcc))
    time_goal <- as.numeric(isolate(input$time_goal))
    
    # Inputs of interventions:
    
    #Medications
    int_lipidlowering_med <- isolate(input$int_lipidlowering_med)
    int_bplowering_med <- isolate(input$int_bplowering_med)
    
    #Stop smoking
    int_stop_smoke <- isolate(input$int_stop_smoke)
    
    #Diet
    int_salt_reduction <- isolate(input$int_salt_reduction)
    int_dash_diet <- isolate(input$int_dash_diet)
    
    int_potassium_diet <- isolate(input$int_potassium_diet)
    int_mediterranean_diet <- isolate(input$int_mediterranean_diet)
    
    int_grain_diet <- isolate(input$int_grain_diet)
    int_plant_ster_diet <- isolate(input$int_plant_ster_diet)
    
    int_diet_chol_red <- isolate(input$int_diet_chol_red)
    
    int_pufa_diet <- isolate(input$int_pufa_diet)
    int_red_rice_diet <- isolate(input$int_red_rice_diet)
    
    #Alcohol reduction
    int_alco_reduction <- isolate(input$int_alco_reduction)
    
    #Physical Activity
    int_pa_counceling <- isolate(input$int_pa_counceling)
    int_pa_dyn_resist <- isolate(input$int_pa_dyn_resist)
    int_pa_endurance <- isolate(input$int_pa_endurance)
    int_pa_isometric <- isolate(input$int_pa_isometric)
    
    #Weight loss
    int_weight_loss <- isolate(input$int_weight_loss)
    
    
    ######################################################
    
    # Calculate baseline risks: 
    score_baseline <- calculate_score(age = age_lcc, chol = chol_lcc, sbp = sbp_lcc, smok = smok_lcc, sex = sex_lcc)
    ascvd_baseline <- calculate_ascvd(age = age_lcc, chol = chol_lcc, sbp = sbp_lcc, smok = smok_lcc, sex = sex_lcc,
                                      hdl = hdl_lcc, bpmed = bpmed_lcc, diab = diab_lcc)
    print(ascvd_baseline)
    
    # Calculate risk with only age changing:
    score_change_only_age_changed <- calculate_score(age = age_lcc+time_goal, chol = chol_lcc, sbp = sbp_lcc, smok = smok_lcc, sex = sex_lcc)
    ascvd_change_only_age_changed <- calculate_ascvd(age = age_lcc+time_goal, chol = chol_lcc, sbp = sbp_lcc, smok = smok_lcc, sex = sex_lcc,
                                                     hdl = hdl_lcc, bpmed = bpmed_lcc, diab = diab_lcc)
    
    ######################################################
    
    # Change risk factors based onselected interventions
    
    
    # Medications
    
    if(int_lipidlowering_med == "low") { 
      chol_lcc = chol_lcc*0.895 
    } else if(int_lipidlowering_med == "mod") {
      chol_lcc = chol_lcc*0.79
    } else if(int_lipidlowering_med == "high") {
      chol_lcc = chol_lcc*0.65
    } else {
      chol_lcc = chol_lcc
    }
    
    if(int_bplowering_med == "mono") { 
      sbp_lcc =  sbp_lcc = sbp_lcc - 10  
    } else if(int_bplowering_med == "combi") {
      sbp_lcc = sbp_lcc = sbp_lcc - 20 
    } else {
      sbp_lcc = sbp_lcc
    }
    
    
    # Diet
    if(int_salt_reduction == T) { sbp_lcc = sbp_lcc - 5.4 }
    if(int_dash_diet == T) { sbp_lcc = sbp_lcc - 11.4; chol_lcc = chol_lcc - 0.2}
    
    if(int_potassium_diet == T) { sbp_lcc = sbp_lcc - 5.3 }
    if(int_mediterranean_diet == T) { sbp_lcc = sbp_lcc - 2.6; chol_lcc = chol_lcc - 0.4 }
    
    if(int_grain_diet == T) { chol_lcc = chol_lcc - 0.2 }
    if(int_plant_ster_diet == T) { chol_lcc = chol_lcc - 0.4 }
    
    if(int_diet_chol_red == "low") { 
      chol_lcc =  chol_lcc = chol_lcc - 0.3  
    } else if(int_bplowering_med == "high") {
      chol_lcc = chol_lcc = chol_lcc - 0.5 
    } else {
      chol_lcc = chol_lcc
    }
    
    if(int_pufa_diet == T) { chol_lcc = chol_lcc - 0.8 }
    if(int_red_rice_diet == T) { chol_lcc = chol_lcc - 1.0 }
    
    # Alcohol reduction
    if(int_alco_reduction == T) { sbp_lcc = sbp_lcc - 3.1 } 
    
    # Physical Activity
    if(int_pa_counceling == T) { sbp_lcc = sbp_lcc - 1.6; chol_lcc = chol_lcc - 0.1 }
    if(int_pa_dyn_resist == T) { sbp_lcc = sbp_lcc - 4.3; chol_lcc = chol_lcc - 0.1 }
    if(int_pa_endurance == T) { sbp_lcc = sbp_lcc - 8.3; chol_lcc = chol_lcc - 0.1 }
    if(int_pa_isometric == T) { sbp_lcc = sbp_lcc - 5.2; chol_lcc = chol_lcc - 0.1 }
    
    # Weight loss
    if(int_weight_loss == "5kg") { 
      chol_lcc = chol_lcc - 0.5
      sbp_lcc = sbp_lcc - 5.5
    } else if(int_weight_loss == "10kg") {
      chol_lcc = chol_lcc - (0.5*2)
      sbp_lcc = sbp_lcc - (5.5*2)
    } else if(int_weight_loss == "15kg") {
      chol_lcc = chol_lcc - (0.5*3)
      sbp_lcc = sbp_lcc - (5.5*3)
    } else if(int_weight_loss == "20kg") {
      chol_lcc = chol_lcc - (0.5*4)
      sbp_lcc = sbp_lcc - (5.5*4)
    } else {
      chol_lcc = chol_lcc 
      sbp_lcc = sbp_lcc 
    }
    
    ######################################################
    
    # Calculate risks with interventions
    
    # Also age increases with the 5-year interval (5-year interval used in Lindbohm et al.):
    
    # SCORE:
    score_changed <- calculate_score(age = age_lcc+time_goal, chol = chol_lcc, sbp = sbp_lcc, smok = smok_lcc, sex = sex_lcc)
    if(int_stop_smoke == T) { score_changed = 0.6*score_changed }
    score_difference <- score_changed-score_baseline
    
    score_lifeyears <- calculate_lifeyears_score(age0 = age_lcc, difference = score_difference)$lifeyears
    score_is1 <- calculate_lifeyears_score(age0 = age_lcc, difference = score_difference)$is1
    score_is2 <- calculate_lifeyears_score(age0 = age_lcc, difference = score_difference)$is2
    
    score_free_of_cvd_withchange <- age_lcc+score_is1
    score_free_of_cvd_withoutchange <- age_lcc+score_is2
    
    # ASCVD:
    ascvd_changed <- calculate_ascvd(age = age_lcc+time_goal, chol = chol_lcc, sbp = sbp_lcc, smok = smok_lcc, sex = sex_lcc,
                                     hdl = hdl_lcc, bpmed = bpmed_lcc, diab = diab_lcc)
    if(int_stop_smoke == T) { ascvd_changed = 0.6*ascvd_changed }
    ascvd_difference <- ascvd_changed-ascvd_baseline
    
    ascvd_lifeyears <- calculate_lifeyears_ascvd(age0 = age_lcc, difference = ascvd_difference)$lifeyears
    ascvd_is1 <- calculate_lifeyears_ascvd(age0 = age_lcc, difference = ascvd_difference)$is1
    ascvd_is2 <- calculate_lifeyears_ascvd(age0 = age_lcc, difference = ascvd_difference)$is2
    
    ascvd_free_of_cvd_withchange <- age_lcc+ascvd_is1
    ascvd_free_of_cvd_withoutchange <- age_lcc+ascvd_is2
    
    # Plot labels:
    text_xleft <- "Current\nvisit\n"
    text_xmiddle <- "Next visit with\nno intervention\n"
    text_xright <- "Next visit with\nintervention\n"
    
    text_annotate1 <- "Change in CVD-free life years by next visit: "
    text_annotate2 <- "CVD-free life expectancy (years)\nCurrent visit: "
    text_annotate3 <- "\nNext visit: "
    
    ######################################################
    
    output$scoreplot2 <- renderPlot({
      
      plot_firstsecond(type = "score", first = score_baseline, second = score_changed,
                       second_onlyage = score_change_only_age_changed, ly = round(score_lifeyears, 1),
                       free_of_cvd_withchange = score_free_of_cvd_withchange,
                       free_of_cvd_withoutchange = score_free_of_cvd_withoutchange,
                       text_xleft = text_xleft, text_xmiddle = text_xmiddle, text_xright = text_xright,
                       text_annotate1 = text_annotate1, text_annotate2 = text_annotate2, text_annotate3 = text_annotate3)
      
    })
    
    output$ascvdplot2 <- renderPlot({
      
      plot_firstsecond(type = "ascvd", first = ascvd_baseline, second = ascvd_changed,
                       second_onlyage = ascvd_change_only_age_changed, ly = round(ascvd_lifeyears, 1),
                       free_of_cvd_withchange = ascvd_free_of_cvd_withchange,
                       free_of_cvd_withoutchange = ascvd_free_of_cvd_withoutchange,
                       text_xleft = text_xleft, text_xmiddle = text_xmiddle, text_xright = text_xright,
                       text_annotate1 = text_annotate1, text_annotate2 = text_annotate2, text_annotate3 = text_annotate3)
      
    })
    
    
  })
  
}


# Run the app 
shinyApp(ui = ui, server = server)

# -----------------------------------------------------------------------



