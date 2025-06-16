#   Load libraries
library(dplyr)      
library(ggplot2)    
library(survey)     
library(mitools)    
library(kableExtra) 
library(mice)   
library(tidyverse)
library(stringmagic)
library(dreamerr)
library(fixest)
library(srvyr)
library(haven)
library(Hmisc)
library(sjPlot)
library(labelled)
library(flextable)
library(gt)
library(modelsummary)

#                      01_data_cleaning

#   Load & prepare MICS data files 

#- Load MICS data files

mics3_soml <- readRDS("Data/mics3_final_backup.rds")
mics4_soml <- readRDS("Data/mics4_final_backup.rds")
mics5_soml <- readRDS("Data/mics5_final_backup.rds")


#   Make the datasets for parallel trends analysis and DiD
mics_soml_stack_bcg <- bind_rows(mics3_soml, mics4_soml, mics5_soml)


#- Make core DiD variables


mics_soml_stack_bcg <- mics_soml_stack_bcg %>%
  mutate(
    treated = adopter_type_bin, 
    post = if_else(year >= 2012, 1, 0), 
    did = treated * post
  )

#                    02_imputation

#- Select variables for model & imputation

mics_imp_bcg <- mics_soml_stack_bcg %>%
  select(bcg, education_mom, wealth_quintile,
         cluster_id, year, survey_weightwm, state, rural_urban_bin,
         mum_agecat, adopter_type_bin, treated, post, did)

#     Multiple imputation

#- MICE to handle missing values

imp_bcg <- mice(mics_imp_bcg, m = 5, method = "pmm", seed = 123, printFlag = FALSE)

#   All imputed datasets into a list 
imp_list_bcg <- complete(imp_bcg, "all")

#- Defining survey design for each imputed dataset

survey_designs_bcg <- lapply(imp_list_bcg, function(df) {
  df %>%
    svydesign(
      id = ~cluster_id,       
      strata = ~state,        
      weights = ~survey_weightwm, 
      data = .,                 
      nest = TRUE           
    )
})

#     Visualise parallel trend check

#- Set parallel trend check

#   Use one (first one) complete imputed dataset

completed_data_bcg_imp1 <- complete(imp_bcg, 1)

#    Mean bcg coverage for plotting by treatment group and year
bcg_summary_plot <- completed_data_bcg_imp1 %>%
  group_by(treated, year) %>%
  summarise(
    mean_bcg = mean(bcg, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    treated_label = if_else(treated == 1, "Treated States (SOML Adopters)", "Control States"),
    year_label = as.character(year) 
  )

#- Box plot - mean coverage for each group in each year

ggplot(bcg_summary_plot, aes(x = year_label, y = mean_bcg, fill = treated_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", mean_bcg)),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
  labs(
    title = "bcg Coverage by Treatment Group and Year",
    subtitle = "Years: 2007 (Pre), 2011 (Pre), 2016 (Post-SOML 2012 Launch)", 
    x = "Year",
    y = "Mean bcg Coverage (0-1 scale)",
    fill = "Group"
  ) +
  scale_y_continuous(limits = c(0, max(bcg_summary_plot$mean_bcg) * 1.1)) +
  theme_minimal() +
  theme(legend.position = "bottom")

#   To save box plots/figure --------------------

p <- ggplot(bcg_summary_plot, aes(x = year_label, y = mean_bcg, fill = treated_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", mean_bcg)),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
  labs(
    title = "Mean BCG Coverage by Treatment Group and Year",
    subtitle = "Years: 2007 (Pre), 2011 (Pre), 2016 (Post-SOML 2012 Launch)", 
    x = "Year",
    y = "Mean BCG Equivalent Coverage (0-1 scale)",
    fill = "Group"
  ) +
  scale_y_continuous(limits = c(0, max(bcg_summary_plot$mean_bcg) * 1.1)) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save table in multiple formats
ggsave("figures/year_label_vs_mean_bcg1.png", plot = p, width = 8, height = 6, dpi = 300)
ggsave("figures/year_label_vs_mean_bcg1.pdf", plot = p, width = 8, height = 6)


#- Line plot - trend of coverage over time for both groups

ggplot(bcg_summary_plot, aes(x = year_label, y = mean_bcg, color = treated_label, group = treated_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  
  geom_vline(xintercept = "2011.5", linetype = "dashed", color = "grey50", size = 0.8) + 
  annotate("text", x = "2011.5", y = max(bcg_summary_plot$mean_bcg) * 0.95,
           label = "SOML Launch (2012)", hjust = -0.1, vjust = -0.5, size = 3.5, color = "grey50") +
  labs(
    title = "Trend of bcg Coverage by Treatment Group",
    subtitle = "Parallel Trends Check (2007-2011 Pre-Period) and Post-SOML Trends", 
    x = "Year",
    y = "Mean bcg coverage (0-1 scale)",
    color = "Group"
  ) +
  scale_y_continuous(limits = c(0, max(bcg_summary_plot$mean_bcg) * 1.1)) +
  theme_minimal() +
  theme(legend.position = "bottom")

#    To save line plots/figure --------------------

q <- ggplot(bcg_summary_plot, aes(x = year_label, y = mean_bcg, color = treated_label, group = treated_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  
  geom_vline(xintercept = "2011.5", linetype = "dashed", color = "grey50", size = 0.8) + 
  annotate("text", x = "2011.5", y = max(bcg_summary_plot$mean_bcg) * 0.95,
           label = "SOML Launch (2012)", hjust = -0.1, vjust = -0.5, size = 3.5, color = "grey50") +
  labs(
    title = "Trend of BCG Coverage by Treatment Group",
    subtitle = "Parallel Trends Check (2007-2011 Pre-Period) and Post-SOML Trends", 
    x = "Year",
    y = "Mean BCG Coverage (0-1 scale)",
    color = "Group"
  ) +
  scale_y_continuous(limits = c(0, max(bcg_summary_plot$mean_bcg) * 1.1)) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save fig in multiple formats
ggsave("figures/year_label_vs_mean_bcg2.png", plot = q, width = 8, height = 6, dpi = 300)
ggsave("figures/year_label_vs_mean_bcg2.pdf", plot = q, width = 8, height = 6)

#                   03_did_analysis.R

# Difference-in-Differences model

#- Fit DiD model

did_models_bcg <- lapply(survey_designs_bcg, function(design) {
  svyglm(
    bcg ~ treated * post + rural_urban_bin + mum_agecat + 
      education_mom + wealth_quintile,
    design = design,
    family = quasipoisson()  
  )
})

#- Pool and format results table

library(survey)
did_pooled_bcg <- MIcombine(did_models_bcg)   


#- Exponentiated Coefficients

bcg_table <- data.frame(
  Term = rownames(confint(did_pooled_bcg)),
  IRR = round(exp(coef(did_pooled_bcg)), 4), 
  Std.Error = round(sqrt(diag(did_pooled_bcg$variance)), 4),
  `CI 2.5%` = round(exp(confint(did_pooled_bcg)[, 1]), 4),
  `CI 97.5%` = round(exp(confint(did_pooled_bcg)[, 2]), 4),
  `P-value` = round(2 * pnorm(abs(coef(did_pooled_bcg)/sqrt(diag(did_pooled_bcg$variance))), lower.tail = FALSE), 4),
  check.names = FALSE
) %>%
  mutate(
    Significance = case_when(
      `P-value` < 0.001 ~ "***",
      `P-value` < 0.01 ~ "**",
      `P-value` < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Term, IRR, `CI 2.5%`, `CI 97.5%`, `P-value`, Significance)

#- Name and clarify the terms 

bcg_table <- bcg_table %>%
  mutate(
    Term = case_when(
      Term == "(Intercept)" ~ "Intercept",
      Term == "treated" ~ "Treated Group (Pre-SOML)",
      Term == "post" ~ "Post-SOML Period",
      Term == "treated:post" ~ "DiD Estimate (SOML Effect)",
      Term == "rural_urban_bin" ~ "Rural (Ref: Urban)",
      Term == "mum_agecat" ~ "Mother's Age (Binary)",
      Term == "education_mom" ~ "Mother's Education (Binary)",
      Term == "wealth_quintile" ~ "Wealth Quintile (Binary)",
      TRUE ~ Term
    )
  )
#print(did_pooled_bcg) 

bcg_table <- bcg_table %>%
  mutate(Interpretation = case_when(
    IRR > 1 & `P-value` < 0.05 ~ "Significant increase",
    IRR < 1 & `P-value` < 0.05 ~ "Significant decrease",
    TRUE ~ "No significant effect"
  ))

bcg_table %>%
  kbl(caption = "Difference-in-Differences Analysis for bcg Vaccination Coverage") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE)

#         Formatted table -----------------------

library(flextable)
library(magrittr)

ft <- flextable(bcg_table) %>% 
  autofit() %>%
  theme_zebra()

# Save as image/HTML
save_as_image(ft, path = "tables/did_results_table.png")
save_as_html(ft, path = "tables/did_results_table.html")
save_as_html(ft, path = "tables/did_results_table.pdf")

#                    04_visualization

#- Plotting Incident Rate Ratios (IRRs)

library(ggplot2)

did_irr <- data.frame(
  Term = "SOML Intervention (DiD)",
  IRR = exp(did_pooled_bcg$coefficients["treated:post"]),
  CI_low = exp(confint(did_pooled_bcg)["treated:post", 1]),
  CI_high = exp(confint(did_pooled_bcg)["treated:post", 2])
)

# Create forest plot
ggplot(did_irr, aes(x = Term, y = IRR, ymin = CI_low, ymax = CI_high)) +
  geom_pointrange(size = 1, color = "#4B9DA3") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Impact of SOML on bcg Doses (Incidence Rate Ratio)",
    subtitle = "IRR > 1 indicates increased doses post-intervention",
    x = "",
    y = "IRR with 95% CI"
  ) +
  coord_flip() +  # Horizontal plot
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

#    To save line plots/figure -----------------

ps <- ggplot(did_irr, aes(x = Term, y = IRR, ymin = CI_low, ymax = CI_high)) +
  geom_pointrange(size = 1, color = "#4B9DA3") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Impact of SOML on BCG Doses (Incidence Rate Ratio)",
    subtitle = "IRR > 1 indicates increased doses post-intervention",
    x = "",
    y = "IRR with 95% CI"
  ) +
  coord_flip() + 
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


# Save fig in multiple formats
ggsave("figures/Term_vs_IRR.png", plot = p, width = 8, height = 6, dpi = 300)
ggsave("figures/Term_vs_IRR.pdf", plot = p, width = 8, height = 6)







