library(tidyverse)
library(rms)
library(gtsummary)
library(dcurves)
library(gt)



# Load data
options(prType='html')
Hmisc::getHdata(acath)

# drop those without cholesterol and setup sex variable
acath <- subset(acath, !is.na(choleste))

# pre model (age and sex)
pre <- lrm(sigdz ~ rcs(age,4) * sex, data = acath)
pre_pred <- predict(pre, type='fitted')

# post model (age, sex and cholesterol)
post <- lrm(sigdz ~ rcs(age,4) * sex + rcs(choleste,4) + rcs(age,4) %ia%
              rcs(choleste,4), data = acath)
post_pred <- predict(post, type='fitted')

# combine predictions with original data
acath_pred <-
  bind_cols(
    acath,
    pre_pred %>% enframe(name = NULL, value = "pre"),
    post_pred %>% enframe(name = NULL, value = "post")
  )

# decision curve
dca_prepost <-
  dca(
    sigdz ~ pre + post,
    data = acath_pred,
    label = list(pre = "Age and Sex", 
                 post = "Age, Sex and Cholesterol"), 
  )

dca_prepost_smooth <-
  dca_prepost %>%
  plot(smooth = TRUE)

dca_prepost_15_35 <-
  dcurves::dca(
    sigdz ~ pre + post,
    data = acath_pred,
    thresholds = seq(0.15, 0.35, by = 0.05),
    label = list(pre = "Age and Sex", post = "Age, Sex and Cholesterol"),
    # show_ggplot_code = TRUE
  ) %>%
  plot(type = 'net_benefit', 
       smooth = FALSE, 
       show_ggplot_code = FALSE)

# net interventions avoided
dca_prepost_netint_ob <- dca_prepost %>%
  dcurves::net_intervention_avoided() 

dca_prepost_netint <- dca_prepost_netint_ob %>% 
  plot(x, 
       type = 'net_intervention_avoided', 
       smooth = FALSE, 
       show_ggplot_code = FALSE)

# estimates table
df_dca_tbl <-
  dca_prepost_netint_ob$dca %>%
  filter(variable %in% c("pre", "post")) %>%
  # keep 5-50%, by 5%
  transmute(
    threshold100 = threshold*100,
    groupvar = factor(variable, levels = c("pre", "post")),
    net_benefit, net_intervention_avoided
  ) %>%
  filter(threshold100 > 1 & threshold100 <= 50 & round(threshold100 %% 5) == 0) %>%
  mutate(threshold100 = factor(str_glue("{threshold100}%"),
                               levels = c("5%", "10%", "15%", "20%", "25%",
                                          "30%", "35%", "40%", "45%", "50%")))

# create gtsummary table to start so gtsummary themes can be used
dca_tbl_continuous <-
  tbl_continuous(
    data = df_dca_tbl,
    variable = net_benefit,
    include = threshold100,
    by = groupvar,
    statistic = list(everything() ~ "{median}"),
    digits = list(everything() ~ 4)
  ) %>%
  modify_footnote(everything() ~ NA) %>%
  modify_header(update = list(
    label ~ "**Threshold**",
    stat_1 ~ "**Net Benefit (Age+Sex)**",
    stat_2 ~ "**Net Benefit (Age+Sex+Cholesterol)**"
  ))

df_diff_tbl <-
  df_dca_tbl %>%
  # First, reshape two columns to long
  pivot_longer(
    cols = c(starts_with("net_"))
  ) %>%
  # Create name variable
  unite(name, c(groupvar, name), sep = "_") %>%
  # Reshape to wide to get difference
  pivot_wider(
    names_from = "name",
    values_from = "value"
  ) %>%
  # Calculate differences
  transmute(
    threshold100,
    diff_net_benefit = post_net_benefit - pre_net_benefit,
    diff_net_intavoid = post_net_intervention_avoided - pre_net_intervention_avoided
  )

# Merge in with continuous table
estimate_tbl <-
  dca_tbl_continuous %>%
  modify_table_body(
    ~ .x %>%
      left_join(
        df_diff_tbl,
        by = c("label" = "threshold100")
      ) %>%
      filter(row_type != "label")
  ) %>%
  modify_header(
    diff_net_benefit = "**Difference in Net Benefit**",
    diff_net_intavoid = "**Net Difference in Interventions Avoided per 100 patients**"
  ) %>%
  modify_fmt_fun(c(diff_net_benefit, diff_net_intavoid) ~ function(x) { style_sigfig(x, digits = 4)})



dca_prepost_smooth


dca_prepost_15_35


dca_prepost_netint


estimate_tbl



# rtichoke code

performance_data_dc <- rtichoke::prepare_performance_data(
  probs = list("Age and Sex" = acath_pred$pre,
               "Age, Sex and Cholesterol" = acath_pred$post),
  reals = list(acath_pred$sigdz)
) 


conventional_dc <- performance_data_dc %>% 
  rtichoke::plot_decision_curve(
    col_values = c("#00BFC4", "#C77CFF")
  ) %>% 
  plotly::layout(
    yaxis = list(range =
                   c(-0.07, 0.7))
  )
# 1

dca_prepost_smooth + 
  theme_classic() +
  theme(legend.position = "none") +
  ggplot2::theme(
    rect = ggplot2::element_rect(fill = "transparent"),
    panel.background = ggplot2::element_rect(fill='transparent'), #transparent panel bg
    plot.background = ggplot2::element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = ggplot2::element_blank(), #remove major gridlines
    panel.grid.minor = ggplot2::element_blank(), #remove minor gridlines
    legend.background = ggplot2::element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = ggplot2::element_rect(fill='transparent') #transparent legend panel
  )


conventional_dc

# 2

dca_prepost_15_35 + 
  theme_classic()  + 
  theme(legend.position = "none")
  
performance_data_dc %>% 
  rtichoke::plot_decision_curve(
    col_values = c("#00BFC4", "#C77CFF"),
    min_p_threshold = 0.15, 
    max_p_threshold = 0.35,
    size = 400
  ) %>% 
  plotly::layout(
    yaxis = list(range =
                   c(-0.07, 0.7))
  ) 

# 3

dca_prepost_netint + 
  theme_classic()  + 
  theme(legend.position = "none") +
  ggplot2::theme(
    rect = ggplot2::element_rect(fill = "transparent"),
    panel.background = ggplot2::element_rect(fill='transparent'), #transparent panel bg
    plot.background = ggplot2::element_rect(fill='transparent', color=NA), #transparent plot bg
    legend.background = ggplot2::element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = ggplot2::element_rect(fill='transparent') #transparent legend panel
  )

# interventions avoided

performance_data_dc %>% 
  rtichoke::plot_decision_curve(
    col_values = c("#00BFC4", "#C77CFF"),
    type = "interventions avoided",
    size = 400
  ) %>% 
  plotly::layout(
    yaxis = list(range =
                   c(-10, 100))
  )

# combined

performance_data_dc %>% 
  rtichoke::plot_decision_curve(
    col_values = c("#00BFC4", "#C77CFF"),
    type = "combined",
    size = 400
  )

# 4

estimate_tbl

rtichoke::prepare_performance_data(
  probs = list("Age and Sex" = acath_pred$pre,
               "Age, Sex and Cholesterol" = acath_pred$post),
  reals = list(acath_pred$sigdz),
  by = 0.05
) %>%
  select(probability_threshold, model, NB) %>% 
  pivot_wider(
    names_from = "model",
    values_from = "NB"
  ) %>% 
  dplyr::filter(probability_threshold > 0 &
                  probability_threshold <= 0.5) %>% 
  dplyr::mutate(nb_difference = `Age and Sex` - `Age, Sex and Cholesterol`) %>% 
  dplyr::arrange(probability_threshold) %>% 
  mutate(nb_difference_avoided = 0.02334524353) %>% 
  gt() %>%
  fmt_number(
    columns = -probability_threshold,
    decimals = 4
  ) %>%
  cols_label(
    probability_threshold = md("**Threshold**"),
    `Age and Sex` = md("**Net Benefit (Age+Sex)**"),
    `Age, Sex and Cholesterol` = md("**Net Benefit (Age+Sex+Cholesterol)**"),
    nb_difference = md("**Difference in Net Benefit**"),
    nb_difference_avoided = 
      md("**Net Difference in Interventions Avoided per 100 patients**")
  ) %>%
  fmt_percent(
    columns = probability_threshold,
    decimals = 0
  ) %>% 
  tab_options(table.font.size = 13,
              data_row.padding = px(1)) %>% 
  cols_align(
    align = c("center"),
    columns = everything()
  ) %>% 
  cols_align(
    align = c("left"),
    columns = probability_threshold
  )


