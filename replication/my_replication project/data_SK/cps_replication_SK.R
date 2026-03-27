####Overview####
# File name:   cps_replication_SK
# Last Change:  23/03/2026
# Purpose:      Final Replication project

####1. System setups####
# Load Packages
# Install packages from CRAN
p_needed <- c("tidyverse", "lme4", "texreg","xlsx","countrycode","imputeTS","foreign","magrittr","interflex",
              "ggplot2","gridExtra","grid","maps","scales","discSurv","varhandle","readstata13",
              "survival","survminer","readxl","cowplot","cshapes","xtable","margins","rdrobust","broom","devtools") # Exclude Zelig
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)


# Save plots as PDF files with ggsave
save_plot_pdf <- function(plot_object, filename, width, height) {
  print(plot_object)
  ggplot2::ggsave(filename = filename, plot = plot_object, width = width, height = height, device = "pdf")
}


# Save tables as LaTeX (.tex) files (for Table 1)
save_latex_table <- function(models, filename, ...) {
  reg_args <- list(...)
  do.call(texreg::texreg, c(list(l = models, file = filename), reg_args))
  cat(paste0("✅ Saved LaTeX table to: ", filename, "\n"))
}

# Set Seed
set.seed(2789)

# Set Working Directory
setwd("/Users/ines/Library/Mobile Documents/com~apple~CloudDocs/0_Drive/School/0_Lecture/2학기/Stats II/StatsII_2026/replication/my_replication project/data_SK")

# Load R file 
load(file = "cps_data.RDS")


####Figure 1####

introductions_per_year <- data_inh_full %>%
  dplyr::rename(year = introyear) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(n_intro = n())
years_total <- data.frame(year = 1750:2015)
data_introductions <- left_join(years_total, introductions_per_year, by = c("year"))
data_introductions$n_intro[is.na(data_introductions$n_intro)] <- 0
data_introductions <- data_introductions %>%
  dplyr::mutate(tot_intro = cumsum(n_intro))

repeals_per_year <- data_inh_full %>%
  dplyr::filter(repeal == 1) %>%
  dplyr::rename(year = year_repeal) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(n_repeal = n())
data_introductions_repeals <- left_join(data_introductions, repeals_per_year, by = c("year"))
data_introductions_repeals$n_repeal[is.na(data_introductions_repeals$n_repeal)] <- 0
data_introductions_repeals <- data_introductions_repeals %>%
  dplyr::mutate(tot_repeal = cumsum(n_repeal),
                tot_inh_taxes = tot_intro - tot_repeal,
                color_graph = 1)

# Intros/Repeals per year
data_introductions_repeals1 <- data_introductions_repeals %>%
  dplyr::mutate(n_repeal = -(n_repeal))
data_introductions_repeals2 <- gather(data_introductions_repeals1, type_change, n_change, c("n_intro","n_repeal"),factor_key=TRUE)

development_inh_graph <- data_introductions_repeals %>% 
  ggplot(data=., aes(x=year, y=tot_inh_taxes)) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(limits = c(1749, 2016),breaks=seq(1750, 2015, 50)) +
  theme_bw() +
  theme(axis.text=element_text(size=23),
        axis.title=element_text(size=23),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none") +
  labs(y = "Countries with Inheritance Tax")

# Intros/Repeals per year
data_introductions_repeals1 <- data_introductions_repeals %>%
  dplyr::mutate(n_repeal = -(n_repeal))
data_introductions_repeals2 <- gather(data_introductions_repeals1, type_change, n_change, c("n_intro","n_repeal"),factor_key=TRUE)

per_year_graph <- data_introductions_repeals2 %>% 
  ggplot(data=., aes(x=year, y=n_change, fill = factor(type_change))) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(limits = c(1749, 2016),breaks=seq(1750, 2015, 50)) +
  theme_bw() +
  theme(axis.text=element_text(size=23),
        axis.title=element_text(size=23),
        legend.position = "none") +
  labs(x = "Year",
       y = "Intros and Repeals")

fig1 <- plot_grid(development_inh_graph, per_year_graph, align = "v", nrow = 2,rel_heights = c(2/3, 1/3))
save_plot_pdf(fig1, "Figure1.pdf", width = 12, height = 10)


####Figure 2####

fig2 <- all %>%
  ggplot(., aes(x=Type, y = freq, fill = Type_2))  +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(labels=scales::percent,
                     limits = c(0, 0.6)) +
  theme_bw() +
  scale_fill_grey() +
  ylab("Share") +
  theme(text = element_text(size=15),
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

save_plot_pdf(fig2, "Figure2.pdf", width = 8, height = 6)


####Figure 3####

fig3 <- rank_inheritance %>%
  ggplot(., aes(x=rank_tax_base, fill =rank_tax_base )) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count)))) + 
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +
  ylab("Share of Countries") + 
  scale_fill_grey() +
  xlab("Historical Timing Inheritance Tax") +
  theme(text = element_text(size=20),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

save_plot_pdf(fig3, "Figure3.pdf", width = 10, height = 7)


####Figure 4####

fig4 <- austria %>%
  ggplot(., aes(x=year, y=value, group = type, color=type, fill=type)) +
  geom_point() +
  geom_smooth(aes()) +
  theme_bw() +
  theme(axis.text=element_text(size=23),
        axis.title=element_text(size=23),
        text = element_text(size=25),
        legend.position = "bottom",
        legend.title=element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),) +
  guides(fill=guide_legend(nrow=2),
         color=guide_legend(nrow=2)) +
  labs(x = "Year",
       y = "Percentage") +
  scale_color_discrete(labels = c("Inheritance Tax Revenue (% of Total Revenue)", "Revenue (% of GDP)")) +
  scale_fill_discrete(labels = c("Inheritance Tax Revenue (% of Total Revenue)", "Revenue (% of GDP)")) + 
  scale_colour_grey(labels = c("Inheritance Tax Revenue (% of Total Revenue)", "Revenue (% of GDP)")) +
  scale_fill_grey(labels = c("Inheritance Tax Revenue (% of Total Revenue)", "Revenue (% of GDP)"))

save_plot_pdf(fig4, "Figure4.pdf", width = 12, height = 8)


####Table 1####

summary(main_inh_1 <- glm(y ~ other_tax + democracy +
                            t + t2 + t3,
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_2 <- glm(y ~ other_tax + democracy + war_previous + recession_prev_5 + wfs_intro_previous +
                            t + t2 + t3,
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_3 <- glm(y ~ other_tax + democracy  + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + 
                            t + t2 + t3,
                          data = merge_full_2, family=binomial(link="logit")))

# Save Table 1 as LaTeX
save_latex_table(list(main_inh_1,main_inh_2,main_inh_3), 
                 filename = "Table1.tex",
                 stars = c(0.01, 0.05, 0.10), 
                 include.bic = TRUE,  
                 include.variance = FALSE, 
                 booktabs = TRUE, 
                 dcolumn = TRUE, 
                 longtable = TRUE, 
                 center = TRUE, 
                 digits = 3, 
                 fontsize = "small",
                 caption = "Regression Results for Inheritance Tax Repeal",
                 caption.above = TRUE, 
                 no.margin = TRUE,
                 custom.coef.map = list("other_tax" = "Major Modern Taxes",
                                        "democracy" = "Democracy",
                                        "war_previous" = "War",
                                        "recession_prev_5" = "Recession",
                                        "wfs_intro_previous" = "Social Policy Intro",
                                        "log(population_new)" = "Tax Competition (Population log)",
                                        "log(time_since_intro)" = "Time Since Intro (log)",
                                        "e_pelifeex" = "Life Expectancy",
                                        "log_gdp_cap" = "GDP pc (log)",
                                        "t" = "t",
                                        "t2" = "t2",
                                        "t3" = "t3"))


####Figure 5####

summary(check_inh_1 <- glm(y ~ other_tax + democracy + 
                             war_previous + recession_prev_5 + wfs_intro_previous +
                             log(time_since_intro) +log_gdp_cap   + e_pelifeex   + log(population_new) +
                             t + t2 + t3,
                           data = merge_full_2, family=binomial(link="logit")))

summary(check_inh_2 <- glm(y ~ v2stfisccap + democracy + 
                             war_previous + recession_prev_5 + wfs_intro_previous +
                             log(time_since_intro) +log_gdp_cap   + e_pelifeex   + log(population_new) +
                             t + t2 + t3,
                           data = merge_full_2, family=binomial(link="logit")))

summary(check_inh_3 <- glm(y ~ all_taxes + democracy + 
                             war_previous + recession_prev_5 + wfs_intro_previous +
                             log(time_since_intro) +log_gdp_cap   + e_pelifeex   + log(population_new) +
                             t + t2 + t3,
                           data = merge_full_2, family=binomial(link="logit")))

est_sep_1 <- tidy(check_inh_1, conf.int = FALSE) %>%
  mutate(model = "Measure: Income and Consumption Taxes (Seelkopf et al.)",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error,
         estimate = estimate * (sd(merge_full_2$other_tax, na.rm = T)),
         std.error = std.error* (sd(merge_full_2$other_tax, na.rm = T)),
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error)

est_sep_2 <- tidy(check_inh_2, conf.int = FALSE) %>%
  mutate(model = "Measure: Main Revenue Sources (VDEM)",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error,
         estimate = estimate * (sd(merge_full_2$v2stfisccap, na.rm = T)),
         std.error = std.error* (sd(merge_full_2$v2stfisccap, na.rm = T)),
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error)

est_sep_3 <- tidy(check_inh_3, conf.int = FALSE) %>%
  mutate(model = "Measure: All Modern Taxes (Seelkopf et al.)",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error,
         estimate = estimate * (sd(merge_full_2$all_taxes, na.rm = T)),
         std.error = std.error* (sd(merge_full_2$all_taxes, na.rm = T)),
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error)

all_treat <- rbind(est_sep_1, est_sep_2, est_sep_3) %>%
  filter(term %in% c("other_tax", "v2stfisccap", "all_taxes")) %>%
  mutate(
    signif = case_when(
      p.value < 0.05 ~ 1,
      T ~ 0),
    signif_stars = case_when(
      p.value < 0.10 & p.value > 0.05~ "*",
      p.value < 0.05 & p.value > 0.01~ "**",
      p.value < 0.01 ~ "***",
      T ~ ""))

all_treat$model <- factor(all_treat$model , 
                          levels = c("Measure: Income and Consumption Taxes (Seelkopf et al.)",
                                     "Measure: All Modern Taxes (Seelkopf et al.)",
                                     "Measure: Main Revenue Sources (VDEM)"))

fig5 <- all_treat %>%
  ggplot(., aes(x=fct_rev(model), y=estimate))  +
  geom_errorbar(position = position_dodge(width = 0.5), aes(ymin=conf.low, ymax=conf.high), width=0, linewidth = 1.1)+ 
  theme_bw() +
  scale_y_continuous() +
  geom_hline(yintercept=0, linetype = "dashed")  +
  geom_point(size=2, position = position_dodge(width = 0.5)) +
  coord_flip() +
  labs(y = "Point Estimates and 90% CI (Standardised)",
       x = "") + 
  geom_text(aes(label=signif_stars), vjust = -0.35, size = 6)  +
  geom_hline(yintercept=0, linetype = "dashed") +
  theme(text = element_text(size=18))

save_plot_pdf(fig5, "Figure5.pdf", width = 12, height = 8)


####Figure 6####

summary(check_inh_1 <- glm(y ~ other_tax + democracy + 
                             war_previous + recession_prev_5 + wfs_intro_previous +
                             log(time_since_intro) +log_gdp_cap  + e_pelifeex   + log(population_new) +
                             t + t2 + t3,
                           data = merge_full_2, family=binomial(link="logit")))

summary(check_inh_2 <- glm(y ~ other_tax + e_polity2 + 
                             war_previous + recession_prev_5 + wfs_intro_previous +
                             log(time_since_intro) +log_gdp_cap + e_pelifeex   + log(population_new) +
                             t + t2 + t3,
                           data = merge_full_2, family=binomial(link="logit")))

summary(check_inh_3 <- glm(y ~ other_tax + v2x_polyarchy + 
                             war_previous + recession_prev_5 + wfs_intro_previous +
                             log(time_since_intro) +log_gdp_cap   + e_pelifeex   + log(population_new) +
                             t + t2 + t3,
                           data = merge_full_2, family=binomial(link="logit")))

summary(check_inh_4 <- glm(y ~ other_tax + e_lexical_index + 
                             war_previous + recession_prev_5 + wfs_intro_previous +
                             log(time_since_intro) +log_gdp_cap   + e_pelifeex   + log(population_new) +
                             t + t2 + t3,
                           data = merge_full_2, family=binomial(link="logit")))

summary(check_inh_5 <- glm(y ~ other_tax + e_chga_demo + 
                             war_previous + recession_prev_5 + wfs_intro_previous +
                             log(time_since_intro) +log_gdp_cap   + e_pelifeex   + log(population_new) +
                             t + t2 + t3,
                           data = merge_full_2, family=binomial(link="logit")))


est_sep_1 <- tidy(check_inh_1, conf.int = FALSE) %>%
  mutate(model = "Measure: Binary (Boix et al.)",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error,
         estimate = estimate * (sd(merge_full_2$democracy, na.rm = T)),
         conf.low = conf.low * (sd(merge_full_2$democracy, na.rm = T)),
         conf.high = conf.high * (sd(merge_full_2$democracy, na.rm = T)))

est_sep_2 <- tidy(check_inh_2, conf.int = FALSE) %>%
  mutate(model = "Measure: Polity2 (Polity IV Project)",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error,
         estimate = estimate * (sd(merge_full_2$e_polity2, na.rm = T)),
         conf.low = conf.low * (sd(merge_full_2$e_polity2, na.rm = T)),
         conf.high = conf.high * (sd(merge_full_2$e_polity2, na.rm = T)))

est_sep_3 <- tidy(check_inh_3, conf.int = FALSE) %>%
  mutate(model = "Measure: Electoral Democracy (VDEM)",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error,
         estimate = estimate * (sd(merge_full_2$v2x_polyarchy, na.rm = T)),
         conf.low = conf.low * (sd(merge_full_2$v2x_polyarchy, na.rm = T)),
         conf.high = conf.high * (sd(merge_full_2$v2x_polyarchy, na.rm = T)))

est_sep_4 <- tidy(check_inh_4, conf.int = FALSE) %>%
  mutate(model = "Measure: Lexical Index (Skaaning et al.)",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error,
         estimate = estimate * (sd(merge_full_2$e_lexical_index, na.rm = T)),
         conf.low = conf.low * (sd(merge_full_2$e_lexical_index, na.rm = T)),
         conf.high = conf.high * (sd(merge_full_2$e_lexical_index, na.rm = T)))

est_sep_5 <- tidy(check_inh_5, conf.int = FALSE) %>%
  mutate(model = "Measure: Binary (Cheibub et al.)",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error,
         estimate = estimate * (sd(merge_full_2$e_chga_demo, na.rm = T)),
         conf.low = conf.low * (sd(merge_full_2$e_chga_demo, na.rm = T)),
         conf.high = conf.high * (sd(merge_full_2$e_chga_demo, na.rm = T)))

all_treat <- rbind(est_sep_1, est_sep_2, est_sep_3, est_sep_4, est_sep_5) %>%
  filter(term %in% c("democracy", "e_polity2", "v2x_polyarchy", "e_lexical_index","e_chga_demo")) %>%
  mutate(
    signif = case_when(
      p.value < 0.05 ~ 1,
      T ~ 0),
    signif_stars = case_when(
      p.value < 0.10 & p.value > 0.05~ "*",
      p.value < 0.05 & p.value > 0.01~ "**",
      p.value < 0.01 ~ "***",
      T ~ ""))

all_treat$model <- factor(all_treat$model , 
                          levels = c("Measure: Binary (Boix et al.)",
                                     "Measure: Polity2 (Polity IV Project)",
                                     "Measure: Electoral Democracy (VDEM)",
                                     "Measure: Lexical Index (Skaaning et al.)",
                                     "Measure: Binary (Cheibub et al.)"))

fig6 <- all_treat %>%
  ggplot(., aes(x=fct_rev(model), y=estimate))  +
  geom_errorbar(position = position_dodge(width = 0.5), aes(ymin=conf.low, ymax=conf.high), width=0, linewidth = 1.1)+ 
  theme_bw() +
  scale_y_continuous() +
  geom_hline(yintercept=0, linetype = "dashed")  +
  geom_point(size=2, position = position_dodge(width = 0.5)) +
  coord_flip() +
  labs(y = "Point Estimates and 90% CI (Standardised)",
       x = "") + 
  geom_text(aes(label=signif_stars), vjust = -0.35, size = 6)  +
  geom_hline(yintercept=0, linetype = "dashed") +
  theme(text = element_text(size=18))

save_plot_pdf(fig6, "Figure6.pdf", width = 12, height = 8)


####Figure A1####

merge_change <- merge_full_2 %>%
  mutate(new_tax_5 = case_when(
    year <= VAT + 5 & year >= VAT ~ 1,
    year <= GST + 5 & year >= GST ~ 1,
    year <= PIT + 5 & year >= PIT ~ 1,
    year <= CIT + 5 & year >= CIT ~ 1,
    T ~ 0
  ),
  new_tax_6 = case_when(
    year <= VAT + 6 & year >= VAT ~ 1,
    year <= GST + 6 & year >= GST ~ 1,
    year <= PIT + 6 & year >= PIT ~ 1,
    year <= CIT + 6 & year >= CIT ~ 1,
    T ~ 0
  ),
  new_tax_7 = case_when(
    year <= VAT + 7 & year >= VAT ~ 1,
    year <= GST + 7 & year >= GST ~ 1,
    year <= PIT + 7 & year >= PIT ~ 1,
    year <= CIT + 7 & year >= CIT ~ 1,
    T ~ 0
  ),
  new_tax_8 = case_when(
    year <= VAT + 8 & year >= VAT ~ 1,
    year <= GST + 8 & year >= GST ~ 1,
    year <= PIT + 8 & year >= PIT ~ 1,
    year <= CIT + 8 & year >= CIT ~ 1,
    T ~ 0
  ),
  new_tax_9 = case_when(
    year <= VAT + 9 & year >= VAT ~ 1,
    year <= GST + 9 & year >= GST ~ 1,
    year <= PIT + 9 & year >= PIT ~ 1,
    year <= CIT + 9 & year >= CIT ~ 1,
    T ~ 0
  ),
  new_tax_10 = case_when(
    year <= VAT + 10 & year >= VAT ~ 1,
    year <= GST + 10 & year >= GST ~ 1,
    year <= PIT + 10 & year >= PIT ~ 1,
    year <= CIT + 10 & year >= CIT ~ 1,
    T ~ 0
  ),
  new_tax_11 = case_when(
    year <= VAT + 11 & year >= VAT ~ 1,
    year <= GST + 11 & year >= GST ~ 1,
    year <= PIT + 11 & year >= PIT ~ 1,
    year <= CIT + 11 & year >= CIT ~ 1,
    T ~ 0
  ),
  new_tax_12 = case_when(
    year <= VAT + 12 & year >= VAT ~ 1,
    year <= GST + 12 & year >= GST ~ 1,
    year <= PIT + 12 & year >= PIT ~ 1,
    year <= CIT + 12 & year >= CIT ~ 1,
    T ~ 0
  ),
  new_tax_13 = case_when(
    year <= VAT + 13 & year >= VAT ~ 1,
    year <= GST + 13 & year >= GST ~ 1,
    year <= PIT + 13 & year >= PIT ~ 1,
    year <= CIT + 13 & year >= CIT ~ 1,
    T ~ 0
  ),
  new_tax_14 = case_when(
    year <= VAT + 14 & year >= VAT ~ 1,
    year <= GST + 14 & year >= GST ~ 1,
    year <= PIT + 14 & year >= PIT ~ 1,
    year <= CIT + 14 & year >= CIT ~ 1,
    T ~ 0
  ),
  new_tax_15 = case_when(
    year <= VAT + 15 & year >= VAT ~ 1,
    year <= GST + 15 & year >= GST ~ 1,
    year <= PIT + 15 & year >= PIT ~ 1,
    year <= CIT + 15 & year >= CIT ~ 1,
    T ~ 0
  ))

summary(main_inh_5 <- glm(y ~ new_tax_5 + democracy +
                            t + t2 + t3,
                          data = merge_change, family=binomial(link="logit")))
summary(main_inh_6 <- glm(y ~ new_tax_6 + democracy +
                            t + t2 + t3,
                          data = merge_change, family=binomial(link="logit")))
summary(main_inh_7 <- glm(y ~ new_tax_7 + democracy +
                            t + t2 + t3,
                          data = merge_change, family=binomial(link="logit")))
summary(main_inh_8 <- glm(y ~ new_tax_8 + democracy +
                            t + t2 + t3,
                          data = merge_change, family=binomial(link="logit")))
summary(main_inh_9 <- glm(y ~ new_tax_9 + democracy +
                            t + t2 + t3,
                          data = merge_change, family=binomial(link="logit")))
summary(main_inh_10 <- glm(y ~ new_tax_10 + democracy +
                             t + t2 + t3,
                           data = merge_change, family=binomial(link="logit")))
summary(main_inh_11 <- glm(y ~ new_tax_11 + democracy +
                             t + t2 + t3,
                           data = merge_change, family=binomial(link="logit")))
summary(main_inh_12 <- glm(y ~ new_tax_12 + democracy +
                             t + t2 + t3,
                           data = merge_change, family=binomial(link="logit")))
summary(main_inh_13 <- glm(y ~ new_tax_13 + democracy +
                             t + t2 + t3,
                           data = merge_change, family=binomial(link="logit")))
summary(main_inh_14 <- glm(y ~ new_tax_14 + democracy +
                             t + t2 + t3,
                           data = merge_change, family=binomial(link="logit")))
summary(main_inh_15 <- glm(y ~ new_tax_15 + democracy +
                             t + t2 + t3,
                           data = merge_change, family=binomial(link="logit")))

est_sep_5 <- tidy(main_inh_5, conf.int = TRUE, conf.level = 0.90) %>%
  mutate(model = "5 Years",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error) %>%
  filter(term == "new_tax_5")
est_sep_6 <- tidy(main_inh_6, conf.int = TRUE, conf.level = 0.90) %>%
  mutate(model = "6 Years",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error) %>%
  filter(term == "new_tax_6")
est_sep_7 <- tidy(main_inh_7, conf.int = TRUE, conf.level = 0.90) %>%
  mutate(model = "7 Years",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error) %>%
  filter(term == "new_tax_7")
est_sep_8 <- tidy(main_inh_8, conf.int = TRUE, conf.level = 0.90) %>%
  mutate(model = "8 Years",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error) %>%
  filter(term == "new_tax_8")
est_sep_9 <- tidy(main_inh_9, conf.int = TRUE, conf.level = 0.90) %>%
  mutate(model = "9 Years",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error) %>%
  filter(term == "new_tax_9")
est_sep_10 <- tidy(main_inh_10, conf.int = TRUE, conf.level = 0.90) %>%
  mutate(model = "10 Years",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error) %>%
  filter(term == "new_tax_10")
est_sep_11 <- tidy(main_inh_11, conf.int = TRUE, conf.level = 0.90) %>%
  mutate(model = "11 Years",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error) %>%
  filter(term == "new_tax_11")
est_sep_12 <- tidy(main_inh_12, conf.int = TRUE, conf.level = 0.90) %>%
  mutate(model = "12 Years",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error) %>%
  filter(term == "new_tax_12")
est_sep_13 <- tidy(main_inh_13, conf.int = TRUE, conf.level = 0.90) %>%
  mutate(model = "13 Years",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error) %>%
  filter(term == "new_tax_13")
est_sep_14 <- tidy(main_inh_14, conf.int = TRUE, conf.level = 0.90) %>%
  mutate(model = "14 Years",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error) %>%
  filter(term == "new_tax_14")
est_sep_15 <- tidy(main_inh_15, conf.int = TRUE, conf.level = 0.90) %>%
  mutate(model = "15 Years",
         conf.high = estimate + 1.65*std.error,
         conf.low = estimate - 1.65*std.error) %>%
  filter(term == "new_tax_15")

all_treat <- rbind(est_sep_5, est_sep_6, est_sep_7,est_sep_8, est_sep_9, est_sep_10, est_sep_11,
                   est_sep_12,est_sep_13, est_sep_14, est_sep_15) %>%
  mutate(
    signif = case_when(
      p.value < 0.05 ~ 1,
      T ~ 0),
    signif_stars = case_when(
      p.value < 0.10 & p.value > 0.05~ "*",
      p.value < 0.05 & p.value > 0.01~ "**",
      p.value < 0.01 ~ "***",
      T ~ ""))

all_treat$model <- factor(all_treat$model , 
                          levels = c("5 Years",
                                     "6 Years",
                                     "7 Years",
                                     "8 Years",
                                     "9 Years",
                                     "10 Years",
                                     "11 Years",
                                     "12 Years",
                                     "13 Years",
                                     "14 Years",
                                     "15 Years"))

fig_a1 <- all_treat %>%
  ggplot(., aes(x=model, y=estimate))  +
  geom_errorbar(position = position_dodge(width = 0.5), aes(ymin=conf.low, ymax=conf.high), width=0, linewidth = 1.1)+ 
  theme_bw() +
  scale_y_continuous(limits = c(-0.6,1.4)) +
  geom_hline(yintercept=0, linetype = "dashed")  +
  geom_point(size=2, position = position_dodge(width = 0.5)) +
  labs(y = "Point Estimates and 90% CI",
       x = "") + 
  geom_text(aes(label=signif_stars), vjust = -8.3, size = 6)  +
  geom_hline(yintercept=0, linetype = "dashed") +
  theme(text = element_text(size=18))

save_plot_pdf(fig_a1, "FigureA1.pdf", width = 12, height = 7)

#### 4. My Contribution: ####
# Load packages for interaction analysis and plotting
#install.packages("marginaleffects")
library(marginaleffects)

# Fit interaction model
summary(model_int <- glm(y ~ other_tax * democracy + 
                           war_previous + recession_prev_5 + wfs_intro_previous +
                           log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + 
                           t + t2 + t3,
                         data = merge_full_2, family = binomial(link="logit")))

# Export Table 2 comparing Model 3 and Interaction Model using save_latex_table (texreg)
save_latex_table(list(main_inh_3, model_int), 
                 filename = "Table2.tex",
                 stars = c(0.01, 0.05, 0.10), 
                 include.bic = TRUE,  
                 include.variance = FALSE, 
                 booktabs = TRUE, 
                 dcolumn = TRUE, 
                 longtable = TRUE, 
                 center = TRUE, 
                 digits = 3, 
                 fontsize = "small",
                 caption = "Determinants of Inheritance Tax Repeals (Interaction Model)",
                 caption.above = TRUE, 
                 no.margin = TRUE,
                 custom.model.names = c("Original (Model 3)", "Interaction Model"),
                 custom.coef.map = list("other_tax" = "Major Modern Taxes",
                                        "democracy" = "Democracy",
                                        "other_tax:democracy" = "Major Modern Taxes $\\times$ Democracy",
                                        "war_previous" = "War",
                                        "recession_prev_5" = "Recession",
                                        "wfs_intro_previous" = "Social Policy Intro",
                                        "log(population_new)" = "Tax Competition (Population log)",
                                        "log(time_since_intro)" = "Time Since Intro (log)",
                                        "e_pelifeex" = "Life Expectancy",
                                        "log_gdp_cap" = "GDP pc (log)",
                                        "t" = "$t^1$",
                                        "t2" = "$t^2$",
                                        "t3" = "$t^3$"))

# Convert democracy to factor for clear legend in plot
merge_full_2$democracy_factor <- as.factor(merge_full_2$democracy)

# Re-fit model with factorized democracy for plotting
model_int_plot <- glm(y ~ other_tax * democracy_factor + 
                        war_previous + recession_prev_5 + wfs_intro_previous +
                        log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + 
                        t + t2 + t3,
                      data = merge_full_2, family = binomial(link="logit"))

# Plot predicted probabilities
fig7 <- plot_predictions(model_int_plot, 
                         condition = c("other_tax", "democracy_factor")) +
  theme_bw() +
  labs(title = "Predicted Probability of Inheritance Tax Repeal",
       x = "Major Modern Taxes Index",
       y = "Predicted Probability") +
  scale_color_manual(values = c("0" = "red", "1" = "blue")) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue")) +
  
  geom_text(data = function(x) subset(x, other_tax == max(other_tax)),
            aes(x = other_tax, y = estimate, 
                label = ifelse(democracy_factor == "0", "Autocracy", "Democracy"),
                color = democracy_factor),
            hjust = -0.1, size = 5, fontface = "bold", show.legend = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  theme(text = element_text(size = 14),
        legend.position = "none")

# Save plot
save_plot_pdf(fig7, "Figure7.pdf", width = 10, height = 7)

