####Overview####
# File name:    cps_replication
# Last Change:  07/07/2023
# Purpose:      Data analysis Genschel/Seelkopf/Limberg "Revenue, Redistribution, and the Rise and Fall of Inheritance Taxation" (CPS)

####1. System setups####
#Load Packages
# install packages from CRAN
p_needed <- c("tidyverse", "lme4", "texreg","xlsx","countrycode","imputeTS","foreign","magrittr","interflex",
              "ggplot2","gridExtra","grid","maps","scales","discSurv","varhandle","readstata13",
              "survival","survminer","readxl","cowplot","cshapes","xtable","margins","rdrobust","broom","devtools",
              "Zelig")
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)

#Set Seed
set.seed(2789)

#Set Working Directory as current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load R file - Save in same folder
load( file = "cps_data.RDS")

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

#Intros/Repeals per year
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

#Intros/Repeals per year
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

plot_grid(development_inh_graph, per_year_graph, align = "v", nrow = 2,rel_heights = c(2/3, 1/3))

####Figure 2####

all %>%
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


####Figure 3####

rank_inheritance %>%
  ggplot(., aes(x=rank_tax_base, fill =rank_tax_base )) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +
  ylab("Share of Countries") + 
  scale_fill_grey() +
  xlab("Historical Timing Inheritance Tax") +
  theme(text = element_text(size=20),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



####Figure 4####

austria %>%
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

####Table 1 ####

merge_full_2  %>%
  filter(is.na(democracy)) %>%
  View()




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

screenreg(list(main_inh_1,main_inh_2,main_inh_3), 
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


####Figure 5 & Table A16####

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


screenreg(list(check_inh_1,check_inh_3,check_inh_2), 
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
        custom.coef.map = list("other_tax" = "Revenue Instruments: Income and Consumption Taxes",
                               "all_taxes" = "Revenue Instruments: All Modern Taxes",
                                "v2stfisccap" = "Revenue Instruments: Main Revenue Sources",
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




all_treat %>%
  ggplot(., aes(x=fct_rev(model), y=estimate))  +
  geom_errorbar(position = position_dodge(width = 0.5), aes(ymin=conf.low, ymax=conf.high), width=0, size = 1.1)+ 
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

####Figure 6 and Table A17####
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


screenreg(list(check_inh_1,check_inh_2,check_inh_3,check_inh_4,check_inh_5), 
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
                               "democracy" = "Democracy: Binary (Boix et al.)",
                               "e_polity2" = "Democracy: Polity2 ",
                               "v2x_polyarchy" = "Democracy: Electoral Democracy",
                               "e_lexical_index" = "Democracy: Lexical Index",
                               "e_chga_demo" = "Democracy: Binary (Cheibub et al.)",
                               "war_previous" = "War",
                               "recession_prev_5" = "Binary",
                               
                               "wfs_intro_previous" = "Social Policy Intro",
                               "log(population_new)" = "Tax Competition (Population log)",
                               "log(time_since_intro)" = "Time Since Intro (log)",
                               "e_pelifeex" = "Life Expectancy",
                               "log_gdp_cap" = "GDP pc (log)",
                               "t" = "t",
                               "t2" = "t2",
                               "t3" = "t3"))


all_treat %>%
  ggplot(., aes(x=fct_rev(model), y=estimate))  +
  geom_errorbar(position = position_dodge(width = 0.5), aes(ymin=conf.low, ymax=conf.high), width=0, size = 1.1)+ 
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


####Table A3####

summary(main_inh_1 <- glm(y ~ other_tax + democracy + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + 
                            t + t2 + t3+ v2svdomaut,
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_2 <- glm(y ~ other_tax + democracy + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + 
                            t + t2 + t3 + v2elreggov,
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_3 <- glm(y ~ other_tax + democracy + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + 
                            t + t2 + t3+ v2pepwrgeo,
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_4 <- glm(y ~ other_tax + democracy + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + 
                            t + t2 + t3+ e_miinflat,
                          data = merge_full_2, family=binomial(link="logit")))

screenreg(list(main_inh_1,main_inh_2,main_inh_3,main_inh_4), 
        stars = c(0.01, 0.05, 0.10), 
        include.bic = TRUE,  
        include.variance = FALSE, 
        booktabs = TRUE, 
        dcolumn = TRUE, 
        longtable = TRUE, 
        center = TRUE, 
        digits = 3, 
        fontsize = "small",
        caption = "Robustness Check I: Regression Results for Inheritance Tax Repeal, Additional Covariates",
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
                               "v2svdomaut" = "State Autonomy",
                               "v2elreggov" = "Regional Government",
                               "v2pepwrgeo" = "Rural Political Power",
                               "e_miinflat" = "Inflation",
                               "t" = "t",
                               "t2" = "t2",
                               "t3" = "t3"))

####Table A4####

repeal <- merge_full_2 %>% 
  filter(y == 1) %>%
  dplyr::group_by(region,year) %>% 
  dplyr::count() %>%
  dplyr::rename(n_inh_repeal = n)

#Merge Variables for Regional INH Repeal with long dataset
mix <- left_join(merge_full_2, repeal, by = c("region","year"))  
mix$n_inh_repeal[is.na(mix$n_inh_repeal)] <- 0 #Fill years where no INH has been regionally repealed
n_inh_repeal_1 <- mix %>%
  dplyr::group_by(cowc) %>%
  dplyr::arrange(cowc,year) %>%
  dplyr::mutate(tot_repeals = cumsum(n_inh_repeal))
n_inh_repeal_2 <- n_inh_repeal_1 %>%
  dplyr::group_by(cowc) %>%
  dplyr::mutate(tot_inh_new=na.locf(tot_repeals)) #Fill up total n of adoptions for years where no INH has been repealed
n_inh_repeal_3 <- n_inh_repeal_2 %>%
  dplyr::group_by(cowc) %>%
  dplyr::mutate(l_tot_inh_new = dplyr::lag(tot_inh_new)) %>%
  dplyr::filter(year >= enter) %>%
  data.frame()

summary(main_inh_1 <- glm(y ~ other_tax + democracy + l_tot_inh_new +
                            t + t2 + t3,
                          data = n_inh_repeal_3, family=binomial(link="logit")))

summary(main_inh_2 <- glm(y ~ other_tax + democracy + war_previous + recession_prev_5 + wfs_intro_previous + l_tot_inh_new +
                            t + t2 + t3,
                          data = n_inh_repeal_3, family=binomial(link="logit")))

summary(main_inh_3 <- glm(y ~ other_tax + democracy + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + l_tot_inh_new +
                            t + t2 + t3,
                          data = n_inh_repeal_3, family=binomial(link="logit")))

screenreg(list(main_inh_1,main_inh_2,main_inh_3), 
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
                                 "l_tot_inh_new" = "Spatial Lag Region",
                                 "t" = "t",
                                 "t2" = "t2",
                                 "t3" = "t3"))


####Table A5####

summary(main_inh_1 <- coxph((formula = Surv(spell1, spell_end, y))  ~ 
                              other_tax + democracy , 
                            data = merge_full_2))

summary(main_inh_2 <- coxph((formula = Surv(spell1, spell_end, y))  ~ 
                              other_tax + democracy + 
                              war_previous + recession_prev_5 + wfs_intro_previous, 
                            data = merge_full_2))

summary(main_inh_3 <- coxph((formula = Surv(spell1, spell_end, y))  ~ 
                              other_tax + democracy + 
                              war_previous + recession_prev_5 + wfs_intro_previous +
                              log(time_since_intro) +log_gdp_cap   + e_pelifeex   + log(population_new), 
                            data = merge_full_2))

screenreg(list(main_inh_1,main_inh_2,main_inh_3), 
          stars = c(0.01, 0.05, 0.10), 
          include.bic = TRUE,  
          include.variance = FALSE, 
          booktabs = TRUE, 
          dcolumn = TRUE, 
          longtable = TRUE, 
          center = TRUE, 
          digits = 3, 
          fontsize = "small",
          caption = "Robustness Check IV: Regression Results for Inheritance Tax Repeal, Cox PH Models",
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
                                 "log_gdp_cap" = "GDP pc (log)"))


####Table A6####
summary(main_inh_1 <- glm(y ~ other_tax + democracy + factor(year),
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_2 <- glm(y ~ other_tax + democracy + war_previous + recession_prev_5 + wfs_intro_previous + factor(year),
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_3 <- glm(y ~ other_tax + democracy + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + factor(year),
                          data = merge_full_2, family=binomial(link="logit")))


screenreg(list(main_inh_1,main_inh_2,main_inh_3), 
        stars = c(0.01, 0.05, 0.10), 
        include.bic = TRUE,  
        include.variance = FALSE, 
        booktabs = TRUE, 
        dcolumn = TRUE, 
        longtable = TRUE, 
        center = TRUE, 
        digits = 3, 
        fontsize = "small",
        caption = "Robustness Check III: Regression Results for Inheritance Tax Repeal, Year Fixed Effects",
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


####Table A7####

merge_full_1970 <- merge_full_2 %>%
  mutate(new_enter = case_when(
    enter < 1970 ~ 1970,
    T ~ enter
  ),
  tnew = year - new_enter,
  tnew2 = tnew*tnew,
  tnew3 = tnew*tnew*tnew) %>%
  filter(year >= 1970)


summary(main_inh_1 <- glm(y ~ other_tax + democracy + 
                            tnew + tnew2 + tnew3,
                          data = merge_full_1970, family=binomial(link="logit")))
summary(main_inh_2 <- glm(y ~ other_tax + democracy + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            tnew + tnew2 + tnew3,
                          data = merge_full_1970, family=binomial(link="logit")))
summary(main_inh_3 <- glm(y ~ other_tax + democracy + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + 
                            tnew + tnew2 + tnew3,
                          data = merge_full_1970, family=binomial(link="logit")))

screenreg(list(main_inh_1,main_inh_2,main_inh_3), 
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
                                 "tnew" = "t",
                                 "tnew2" = "t2",
                                 "tnew3" = "t3"))











####Table A8####

summary(main_inh_1 <- glm(y ~ other_tax + democracy + factor(cowc),
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_2 <- glm(y ~ other_tax + democracy + war_previous + recession_prev_5 + wfs_intro_previous + factor(cowc),
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_3 <- glm(y ~ other_tax + democracy  + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + factor(cowc),
                          data = merge_full_2, family=binomial(link="logit")))


screenreg(list(main_inh_1,main_inh_2,main_inh_3), 
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


####Table A9####

summary(main_inh_1 <- glm(y ~ other_tax + democracy +
                            t + t2 + t3+ factor(region),
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_2 <- glm(y ~ other_tax + democracy + war_previous + recession_prev_5 + wfs_intro_previous +
                            t + t2 + t3+ factor(region),
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_3 <- glm(y ~ other_tax + democracy + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + 
                            t + t2 + t3+ factor(region),
                          data = merge_full_2, family=binomial(link="logit")))

screenreg(list(main_inh_1,main_inh_2,main_inh_3), 
        stars = c(0.01, 0.05, 0.10), 
        include.bic = TRUE,  
        include.variance = FALSE, 
        booktabs = TRUE, 
        dcolumn = TRUE, 
        longtable = TRUE, 
        center = TRUE, 
        digits = 3, 
        fontsize = "small",
        caption = "Robustness Check II: Regression Results for Inheritance Tax Repeal, Region Fixed Effects",
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


####Table A10####

summary(main_inh_1 <- lmer(y ~ other_tax + democracy + t + t2 + t3 + (1 | cowc), data = merge_full_2))
summary(main_inh_2 <- lmer(y ~ other_tax + democracy + war_previous + recession_prev_5 + wfs_intro_previous  +t + t2 + t3 + (1 | cowc), data = merge_full_2))
summary(main_inh_3 <- lmer(y ~ other_tax + democracy +
                             war_previous + recession_prev_5 + wfs_intro_previous +
                             log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + t + t2 + t3 + (1 | cowc), data = merge_full_2))

screenreg(list(main_inh_1,main_inh_2,main_inh_3), 
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

####Table A11####

summary(main_inh_1 <- glm(y ~ other_tax + democracy + 
                           t + t2 + t3 ,
                         data = merge_full_2))
summary(main_inh_2 <- glm(y ~ other_tax + democracy + 
                           war_previous + recession_prev_5 + wfs_intro_previous +
                           t + t2 + t3 ,
                         data = merge_full_2))
summary(main_inh_3 <- glm(y ~ other_tax + democracy + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log(time_since_intro) +log_gdp_cap   + e_pelifeex   + log(population_new) +
                            t + t2 + t3 ,
                          data = merge_full_2))

screenreg(list(main_inh_1,main_inh_2,main_inh_3), 
        stars = c(0.01, 0.05, 0.10), 
        include.bic = TRUE,  
        include.variance = FALSE, 
        booktabs = TRUE, 
        dcolumn = TRUE, 
        longtable = TRUE, 
        center = TRUE, 
        digits = 3, 
        fontsize = "small",
        caption = "Robustness Check V: LPM",
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


####Table A12####


merge_full_2 <- merge_full_2 %>%
  data.frame()

summary(main_inh_1 <- zelig(y ~ other_tax + democracy + 
                          t + t2 + t3,
                        data = merge_full_2, model = "relogit", tau = 42/6220))
summary(main_inh_2 <- zelig(y ~ other_tax + democracy + 
                           war_previous + recession_prev_5 + wfs_intro_previous +
                          t + t2 + t3,
                        data = merge_full_2, model = "relogit", tau = 42/6220))
summary(main_inh_3 <- zelig(y ~ other_tax + democracy + 
                              war_previous + recession_prev_5 + wfs_intro_previous +
                              log(time_since_intro) +log_gdp_cap   + e_pelifeex   + log(population_new) +
                              t + t2 + t3,
                        data = merge_full_2, model = "relogit", tau = 42/6220))



screenreg(list(main_inh_1,main_inh_2,main_inh_3), 
        stars = c(0.01, 0.05, 0.10), 
        include.bic = TRUE,  
        include.variance = FALSE, 
        booktabs = TRUE, 
        dcolumn = TRUE, 
        longtable = TRUE, 
        center = TRUE, 
        digits = 3, 
        fontsize = "small",
        caption = "Robustness Check VI: Rare Events Logit",
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

####Table A13####

summary(main_inh_1 <- glm(y ~ consumption_dummy + democracy +
                            t + t2 + t3,
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_2 <- glm(y ~ income_dummy + democracy +
                            t + t2 + t3,
                          data = merge_full_2, family=binomial(link="logit")))


summary(main_inh_3 <- glm(y ~ consumption_dummy  + democracy + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + 
                            t + t2 + t3,
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_4 <- glm(y ~ income_dummy  + democracy + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + 
                            t + t2 + t3,
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_5 <- glm(y ~ consumption_dummy + income_dummy  + democracy + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + 
                            t + t2 + t3,
                          data = merge_full_2, family=binomial(link="logit")))

screenreg(list(main_inh_1,main_inh_2,main_inh_3,main_inh_4, main_inh_5), 
        stars = c(0.01, 0.05, 0.10), 
        include.bic = TRUE,  
        include.variance = FALSE, 
        booktabs = TRUE, 
        dcolumn = TRUE, 
        longtable = TRUE, 
        center = TRUE, 
        digits = 3, 
        fontsize = "small",
        caption = "Consumption Tax Introduction, Income Tax Introduction, and Inheritance Tax Repeal",
        caption.above = TRUE, 
        no.margin = TRUE,
        custom.coef.map = list("consumption_dummy" = "Consumption Tax",
                               "income_dummy" = "Income Tax",
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

####Table A14####

summary(main_inh_1 <- glm(y ~ other_tax + democracy +
                            t + t2 + t3,
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_2 <- glm(y ~ other_tax + democracy + war_previous + recession_prev_5 + wfs_intro_previous +
                            t + t2 + t3,
                          data = merge_full_2, family=binomial(link="logit")))

summary(main_inh_3 <- glm(y ~ other_tax + democracy + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log_gdp_cap + e_pelifeex + log(population_new) + 
                            t + t2 + t3,
                          data = merge_full_2, family=binomial(link="logit")))

screenreg(list(main_inh_1,main_inh_2,main_inh_3), 
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

####Table A15####

merge_limited <- merge_full_2 %>%
  filter(country.x.x != "Canada",
         country.x.x != "El Salvador")



summary(main_inh_1 <- glm(y ~ other_tax + democracy +
                            t + t2 + t3,
                          data = merge_limited, family=binomial(link="logit")))

summary(main_inh_2 <- glm(y ~ other_tax + democracy + war_previous + recession_prev_5 + wfs_intro_previous +
                            t + t2 + t3,
                          data = merge_limited, family=binomial(link="logit")))

summary(main_inh_3 <- glm(y ~ other_tax + democracy + 
                            war_previous + recession_prev_5 + wfs_intro_previous +
                            log(time_since_intro) + log_gdp_cap + e_pelifeex + log(population_new) + 
                            t + t2 + t3,
                          data = merge_limited, family=binomial(link="logit")))

screenreg(list(main_inh_1,main_inh_2,main_inh_3), 
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


all_treat %>%
  ggplot(., aes(x=model, y=estimate))  +
  geom_errorbar(position = position_dodge(width = 0.5), aes(ymin=conf.low, ymax=conf.high), width=0, size = 1.1)+ 
  theme_bw() +
  scale_y_continuous(limits = c(-0.6,1.4)) +
  geom_hline(yintercept=0, linetype = "dashed")  +
  geom_point(size=2, position = position_dodge(width = 0.5)) +
  labs(y = "Point Estimates and 90% CI",
       x = "") + 
  geom_text(aes(label=signif_stars), vjust = -8.3, size = 6)  +
  geom_hline(yintercept=0, linetype = "dashed") +
  theme(text = element_text(size=18))

