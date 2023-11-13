source('SharedScripts.R')
source('Mdrscript.R')

TB_DATA <- read.csv('TB_DATA.csv')
TB_DATA_df2 <- ageCats(TB_DATA_df1)
TB_DATA_df2$R_TYPE <- NA
TB_DATA_df2$R_TYPE[TB_DATA_df2$Resistance == 'MDR'] <- 'MDR'
TB_DATA_df2$R_TYPE[TB_DATA_df2$Resistance != 'MDR'] <- 'NON_MDR'
TB_DATA_df2
tbplot <- ggplot(TB_DATA_df2, aes(x=age_group, fill = Resistance)) + geom_bar(position = position_dodge(preserve = 'single'))
tbplot
smearplot <- ggplot(TB_DATA_df2, aes(x=Resistance, fill = SMEAR)) + geom_bar(position = position_dodge(preserve = 'single'))
smearplot


ggplot(TB_DATA_df2, aes(x = Resistance, fill = Resistance)) +
  geom_bar() +
  facet_wrap(~ age_group, nrow = 1) +
  labs(title = "Resistance Trends by Age Groups", x = "Resistance Levels", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

TB_DATA_df3 <- TB_DATA_df2
TB_DATA_df3$R_TYPE <- ifelse(TB_DATA_df2$Resistance == 'MDR', 1, 0)
# Logistics regression for MULTIVARIATE analysis
logistic_model <- glm(R_TYPE ~ age_group + GENDER + SMEAR + TB_TYPE + HIV_STATUS, data = TB_DATA_df3, family = binomial)
TB_DATA_df3
odds_ratios <- exp(coef(logistic_model))
conf_intervals <- confint(logistic_model)
results <- data.frame(Odds_Ratio = odds_ratios, Lower_CI = conf_intervals[, 1], Upper_CI = conf_intervals[, 2])
results 
# Logistics regression for UNIVARIATE analysis
univariate_vars <- c('age_group', 'SMEAR', 'TB_TYPE', 'HIV_STATUS', 'GENDER')
results2 <- list()
for (var in univariate_vars){
  logistic_model <- glm(data = TB_DATA_df3, formula = R_TYPE, var)
  
}

gc <- geocode(paste(mdr_tb_dfv3$Province, mdr_tb_dfv3$District, 'Kenya'))
mapdf <- cbind(mdr_tb_dfv3, gc)
map <- get_map(gc, zoom = 6)
map
kenyan_map <- ggmap(map)
newkenyanmap <- ggmap(get_map(location = 'kenya', zoom = 6, maptype = 'roadmap')) + theme(panel.background = element_blank()) + geom_point(data = mapdf, aes(x=lon, y=lat, color = MDR_TYPE)) 
newkenyanmap 

