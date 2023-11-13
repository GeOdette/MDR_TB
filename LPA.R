source('SharedScripts.R')
mdrtb_orig <- read_excel('ntrl-jul2015-dec2016.xlsx') 
df1 <- mdrtb_orig |> 
  select(-Template, -sample_number, -SAMPLE_STATUS, -SAMPLED_DATE, -LOGIN_DATE, 
         -MOH_COLLECTION_DATE, -DATE_REVIEWED, -RELEASED_ON, -MGIT, 
         -MGITLJ_COMPARE, -GENE_EXPERT_MTB, -GENE_EXPERT_RIF, -ScreenSmear, 
         -SMEAR_DIRECT, -SMEAR_COLONY, -MTBL_ISOLATED, -TEXT_ID,
         -DST_1ST_Method, -SmearScreen, -DST_1ST_Growth_Control, -DST_SECOND_LINE, -IdentificationTest, 
         -First_Name, -Middle_Name, -Last_Name, -DRS_NO, 
         -LJ1_GROWTH, -MOH_HEALTH_FACILITY, -PATIENT) %>%
  rename(Sex=X_GENDER, Patient_Type=MOH_TB_TYPE_PATIENT, 
         Smear = SMEAR_CONCENTRA, STR_pDST=DST_1ST_Streptomycin,
         INH_pDST=DST_1ST_Isoniazid, RIF_pSDT=DST_1ST_Rifampicin,
         EMB_pDST=DST_1ST_Ethambutol, PRZ_pDST=DST_1ST_Pyrazinamide,
         HIV= MOH_HIV_STATUS, rpoB=DNA_MTBDRPLUS_rpoB,
         KatG=DNA_MTBDRPLUS_KatG, inhA= DNA_MTBDRPLUS_inhA)
df2 <- df1 %>% select(-STR_pDST, -INH_pDST, -EMB_pDST, -PRZ_pDST, -RIF_pSDT,
                      -DST_1ST_GC_Pyrazinamide)
df2$AGE <- as.numeric(df2$AGE)

mdrtb_lpa <- df2[complete.cases(df2[, c("inhA")]), ] |> ageCats() |>
  mutate(Patient_Type = fct_recode(Patient_Type, 
                                   "Failure at initial treatment" = "FA_1STL_TR",
                                   "Retreatment after treatment failure" = "FA_RE_TREA",
                                   "MDR Contaminated" = "MDR_CONTA",
                                   "MDR Follow-UP" = "MDR_F",
                                   "New Patient" = "NEW",
                                   "Retreatment after default" = "RE_AF_DEFA",
                                   "Positive" = "Positive", 
                                  "SP_SM_NR" = "Sputum smear negative relapse",
                                  "SP_SM_PR" = "Sputum smear positive relapse"),
         HIV= fct_recode(HIV, "Not Done" = "NOT_DONE",
                                     "Declined" = "DECLINED",
                                     "Negative" ="NEGATIVE",
                                     "Positive" = "POSITIVE")) |>
  labelled::set_variable_labels(HIV = "HIV STATUS", Sex = "Sex",
                                Patient_Type = "TB Patient Type",
                                age_group = "AGE GROUP",
                                Smear = "Smear Concentration"
  ) 


mdrtb_lpa2 <- mdrtb_lpa |> select(-Province, -District, -Facility) |> 
  mutate(MDR = case_when(
    (KatG == "Resistant" & rpoB == "Resistant") |
      (rpoB == 'Resistant' & inhA == 'Resistant') ~ "MDR",
    TRUE ~ "Non-MDR"
  ))

mdrtb_lpa4 <- mdrtb_lpa2 |> mutate(
  MDR_TYPE = case_when(
    KatG == "Resistant" & rpoB == "Resistant" ~ "RpoB + KatG",
    TRUE ~ "NONE"
  )
)
mdrtb_lpa5 <- mdrtb_lpa2 |> mutate(
  MDR_TYPE = case_when(
    inhA == "Resistant" & rpoB == "Resistant" ~ "RpoB + inhA",
    TRUE ~ "NONE"
  )
)
mdrtb_lpa6 <- mdrtb_lpa2[mdrtb_lpa2$MDR == "MDR", ]
mdrtb_lpa6 
mdrtb_lpa3 <- mdrtb_lpa6 |> mutate(
  MDR_TYPE = case_when(
    KatG == "Resistant" & rpoB == "Resistant" & inhA == "Resistant" ~ "RpoB + KatG + inhA",
    KatG == "Resistant" & rpoB == "Resistant" ~ "RpoB + KatG",
    inhA == "Resistant" & rpoB == "Resistant" ~ "RpoB + inhA",
    TRUE ~ "NON-MDR"
  )
)
mdrtb_lpa4 |> tbl_summary(by = MDR_TYPE)
mdrtb_lpa3 |> tbl_summary(by = MDR_TYPE)
mdrtb_lpa3 |> select(MDR_TYPE) |> tbl_summary()
mdrtb_lpa5 |> tbl_summary(by = MDR_TYPE)
fig6 <- mdrtb_lpa2 |> select(-rpoB, -inhA, -KatG) |> tbl_summary(by=MDR, 
                          type = AGE ~ "continuous2",
                          statistic = list(
                            AGE ~ c("{mean} ({sd})",
                                    "{min}, {max}")
                          )) |>
  add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)) |>
  add_q() |>
  add_ci()
fig6
fig8 <- mdrtb_lpa2 |> select(Patient_Type, Sex, Smear, MDR)|>
  tbl_summary(by =MDR) |>
  add_difference() |>
  add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE))
fig8s <- fig8 |> as_gt() |>
  gt::gtsave("figure8.png")
fig6s <- fig6 |> as_gt() |>
  gt::gtsave("figure6.png")
fig8
fig7 <- mdrtb_lpa2 |> select(AGE, age_group, MDR) |> tbl_summary(by=MDR, 
                                                                 type = AGE ~ "continuous2",
                                                                 statistic = list(
                                                                   AGE ~ c("{mean} ({sd})",
                                                                           "{min}, {max}")
                                                                 )) |>
  add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)) |>
  add_q() |>
  add_ci()
fig7
fig7s <- fig7 |> as_gt() |>
  gt::gtsave("figure7.png")
mdr_predictors <- mdrtb_lpa2 |> select(Patient_Type, Sex, Smear, MDR) |> tbl_summary(by=MDR) |>
  add_difference() |>
  add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE))
mdr_predictors
mdr_predictorsS <- mdr_predictors |> as_gt() |>
  gt::gtsave("figure8.png")

LPA_mod1 <-mdrtb_lpa2 |> mutate(MDR = case_when(
  MDR == "MDR" ~ 1,
  MDR == "Non-MDR" ~ 0
))

LPA_mod2 <- glm(MDR ~ Patient_Type + Sex + Smear, data = LPA_mod1, family = binomial)
LPA_mod2 
tbl_regression(LPA_mod2)

fig9 <- tbl_regression(LPA_mod2, exponentiate = TRUE)
fig9
fig9s <- fig9 |> as_gt() |>
  gt::gtsave(filename = "figure9.png")


geo_lpa <- mdrtb_lpa %>%
  mutate(MDR = case_when(
    (KatG == "Resistant" & rpoB == "Resistant") |
      (rpoB == 'Resistant' & inhA == 'Resistant') ~ "MDR",
    TRUE ~ "Non-MDR"
  )) %>%
  filter(MDR == "MDR") %>%
  select(Province, District, Facility, MDR)


geo_lpa_summary <- geo_lpa %>%
  group_by(Province) %>%
  summarise(total = n()) %>%
  mutate(pct = (total / sum(total)) * 100)
geo_lpa_summary
shapefile_path <- "/home/odette/MDR_TB/STEVE_ODETTE_GEORGE/shapefiles/"
gdf <- st_read(shapefile_path)

merged_df <- merge(gdf, geo_lpa_summary, by.x = "Name", by.y = "Province")
merged_df$geometry 
map <- ggplot() +
  geom_sf(data = merged_df, aes(fill = pct)) +
  theme_minimal() + plain_theme


print(map)
ggsave(filename = "MDR_map.png", plot = map, width = 10, height = 8, units = "in")
lpa_dst_df <- df1[complete.cases(df1[, c("INH_pDST")]), ]
lpa_dst_df_mdr <- lpa_dst_df |> mutate(
  MDR_pDST = case_when(
    INH_pDST == "Resistant" & RIF_pSDT == "Resistant" ~ "MDR",
    TRUE ~ "Non-MDR"
  ))
lpa_dst_df_mdr <- lpa_dst_df_mdr %>%  filter(MDR_pDST == "MDR") |> select(-DST_1ST_GC_Pyrazinamide) |>
    mutate(MDR_LPA = case_when(
      (KatG == "Resistant" & rpoB == "Resistant") |
        (rpoB == 'Resistant' & inhA == 'Resistant') ~ "MDR",
      TRUE ~ "Non-MDR"
  ))
lpa_dst_df_mdr1 <- lpa_dst_df_mdr |> select(MDR_pDST, MDR_LPA) 
lpa_dst_df_mdr1 |> tbl_summary()
lpa_dst_df_mdr1 
concordant <- sum(lpa_dst_df_mdr1$MDR_pDST == "MDR" & lpa_dst_df_mdr1$MDR_LPA == "MDR")
discordant <- sum(lpa_dst_df_mdr1$MDR_pDST != "MDR" | lpa_dst_df_mdr1$MDR_LPA != "MDR")

venn_obj <- venneuler(c(Concordant = concordant, Discordant = discordant))
plot(venn_obj)
venn.diagram(lpa_dst_df_mdr1,
  names = c("MDR_pDST", "MDR_LPA"),
  fill = c("red", "blue"),
  alpha = 0.7,
  filename = "venn_diagram.pdf"
)
