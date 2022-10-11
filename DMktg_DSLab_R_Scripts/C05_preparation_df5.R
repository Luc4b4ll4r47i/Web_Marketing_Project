#### FIRST LOOK of df_5 ####

str(df_5_camp_cat)
summary(df_5_camp_cat)

#### START CLEANING df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat

#### CLEANING LOW VARIANCE in df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)

#### EXPLORE COLUMNS of df_4 ####

## compute distribution TYP_CAMP
df_5_dist_typcamp <- df_5_camp_cat_clean %>%
  group_by(TYP_CAMP) %>%
  summarize(TOT_CAMPs = n_distinct(ID_CAMP)) %>%
  mutate(PERCENT = TOT_CAMPs/sum(TOT_CAMPs)*100) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_5_dist_typcamp

## plot distribution 
plot_df5_dist_typcamp <- (
  ggplot(data=df_5_dist_typcamp
         , aes(x=TYP_CAMP, y=TOT_CAMPs)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    geom_text(aes(label=round(PERCENT, digits = 2)), position=position_dodge(width=0.9), vjust=-0.15)
)

plot_df5_dist_typcamp

#### FINAL REVIEW df_5_clean ####

str(df_5_camp_cat_clean)
summary(df_5_camp_cat_clean)

