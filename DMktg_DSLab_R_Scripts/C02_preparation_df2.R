#### FIRST LOOK of df_2 ####

str(df_2_cli_account)
summary(df_2_cli_account)

#### START CLEANING df_2 ####

df_2_cli_account_clean <- df_2_cli_account

#### CLEANING DUPLICATE VALUES in df_2 ####

## check for duplicates
df_2_cli_account_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_2 ####

## format boolean as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = as.factor(W_PHONE))

## format numerical categories as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(TYP_CLI_ACCOUNT = as.factor(TYP_CLI_ACCOUNT))

#### CLEANING MISSING VALUES in df_2 ####

## MISSING VALUES mapped as natural values ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = fct_explicit_na(W_PHONE, "0"))

## MISSING VALUES mapped as new level in categorical columns ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%  
  mutate(EMAIL_PROVIDER = fct_explicit_na(EMAIL_PROVIDER, "(missing)")) %>%
  mutate(TYP_JOB = fct_explicit_na(TYP_JOB, "(missing)"))

#### CONSISTENCY CHECK ID_CLI in df_1/df_2 ####

cons_idcli_df1_df2 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_2_cli_account_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_2 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_2) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df2

#!!! NOTE: all ID_CLI in df_1 are also in df_2 and vice-versa !!!#

#### EXPLORE COLUMNS of df_2 ####

### Variable EMAIL_PROVIDER ###

## compute distribution
df_2_dist_emailprovider <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_emailprovider

tot_emailproviders <- n_distinct(df_2_dist_emailprovider$EMAIL_PROVIDER)

tot_emailproviders

#!!! NOTE: too many different values for EMAIL_PROVIDER to be an useful category !!!#

#### ???? TO DO df_2 ???? ####
# COMPUTE THE DISTRIBUTION for the remaining df_2_cli_fid_clean variables

### Variable W_PHONE ###
## compute distribution
df_2_dist_wphone <- df_2_cli_account_clean %>%
  group_by(W_PHONE) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

  df_2_dist_wphone

### Variable TYP_JOB ###
## compute distribution
df_2_dist_job <- df_2_cli_account_clean %>%
  group_by(TYP_JOB) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_job

tot_job <- n_distinct(df_2_dist_job$TYP_JOB)
tot_job

#### RESHAPING df_2 ####

## keep the most frequent EMAIL_PROVIDER values and add a common factor level "OTHER" for the remaining ##
df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  as.data.frame() %>%
  head(20)

## always keep the (missing) level for technical reasons
## select levels that cover the 85% of the cases, the remaining 15% 
clean_email_providers <- df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))

head(clean_email_providers, 20)

## add clean EMAIL_PROVIDER ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))


#### EXPLORE NEW COLUMNS EMAIL_PROVIDER_CLEAN in df_2 ####

## compute distribution
df2_dist_emailproviderclean <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER_CLEAN) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df2_dist_emailproviderclean

## plot distribution
plot_df2_dist_emailproviderclean <- (
  ggplot(data=df2_dist_emailproviderclean
         , aes(x=reorder(EMAIL_PROVIDER_CLEAN, PERCENT), y=TOT_CLIs)) +
    xlab("EMAIL_PROVIDER_CLEAN")+
    geom_bar(stat="identity"
             , fill="steelblue") +
    geom_text(aes(label=round(PERCENT, digits = 2)), position=position_dodge(width=0.9))+
    coord_flip()
)

plot_df2_dist_emailproviderclean

#### ???? TO DO df_2 ???? ####
# EXPLORE the remaining df_2_cli_account_clean relevant variables

## plot distribution W_PHONE
plot_df2_dist_wphone<- (
  ggplot(data=df_2_dist_wphone
         , aes(x=W_PHONE, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    geom_text(aes(label=round(PERCENT, digits = 3)), position=position_dodge(width=0.9), vjust=-0.15)
)

plot_df2_dist_wphone

## plot distribution TYP_JOB
plot_df2_dist_job<- (
  ggplot(data=df_2_dist_job
         , aes(x=TYP_JOB, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue")+
    coord_flip()
)

plot_df2_dist_job

#### FINAL REVIEW df_2_clean ####

str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)