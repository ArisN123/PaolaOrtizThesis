library(tidyverse)
library(broom)
library(knitr)
library(car)
library(psych)
library(lmtest)
library(sandwich)
library(mediation)
library(corrplot)
library(ggcorrplot)
library(tibble)
library(ggplot2)

## table1 is the cleaned table of responses from the survey
long_table <- table1 %>%
  pivot_longer(
    cols = c(FAM1:FAM3, WTB11:WTB33, SE11:SE33, SEA1:SEA3, WTBA1:WTBA3),
    names_to = c("variable", "product", "question"),
    names_pattern = "([A-Z]+)([123])([123]?)"
  ) %>%
  mutate(
    question = ifelse(question == "", NA, question) 
  ) %>%
  pivot_wider(
    names_from = c("variable", "question"),
    values_from = value
  ) %>%
  rename_with(~ gsub("_NA", "", .x)) 

long_table <- long_table %>%
  mutate(
    product = factor(product, levels = c("1", "2", "3")),
    WTBaverage = (WTB_1 + WTB_2 + WTB_3) / 3,
    SEaverage = (SE_1 + SE_2 + SE_3) / 3
  )


long_table <- long_table %>%
  mutate(C2 = as.numeric(gsub("[^0-9.]", "", C2))) %>%
  
  filter(C2 <= 100)




long_table <- long_table %>%
  mutate(
    C1 = factor(C1, ordered = TRUE),
    
    C2 = as.numeric(C2),
    
    C3 = factor(C3),
    
    C4 = factor(C4, ordered = TRUE)
  
  )

long_table <- long_table %>%
  mutate(
    CONNUM = factor(CONNUM),
    
    product = factor(product),
    
    FAM = factor(FAM, ordered = TRUE)
  )

## multicolinarity test

corr_table <- long_table

corr_table$C1 <- as.numeric(as.character(corr_table$C1))
corr_table$C4 <- as.numeric(as.character(corr_table$C4))
corr_table$FAM <- as.numeric(as.character(corr_table$FAM))
corr_table$CONNUM <- as.numeric(as.character(corr_table$CONNUM))

corr_data <- corr_table[, c("WTBA", "SEA", "FAM", "C1", "C2", "C4", "CONNUM")]
cor_matrix <- cor(corr_data, method = "spearman", use = "complete.obs")
round(cor_matrix, 2)


y=cor(round(cor_matrix,2))

corrplot(y, method = 'square', order = 'FPC', type = 'lower', diag = FALSE)


corrplot(y, method = "color", type = "lower", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         diag = FALSE)



mask_upper <- upper.tri(cor_matrix)
cor_matrix_masked <- cor_matrix
cor_matrix_masked[mask_upper] <- NA



colnames(cor_matrix_masked) <- c("Condition", "Education", "Age", 
                                 "Cooking Frequency", "Familiarity", 
                                 "Self Efficacy", "Willingness to Buy")

rownames(cor_matrix_masked) <- colnames(cor_matrix_masked)
ggcorrplot(cor_matrix_masked,
           method = "square",
           type = "full",     
           lab = TRUE,
           lab_col = "black",
           tl.col = "black",
           tl.srt = 90,
           colors = c("blue", "white", "red"),
           ggtheme = theme_minimal())  +
  scale_x_discrete(limits = rev) 





# h1: Consumers willingness to buy will, on average, be higher for products of which the packaging 
#includes a recipe (image + text) compared to a packaging that features only an image.


prod1 <- subset(long_table, product == 1)
prod2 <- subset(long_table, product == 2)
prod3 <- subset(long_table, product == 3)

prod1$CONNUM <- as.numeric(as.character(prod1$CONNUM))
prod2$CONNUM <- as.numeric(as.character(prod2$CONNUM))
prod3$CONNUM <- as.numeric(as.character(prod3$CONNUM))

model_h1_p1 <- lm(WTBA ~ CONNUM + C1 + C2 + C3 + C4, data = prod1)
model_h1_p2 <- lm(WTBA ~ CONNUM + C1 + C2 + C3 + C4, data = prod2)
model_h1_p3 <- lm(WTBA ~ CONNUM + C1 + C2 + C3 + C4, data = prod3)

summary(model_h1_p1)
summary(model_h1_p2)
summary(model_h1_p3)


coeftest(model_h1_p1, vcov = vcovHC(model_h1_p1, type = "HC0"))
coeftest(model_h1_p2, vcov = vcovHC(model_h1_p2, type = "HC0"))
coeftest(model_h1_p3, vcov = vcovHC(model_h1_p3, type = "HC0"))




prod1$FAM_c <- scale(as.numeric(as.character(prod1$FAM)), center = TRUE, scale = FALSE)
prod2$FAM_c <- scale(as.numeric(as.character(prod2$FAM)), center = TRUE, scale = FALSE)
prod3$FAM_c <- scale(as.numeric(as.character(prod3$FAM)), center = TRUE, scale = FALSE)



model_h3_p1 <- lm(WTBA ~ CONNUM * FAM_c + C1 + C2 + C3 + C4, data = prod1)
model_h3_p2 <- lm(WTBA ~ CONNUM * FAM_c + C1 + C2 + C3 + C4, data = prod2)
model_h3_p3 <- lm(WTBA ~ CONNUM * FAM_c + C1 + C2 + C3 + C4, data = prod3)

summary(model_h3_p1)
summary(model_h3_p2)
summary(model_h3_p3)





product_names <- c("Tofu", "Chia Seeds", "Lentils")


#h2



long_table <- long_table %>% filter(as.numeric(as.character(C3)) != 4)
long_table <- long_table %>% filter(as.numeric(as.character(C3)) != 3)

long_table <- long_table %>% filter(as.numeric(as.character(C4)) != 5)
products <- split(long_table, long_table$product)

h2_results <- lapply(products, function(df) {
  df <- df %>% mutate(CONNUM = as.numeric(CONNUM))  
  model_m <- lm(SEA ~ CONNUM + C1 + C2 + C3 + C4, data = df)
  model_y <- lm(WTBA ~ SEA + CONNUM + C1 + C2 + C3 + C4, data = df)
  mediate(model_m, model_y, treat = "CONNUM", mediator = "SEA", boot = TRUE, sims = 5000)
})


product_names <- c("Tofu", "Chia Seeds", "Lentils")

h2_table <- lapply(seq_along(h2_results), function(i) {
  res <- summary(h2_results[[i]])
  data.frame(
    Product = product_names[i],
    ACME_Estimate = round(res$d0, 3),
    CI_Lower = round(res$d0.ci[1], 3),
    CI_Upper = round(res$d0.ci[2], 3),
    p_value = round(res$d0.p, 3),
    Supported = ifelse(res$d0.p < 0.10, "Yes", "No")
  )
}) %>% bind_rows()

print(kable(h2_table, caption = "H2 Results: Mediation via Self-Efficacy (ACME with Bootstrapping)"))

#h4


products <- split(long_table, long_table$product)

h4_results <- lapply(products, function(df) {
  df <- df %>%
    mutate(
      CONNUM = as.numeric(CONNUM),
      FAM = as.numeric(as.character(FAM)),        
      FAM_c = scale(FAM, center = TRUE, scale = FALSE),
      interaction = CONNUM * FAM_c
    )
  
  model_m <- lm(SEA ~ CONNUM + FAM_c + interaction + C1 + C2 + C3 + C4, data = df)
  model_y <- lm(WTBA ~ SEA + CONNUM + C1 + C2 + C3 + C4, data = df)
  
  fam_sd <- sd(df$FAM_c)
  
  df_low <- df %>%
    mutate(FAM_c = -1 * fam_sd, interaction = CONNUM * FAM_c)
  med_low <- mediate(lm(SEA ~ CONNUM + FAM_c + interaction + C1 + C2 + C3 + C4, data = df_low),
                     model_y, treat = "CONNUM", mediator = "SEA", boot = TRUE, sims = 5000)
  
  df_high <- df %>%
    mutate(FAM_c = 1 * fam_sd, interaction = CONNUM * FAM_c)
  med_high <- mediate(lm(SEA ~ CONNUM + FAM_c + interaction + C1 + C2 + C3 + C4, data = df_high),
                      model_y, treat = "CONNUM", mediator = "SEA", boot = TRUE, sims = 5000)
  
  list(low = med_low, high = med_high)
})


product_names <- c("Tofu", "Chia Seeds", "Lentils")


## descriptive stats 


summary_table <- long_table %>%
  mutate(
    CONNUM = as.factor(CONNUM),
    product = recode_factor(product, `1` = "Tofu", `2` = "Chia Seeds", `3` = "Lentils")
  ) %>%
  group_by(product, CONNUM) %>%
  summarise(
    N = n(),
    WTBA_mean = round(mean(WTBA, na.rm = TRUE), 2),
    WTBA_sd = round(sd(WTBA, na.rm = TRUE), 2),
    SEA_mean = round(mean(SEA, na.rm = TRUE), 2),
    SEA_sd = round(sd(SEA, na.rm = TRUE), 2),
    FAM_mean = round(mean(as.numeric(as.character(FAM)), na.rm = TRUE), 2),
    FAM_sd = round(sd(as.numeric(as.character(FAM)), na.rm = TRUE), 2)
  ) %>%
  arrange(product, CONNUM)

kable(summary_table, caption = "Descriptive Statistics by Product and Experimental Condition")


# scale reliability 


alpha_SE_tofu <- psych::alpha(select(prod1, SE_1, SE_2, SE_3))
alpha_SE_chia <- psych::alpha(select(prod2, SE_1, SE_2, SE_3))
alpha_SE_lentils <- psych::alpha(select(prod3, SE_1, SE_2, SE_3))

alpha_SE_all <- psych::alpha(select(long_table, SE_1, SE_2, SE_3))

alpha_WTB_tofu <- psych::alpha(select(prod1, WTB_1, WTB_2, WTB_3))
alpha_WTB_chia <- psych::alpha(select(prod2, WTB_1, WTB_2, WTB_3))
alpha_WTB_lentils <- psych::alpha(select(prod3, WTB_1, WTB_2, WTB_3))

alpha_WTB_all <- psych::alpha(select(long_table, WTB_1, WTB_2, WTB_3))


# rando check

long_table <- long_table %>%
  mutate(
    CONNUM = as.factor(CONNUM),  
    C1 = as.numeric(as.character(C1)),  
    C2 = as.numeric(C2),  
    C3 = as.factor(C3),   
    C4 = as.factor(C4),   
    FAM = as.numeric(as.character(FAM))  
  )

t.test(C2 ~ CONNUM, data = long_table)

wilcox.test(C1 ~ CONNUM, data = long_table)

fisher.test(table(long_table$C3, long_table$CONNUM))

fisher.test(table(long_table$C4, long_table$CONNUM))

wilcox.test(FAM ~ CONNUM, data = long_table)


#correlation analysis

cor_data <- long_table %>%
  select(WTBA, SEA, FAM) %>%
  mutate(FAM = as.numeric(as.character(FAM)))  

cor_matrix <- cor(cor_data, use = "complete.obs", method = "pearson")

cor_pvals <- cor.mtest(cor_data, conf.level = 0.95)$p

ggcorrplot(cor_matrix,
           p.mat = cor_pvals,
           method = "square",
           type = "lower",
           lab = TRUE,
           tl.cex = 12,
           colors = c("blue", "white", "red"),
           ggtheme = theme_minimal())



#assumption checks for OLS
#VIF

vif_table <- car::vif(model_h1_p1)
vif_df <- as.data.frame(vif_table)
vif_df <- rownames_to_column(vif_df, var = "Variable")

vif_df <- vif_df %>%
  mutate(Adjusted = GVIF^(1 / (2 * Df)))

kable(vif_df, caption = "Table 3: VIF for Multicollinearity")

# linearity
par(mfrow = c(3, 4))  
lapply(models, function(m) {
  plot(m, which = 1, main = deparse(substitute(m)))
})



#normality
kable(diagnostics_table[, c("Model", "SW_p", "Norm_OK")],
      caption = "Table 4: Shapiro-Wilk Test for Normality")

# Q-Q plots
par(mfrow = c(3, 4))  
lapply(models, function(m) {
  qqnorm(resid(m)); qqline(resid(m)); title(main = names(models)[[which(models == m)]])
})

#homoscedacity

bp_results <- lapply(models, function(m) bptest(m))

bp_table <- data.frame(
  Model = names(bp_results),
  BP_Statistic = sapply(bp_results, function(x) round(x$statistic, 2)),
  df = sapply(bp_results, function(x) x$parameter),
  p_value = sapply(bp_results, function(x) round(x$p.value, 4)),
  Interpretation = sapply(bp_results, function(x) ifelse(x$p.value < 0.05, "Significant heteroskedasticity", "No heteroskedasticity"))
)

kable(bp_table, caption = "Table 5: Breusch-Pagan Test for Homoscedasticity")




