library(readxl)
library(utils)
library(tidyverse)
library(lme4)
library(DHARMa)
library(plotly)
library(MuMIn)
library(stargazer)



# reading data

vac <- read_xlsx("data/IAB-Stellenerhebung_Indikatoren.xlsx",
 skip = 13, 
 sheet = 3, 
 col_names = FALSE  )



unempl <- read.csv2("data/arbeitslose_deutschland_originalwert.csv", skip = 2)

vac <- vac[vac[[1]] >= 2011, ]
vac[[1]] <- gsub("\\*", "", vac[[1]])
vac[[1]] <- as.integer(vac[[1]])

vac <- vac[ , 1:5]

colnames(vac) <- c("year", "quarter", "total_vac", "west", "east")




unempl[[1]] <- as.Date(unempl[[1]], format = "%d/%m/%Y")
colnames(unempl)<- c("date", "total_unemp", "men", "women", "youth", "long_time")

unempl <- unempl %>%
  mutate(
    date = as.Date(date, format = "%d/%m/%Y"),  # falls noch nicht als Date
    year = as.integer(year(date)),
    quarter = quarter(date)
  )

unempl2 <- aggregate(unempl[ , 2:6], 
  by = list(year = unempl$year, quarter = unempl$quarter), 
  FUN = mean, 
  na.rm = TRUE  ) 

df_emp <- vac %>%
  inner_join(unempl2, by = c("year", "quarter"))




# simple linear regression

m1 <- lm(total_unemp ~ total_vac, data = df_emp)
summary(m1)

# PLot m1

ggplot(df_emp, aes(x = total_vac, total_unemp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

df_plot <- data.frame(
  fitted = m1$fitted.values,
  residuals = m1$residuals
)

#### residual vs fitted OLS
ggplot(df_plot, aes(x = fitted, y = residuals)) +
  geom_point(color = "blue") +                       
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  theme_minimal()

# random intercept model
m2 <- lmer(total_unemp ~ total_vac + (1 | year), data = df_emp)
summary(m2)


## R2 Schätzung
m2_r2 <- r.squaredGLMM(m2)



## Shapiro-Wilk-Test 
shapiro.test(resid(m2))

## test heteroscedasticity
sim_res <- simulateResiduals(fittedModel = m2)

testDispersion(sim_res)
testUniformity(sim_res)




random_plot <- df_emp %>%
  mutate(
    fitted = fitted(m2),                       
    fitted_fixed = predict(m2, re.form = NA)   
  )

## PLot the model

m2_plot <- ggplot(random_plot, aes(x = total_vac, y = total_unemp, color = factor(year))) +
  geom_point(alpha = 0.6) +  
  geom_line(aes(y = fitted, group = year, color = factor(year))) +  # Linien pro Jahr
  geom_line(aes(y = fitted_fixed), color = "gold", linewidth = 1) +   # Gesamtregressionslinie
  labs(title = "Random Intercept Model: Unemployed vs. Vacancies",
       x = "Total Vacancies",
       y = "Total Unemployed",
       color = "Year") +
  theme(
    panel.background = element_rect(fill = "black"), # Panel schwarz
    plot.background = element_rect(fill = "black"),  # Gesamter Plot schwarz
    panel.grid.major = element_line(color = "grey30"), 
    panel.grid.minor = element_line(color = "grey20"),
    axis.text = element_text(color = "white"),       # Achsenticks weiß
    axis.title = element_text(color = "white"),      # Achsentitel weiß
    plot.title = element_text(color = "white"),       # Titel weiß
    legend.background = element_rect(fill = "black"),# Legendenhintergrund schwarz
    legend.key = element_rect(fill = "black"),       # Legendenfelder schwarz
    legend.text = element_text(color = "white"),     # Legendentext weiß
    legend.title = element_text(color = "white"),     # Legendentitel weiß
   # legend.key.size = unit(0.3, "cm")
  )

m2_plotly <- ggplotly(m2_plot)
htmlwidgets::saveWidget(m2_plotly, "RandomInterceptPlot.html")



#### Residual vs fitted random effects 1

df_plot2 <- data.frame(
  fitted = fitted(m2),       # fitted values aus lmer
  residuals = resid(m2)      # Residuen
)
hist(df_plot2$residuals)



ggplot(df_plot2, aes(x = fitted, y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted (Random Effects Modell 1)",
       x = "Fitted values",
       y = "Residuals") +
  theme_minimal()


### Regressions table for M2


boot <- bootMer(m2,
                FUN = function(x) fixef(x),
                nsim = 2000)   # Anzahl Simulationen

# 95%-CI für die Fixed Effects
m2_ci <- as.matrix(t(apply(boot$t, 2, function(x) quantile(x, c(0.025, 0.975))))  )

stargazer(m2,
          type = "html",
          ci = TRUE,
          ci.custom = list(m2_ci),
          ci.level = 0.95,
          digits = 3,
          star.cutoffs = NA,      
          add.lines = list(
            c("Marginal R²", round(m2_r2[1, "R2m"], 3)),
            c("Conditional R²", round(m2_r2[1, "R2c"], 3))
          ),
          title = "Random Ontercept Regression",
          notes = "Coefficients and 95% confidence intervals estimated using 2000 bootstrap resamples",
          notes.append = FALSE,
          out = "reg table.html")

### Trend Plot

df_emp <- df_emp |> 
  mutate(time = year + quarter/4)

sub_emp1 <- df_emp |> filter(time <= 2024.25)
sub_emp2 <- df_emp |> filter(time >= 2024.25) 

trend_plot <- ggplot() +
  geom_line(data = df_emp, aes(x = time, y = total_unemp, color = "Unemployment")) +
  geom_line(data = sub_emp1, aes(x = time, y = total_vac, color = "Vacancies")) +
  geom_line(data = sub_emp2, aes(x = time, y = total_vac, color = "Vacancies (estimate IAB)")) +
  scale_color_manual(values = c("Unemployment" = "red", 
                                "Vacancies" = "blue",
                                "Vacancies (estimate IAB)" = "purple")) +
  labs(x = "year", y = "total value", color = "Variable",
       title = "Unemployment and Vacancies Trend",
       subtitle = "Values ​​in thousands",
       caption = "Source: Bundesagentur für Arbeit,\n
       Institut für Arbeitsmarkt- und Berfusforschung") +
  theme(
    panel.background = element_rect(fill = "black"), # Panel schwarz
    plot.background = element_rect(fill = "black"),  # Gesamter Plot schwarz
    panel.grid.major = element_line(color = "grey30"), 
    panel.grid.minor = element_line(color = "grey20"),
    axis.text = element_text(color = "white"),       # Achsenticks weiß
    axis.title = element_text(color = "white"),      # Achsentitel weiß
    plot.title = element_text(color = "white"),       # Titel weiß
    legend.background = element_rect(fill = "black"),# Legendenhintergrund schwarz
    legend.key = element_rect(fill = "black"),       # Legendenfelder schwarz
    legend.text = element_text(color = "white"),     # Legendentext weiß
    legend.title = element_text(color = "white"),     # Legendentitel weiß
   # legend.key.size = unit(0.3, "cm")
  )
  

 trend_plotly <- ggplotly(trend_plot) %>% 
                layout(
                  title = list(
                    text = "Unemployment and Vacancies Trend<br><sup>Values in thousands</sup>"
                  )
                )

htmlwidgets::saveWidget(trend_plotly, "Trend.html")