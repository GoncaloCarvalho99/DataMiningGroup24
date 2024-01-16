install.packages("posterdown")

# Install and load necessary libraries
install.packages("car")
install.packages("lmtest")
install.packages("sandwich")
install.packages("tidyverse")
install.packages("readxl")
install.packages("writexl")

library(car)
library(lmtest)
library(sandwich)
library(tidyverse)  
library(readxl)
library(writexl)


# Load the dataset
#billionaires <- read_excel("C:/Users/maria/OneDrive/Documentos/_NOVA/1st sem/Statistics/Project/shark_tank.xlsx")
billionaires <- read_excel("C:/Users/maria/OneDrive/Documentos/_NOVA/1st sem/Statistics/Project/billionaires.xlsx")

# Explore the structure of the dataset
str(billionaires)

# Summary statistics
summary(billionaires)

# Check for missing values
colSums(is.na(billionaires[,]))

# Distribution of key numerical variables
#hist(billionaires$valuation, main = "Distribution of Valuation", xlab = "Valuation")

# Boxplot to identify outliers
#boxplot(billionaires$valuation, main = "Boxplot of Valuation")

# Explore relationships between variables
#cor(billionaires$valuation, billionaires$askedfor)

# Visualize relationships with scatter plots
#plot(billionaires$valuation, billionaires$askedfor, main = "Scatter Plot of Valuation vs Asked For", 
#     xlab = "Valuation", ylab = "Asked For")

# Visualize symmetry
#boxplot(billionaires$askedfor,
#        billionaires$exchangeforstake,
#       names=c("Asked For", "Stake"), col = c("lightgreen","orange"))



var <- c("gender", "selfMade", "age", "cpi_country", "cpi_change_country", "gdp_country", "total_tax_rate_country", "population_country")

# Converta as colunas para o formato 'numeric'
billionaires$cpi_country <- as.numeric(billionaires$cpi_country)
billionaires$cpi_change_country <- as.numeric(billionaires$cpi_change_country)
billionaires$gdp_country <- as.numeric(gsub("[\\$,]", "", billionaires$gdp_country))
billionaires$total_tax_rate_country <- as.numeric(billionaires$total_tax_rate_country)

summary(billionaires[var])



# Missing Values

# Check missing values for each variable
colSums(is.na(billionaires[,var]))

# missing values: age (65), cpi_country(184), cpi_change_country(184), gdp_country(164), total_tax_rate_country (182), population_country (164)

# List of specific countries to delete
countries_to_delete <- c("Andorra", "Bahamas", "Bermuda", "British Virgin Islands", "Cayman Islands", "Eswatini (Swaziland)", "Guernsey", "Hong Kong", "Ireland", "Liechtenstein", "Monaco", "Taiwan", "Turks and Caicos Islands", "Uzbekistan")

# Create a new dataset without rows containing the specified countries and rows with missing 'country'
billionaires_filtered <- subset(billionaires, !(country %in% countries_to_delete) & complete.cases(country))

colSums(is.na(billionaires_filtered[,var]))

total_rows_original <- nrow(billionaires)
total_rows_filtered <- nrow(billionaires_filtered)
# Calculate the percentage of rows deleted
percentage_rows_deleted <- ((nrow(billionaires) - total_rows_filtered) / total_rows_original)

cat("Number of rows in the original dataset:", total_rows_original, "\n")
cat("Number of rows in the filtered dataset:", total_rows_filtered, "\n")
cat("Percentage of rows deleted:", percentage_rows_deleted, "%\n")

# Replace 'age' missing values with the median
billionaires_filtered$age[is.na(billionaires_filtered$age)] <- median(billionaires_filtered$age, na.rm = TRUE)

colSums(is.na(billionaires_filtered[,var]))



# Plots

# Explore relationships between variables

# Scatter plot for valuation and askedfor
#plot(billionaires$valuation, billionaires$askedfor, main = "Scatter Plot of Valuation vs Asked For", 
#     xlab = "Valuation", ylab = "Asked For")

#plot(x = lvaluation, y = laskedfor, main = "Scatter Plot of Valuation vs Asked For", 
#     xlab = "Log Valuation", ylab = "Log Asked For")

# Barplot
#hist(valuation, nclass=20)
#hist(lvaluation, nclass=10)
#hist(askedfor, nclass=20)
#hist(laskedfor, nclass=20)
#hist(stake, nclass=20)
#hist(log(stake), nclass=20)

# Boxplot to compare valuation by category
#boxplot(billionaires$valuation ~ billionaires$category, main = "Boxplot of Valuation by Category", 
#        xlab = "Category", ylab = "Valuation")

# Cross-tabulation for multiple_entreprenuers and deal
#table(billionaires$multiple_entreprenuers, billionaires$deal)

# Correlation matrix for numeric variables
#cor_matrix <- cor(billionaires[, c("valuation", "askedfor", "exchangeforstake")])
#print(cor_matrix)

# Heatmap for correlation matrix
#heatmap(cor_matrix, annot = TRUE, main = "Correlation Heatmap")

# Explore the distribution of the dependent variable
#table(billionaires$deal)

# Bar plot for the distribution of categories
#barplot(table(billionaires$category), main = "Distribution of Categories", 
#        xlab = "Category", ylab = "Frequency", col = "skyblue")



# Model

# Variables
worth <- billionaires$finalWorth
gender <- billionaires$gender
selfmade <- billionaires$selfMade
age <- billionaires$age
cpi <- billionaires$cpi_country
cpi_change <- billionaires$cpi_change_country
gdp <- billionaires$gdp_country
tax_rate <- billionaires$total_tax_rate_country
pop <- billionaires$population_country


model <- lm(worth ~ gender + selfmade + age + cpi + cpi_change + gdp + tax_rate + pop, data = billionaires)
summary(model)



# Hypothesis Testing

# 1. Linearity Test (Visual inspection with residuals vs. fitted values plot)
plot(model, which = 1, main = "Linearity Test: Residuals vs Fitted")

# Interpretation: Se houver um padrão discernível ou não linearidade no gráfico, a suposição de linearidade pode ser violada.


# 2. Independence of Errors (Durbin-Watson Test)
dw_test <- durbinWatsonTest(model)

# Interpretation: O teste de Durbin-Watson verifica a autocorrelação nos resíduos. Um valor próximo a 2 sugere independência, enquanto valores significativamente diferentes podem indicar autocorrelação.

# 3. Homoscedasticity Test (Visual inspection with scale-location plot)
plot(model, which = 3, main = "Homoscedasticity Test: Scale-Location")

# Interpretation: Se houver um padrão discernível ou uma mudança na dispersão com os valores ajustados, a homoscedasticidade pode ser violada.

# Além disso, você pode realizar testes formais para homoscedasticidade, como o teste Breusch-Pagan ou White.

# Teste de Breusch-Pagan para Homoscedasticidade
bp_test <- bptest(model)
print(bp_test)

# Interpretation: Um valor de p próximo de 1 indica homoscedasticidade. Um valor significativamente diferente pode indicar heteroscedasticidade.

# Teste de White para Homoscedasticidade
white_test <- white.test(model)
print(white_test)

# Interpretation: O teste White também verifica homoscedasticidade. A estatística de teste e o p-valor são usados para avaliar a homoscedasticidade.







logistic_model <- glm(deal ~ lvaluation + laskedfor + lstake + multi_entrep, family='binomial', data = billionaires)
logistic_model

summary(logistic_model)

anova(logistic_model, test = "Chisq")

vif(logistic_model)

logistic_model$fitted.values

plot(logistic_model$fitted.values, residuals(logistic_model, type = "deviance"))

residuals <- residuals(logistic_model, type = "deviance")
plot(logistic_model, which = 1)  # Residuals vs Fitted
plot(logistic_model, which = 2)  # Normal Q-Q plot






# 1. Linearity of the model (Visually inspect with a residuals vs. fitted values plot)
plot(model, which = 1)

# 2. Amostragem Aleatória

# 3. Variabilidade dos regressores (variáveis explicativas)

# 4. Ausência de multicolinearidade perfeita

# 5. Número de observações > nº de variáveis

# 6. O erro (u) tem média nula
# o valor esperado é 0.
# violação da hipótese: enviesamento do termo autónomo e do declive

# 7. Homocedasticidade
#     - outliers, modelo incorretamente especificado, assimetria da distribuição de um ou + regressores
#     - heterocedasticidade: estimadores OLS ineficientes
#     - analisar a relação entre os residuos e cada uma das var explicativas
#     - BP test, White test, White (special) test

# 8. Ausência de autocorrelação entre os erros
#     - mais em séries temporais
#     - Durbin-Watson test, Breush-Godfrey

# 9. Ausência de correlação entre as var explicativas e o erro (cov=0)
#     - endogeneidade, VI
#     - Hausman test

# 10. O termo de erro tem distribuição normal.
#     - histograma dos resíduos
#     - test de Jarque-Bera


# Incorreta especificação do modelo: Formal funcional errada

# Incorreta especificação do modelo: Erros de medição nas variáveis



# 2. Independence of errors (Check Durbin-Watson statistic)
durbinWatsonTest(model)

# 3. Homoscedasticity (Visually inspect with a scale-location plot)
plot(model, which = 3)

# 4. No perfect multicollinearity (Check variance inflation factors - VIF)
library(car)
vif(model)

# 5. No endogeneity of regressors (Consider instrumental variables if applicable)

# 6. Normally distributed errors (Check normality of residuals with a quantile-quantile plot)
qqnorm(resid(model))
qqline(resid(model))

# Additionally, a formal test for normality can be performed
shapiro.test(resid(model))
