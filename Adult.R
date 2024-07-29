# Carregando os Dados do arquivo
database <- read.csv(file = "Adult.CSV")

colnames(database) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")

# 1. Identificação do atributo alvo (saída)
target_attribute <- "income"

# 2. Identificação dos tipos de dados dos atributos de entrada (quantitativo, qualitativo)
str(database)

data_types <- c(
  age = "quantitativo",
  workclass = "qualitativo",
  fnlwgt = "quantitativo",
  education = "qualitativo",
  education_num = "quantitativo",
  marital_status = "qualitativo",
  occupation = "qualitativo",
  relationship = "qualitativo",
  race = "qualitativo",
  sex = "qualitativo",
  capital_gain = "quantitativo",
  capital_loss = "quantitativo",
  hours_per_week = "quantitativo",
  native_country = "qualitativo",
  income = "qualitativo"
)
data_types

# 3. Identificação da escala de dados dos atributos de entrada (nominal, ordinal, intervalar, racional)
data_scales <- c(
  age = "Racional",
  workclass = "Nominal",
  fnlwgt = "Racional",
  education = "Ordinal",
  education_num = "Ordinal",
  marital_status = "Nominal",
  occupation = "Nominal",
  relationship = "Nominal",
  race = "Nominal",
  sex = "Nominal",
  capital_gain = "Racional",
  capital_loss = "Racional",
  hours_per_week = "Racional",
  native_country = "Nominal",
  income = "Nominal"
)
data_scales

# 4. Exploração dos dados através de medidas de localidade
numeric_columns1 <- sapply(database, is.numeric)
summary(database[, numeric_columns1])

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

for (i in 1:ncol(database)) {
  mod_val <- Modes(database[,i])
  print(mod_val)
}

# 5. Exploração dos dados através de medidas de espalhamento
spread_measures <- function(x) {
  c(variance = var(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), range = range(x, na.rm = TRUE))
}

# Identifica quais colunas são numéricas.
numeric_columns2 <- sapply(database, is.numeric)
# Aplica a função spread_measures a cada coluna numérica para calcular a variância, desvio padrão e amplitude para cada uma
sapply(database[, numeric_columns2], spread_measures)

# 6. Exploração dos dados através de medidas de distribuição;
# Funções para calcular assimetria e curtose
calc_skewness <- function(x) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  skewness <- sum(((x - mean_x) / sd_x) ^ 3, na.rm = TRUE) / n
  return(skewness)
}

calc_kurtosis <- function(x) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  kurtosis <- sum(((x - mean_x) / sd_x) ^ 4, na.rm = TRUE) / n - 3
  return(kurtosis)
}

# Supondo que 'database' é o seu conjunto de dados
numeric_columns3 <- sapply(database, is.numeric)

# Plotando boxplots para cada variável numérica
boxplot(database$age, main = "Age Boxplot")
boxplot(database$fnlwgt, main = "Fnlwgt Boxplot")
boxplot(database$education_num, main = "Education_num Boxplot")
boxplot(database$capital_gain, main = "capital_gain Boxplot")
boxplot(database$capital_loss, main = "capital_loss Boxplot")
boxplot(database$hours_per_week, main = "hours_per_week Boxplot")

# Calculando assimetria e curtose
skewness_values <- sapply(database[, numeric_columns3], calc_skewness)
kurtosis_values <- sapply(database[, numeric_columns3], calc_kurtosis)

# Criando um dataframe para plotagem
metrics_df <- data.frame(
  Variable = names(skewness_values),
  Skewness = skewness_values,
  Kurtosis = kurtosis_values
)

# Gráfico de assimetria
barplot(metrics_df$Skewness, names.arg = metrics_df$Variable, 
        main = "Assimetria das Variáveis Numéricas", 
        col = "lightblue", ylim = c(min(metrics_df$Skewness) - 1, max(metrics_df$Skewness) + 1),
        cex.names = 0.7, las = 2)

# Gráfico de curtose
barplot(metrics_df$Kurtosis, names.arg = metrics_df$Variable, 
        main = "Curtose das Variáveis Numéricas", 
        col = "lightgreen", ylim = c(min(metrics_df$Kurtosis) - 1, max(metrics_df$Kurtosis) + 1),
        cex.names = 0.7, las = 2)

# 7. Identificação e separação do conjunto de teste
train_data <- read.csv(file = "Adult.CSV")
test_data <- read.csv(file = "adultTest.csv")

colnames(train_data) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")

colnames(test_data) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")

# Identificando colunas numéricas
numeric_columns <- sapply(train_data, is.numeric)

par(mfrow=c(1, 1))
for (col in names(train_data)[numeric_columns]) {
  boxplot(train_data[[col]], test_data[[col]],
          names = c("Train", "Test"),
          main = paste("Boxplot Comparativo para", col),
          col = c("lightblue", "lightgreen"))
}

jitter_horizontal <- function(x) {
  jitter(x, factor = 0.1)
}

# Criando um loop para gerar gráficos de dispersão comparativos
for (col in names(train_data)[numeric_columns]) {
  plot(jitter_horizontal(rep(1, length(train_data[[col]]))), train_data[[col]], 
       xlim = c(0.5, 2.5), xaxt = "n", 
       main = paste("Gráfico de Dispersão Comparativo para", col), 
       xlab = "Conjunto de Dados", ylab = col, col = "blue")
  
  points(jitter_horizontal(rep(2, length(test_data[[col]]))), test_data[[col]], col = "green")
  
  axis(1, at = 1:2, labels = c("Train", "Test"))
  legend("topright", legend = c("Train", "Test"), col = c("blue", "green"), pch = 1)
}

# 8. Identificação e eliminação de atributos não necessários
#é a copia de education
database$education_num <- remove()
#Não é um dado significante para a base de dados
database$fnlwgt <- remove()

# 9. Identificação e eliminação de exemplos não necessários

#Como cada exemplo é importante para o treinamento do modelo de linguagem, a eliminação de exemplos será desconsiderada

# 10. Análise e aplicação de técnicas de amostragem de dados





#database$Work.Class <- ifelse(database$Work.Class == " Private", 0, database$Work.Class)
#database$Work.Class <- ifelse(database$Work.Class == " Self-emp-not-inc", 1, database$Work.Class)
#database$Work.Class <- ifelse(database$Work.Class == " Self-emp-inc", 2, database$Work.Class)
#database$Work.Class <- ifelse(database$Work.Class == " Federal-gov", 3, database$Work.Class)
#database$Work.Class <- ifelse(database$Work.Class == " Local-gov", 4, database$Work.Class)
#database$Work.Class <- ifelse(database$Work.Class == " State-gov" , 5, database$Work.Class)
#database$Work.Class <- ifelse(database$Work.Class == " Never-worked", 6, database$Work.Class)
#database$Work.Class <- ifelse(database$Work.Class == " Without-pay", 7, database$Work.Class)
#database$Work.Class <- ifelse(database$Work.Class == " ?", 8, database$Work.Class)

#database <- subset(database, Work.Class != " ?")

#database$Work.Class

#boxplot(database$Capital.Gain, main= "boxplot capital_gain")
#boxplot(database$Capital.loss, main= "boxplot capital_loss")
