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
summary(database)

# 5. Exploração dos dados através de medidas de espalhamento
spread_measures <- function(x) {
  c(variance = var(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), range = range(x, na.rm = TRUE))
}

# Identifica quais colunas são numéricas.
numeric_columns <- sapply(database, is.numeric)
# Aplica a função spread_measures a cada coluna numérica para calcular a variância, desvio padrão e amplitude para cada uma
sapply(database[, numeric_columns], spread_measures)

# 6. Exploração dos dados através de medidas de distribuição


# 7. Identificação e separação do conjunto de teste
train_data <- read.csv(file = "Adult.CSV")
test_data <- read.csv(file = "adult.test")

# 8. Identificação e eliminação de atributos não necessários
#é a copia de education.num
database$education <- remove()
#Não é um dado significante para a base de dados
database$fnlwgt <- remove()
#pois é atributo alvo
database$Income <- remove()

database <- na.omit(database)

database$Work.Class <- ifelse(database$Work.Class == " Private", 0, database$Work.Class)
database$Work.Class <- ifelse(database$Work.Class == " Self-emp-not-inc", 1, database$Work.Class)
database$Work.Class <- ifelse(database$Work.Class == " Self-emp-inc", 2, database$Work.Class)
database$Work.Class <- ifelse(database$Work.Class == " Federal-gov", 3, database$Work.Class)
database$Work.Class <- ifelse(database$Work.Class == " Local-gov", 4, database$Work.Class)
database$Work.Class <- ifelse(database$Work.Class == " State-gov" , 5, database$Work.Class)
database$Work.Class <- ifelse(database$Work.Class == " Never-worked", 6, database$Work.Class)
database$Work.Class <- ifelse(database$Work.Class == " Without-pay", 7, database$Work.Class)
database$Work.Class <- ifelse(database$Work.Class == " ?", 8, database$Work.Class)

#database <- subset(database, Work.Class != " ?")

database$Work.Class

boxplot(database$Capital.Gain, main= "boxplot capital_gain")
boxplot(database$Capital.loss, main= "boxplot capital_loss")
