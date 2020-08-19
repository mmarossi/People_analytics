df <- read.csv("C:\\Users\\f28553145859\\Documents\\Miriam\\Datasets\\HR_comma_sep.csv")
View(df)
dim(df)
summary(df)
library("dplyr")
turnover <- df %>%
  mutate(
    left = as.factor(left),
    Status = ifelse(left == 1, "Exited", "Active"),
    Work_accident = as.factor(Work_accident),
    promotion_last_5years = as.factor(promotion_last_5years),
    salary = factor(salary, levels = c("low", "medium", "higth"))
  )
install.packages("GGally")
library(GGally)
GGally::ggpairs(turnover)
library(ggplot2)
turnover %>% 
  ggplot() +
  geom_point(aes(x = last_evaluation, y = satisfaction_level, color = Status)) +
  labs(x = "Avaliação de Performance", y = "Nível de Satisfação") +
  scale_color_manual(values = c( "dark blue", "red"))

turnover <- turnover %>% 
  mutate(
    grupos = ifelse(satisfaction_level >= 0.75 & last_evaluation >= 0.8, "Satisfeito | Produtivo",
                    ifelse(satisfaction_level <= 0.12 & last_evaluation >= 0.8, "Insatisfeito | Produtivo",
                           ifelse(satisfaction_level > 0.5 & last_evaluation < 0.8, "Satisfeito | Improdutivo",
                                  ifelse(satisfaction_level < 0.5 & last_evaluation < 0.6, "Insatisfeito | Improdutivo", "Não analisar"))))
  )

turnover %>% 
  filter(!grupos == "Não analisar") %>% 
  ggplot(aes(x = Status, y = number_project)) +
  geom_col(fill = "dark blue") +
  facet_wrap(~grupos)

turnover %>% 
  filter(!grupos == "Não analisar") %>% 
  ggplot(aes(x = Status, y = average_montly_hours)) +
  geom_col(fill = "dark blue") +
  facet_wrap(~grupos)

install.packages("tidymodels")
library(tidymodels)
set.seed(100)
treino_teste <- initial_split(turnover, 0.75)

base_treino <- training(treino_teste)
base_teste <- testing(treino_teste)

# pré processamento 
base_recipe <- base_treino %>% 
  recipe(left~., data = .) %>% 
  step_rm(grupos, Status) %>%  # colunas que nao utilizaremos
  step_zv(all_predictors()) %>% # exclui variáveis com variância = 0 (constantes)
  step_modeimpute(all_nominal(), -all_outcomes()) %>% # coloca a moda nos missings de variáveis nominais
  step_medianimpute(all_numeric()) %>% # coloca a mediana nos missings de variáveis numéricas
  step_novel(all_nominal(), -all_outcomes()) %>% # previni variáveis tipo fator de novas categorias
  step_dummy(all_nominal(), -all_outcomes()) # cria dummys para as variáveis nominais

prep(base_recipe)
head(juice(prep(base_recipe)))

# modelo - regressao logistica
modelo_rl <- logistic_reg(
  penalty = tune(),
  mixture = 1
) %>% 
  set_engine("glmnet")

# validação cruzada
reamostragem <- vfold_cv(base_treino, v = 5)

# worflow
base_wk <- workflow() %>% 
  add_model(modelo_rl) %>% 
  add_recipe(base_recipe)

install.packages("glmnet")
library(glmnet)
tunagem_modelo_rl <- tune_grid(
  base_wk,
  resamples = reamostragem,
  metrics = metric_set(roc_auc, accuracy)
)

# escolha melhor hiperparametro
autoplot(tunagem_modelo_rl)

show_best(tunagem_modelo_rl, metric = "roc_auc")

melhor_hp <- select_best(tunagem_modelo_rl, "roc_auc")

# atualizacao worflow
base_wk <- base_wk %>% 
  finalize_workflow(melhor_hp)

# modelo final
modelo_fit <- last_fit(
  base_wk,
  treino_teste
)

# métricas do modelo
collect_metrics(modelo_fit)

# curva roc
collect_predictions(modelo_fit) %>% 
  roc_curve(left, .pred_1) %>% 
  autoplot()

# modelo final 
modelo_rl_turnover <- fit(base_wk, data = turnover)
