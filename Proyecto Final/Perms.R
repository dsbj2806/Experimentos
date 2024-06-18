grupo <- c("clases repaso", "clases repaso", "clases repaso","clases repaso",
           "clases repaso", "clases repaso", "clases repaso", "clases repaso",
           "clases repaso", "clases repaso", "clases repaso", "clases repaso",
           "clases repaso", "clases repaso", "clases repaso", "clases repaso",
           "clases repaso", "clases repaso", "clases repaso", "clases repaso",
           "clases repaso", "control", "control", "control", "control", "control",
           "control", "control", "control", "control", "control", "control",
           "control", "control", "control", "control", "control", "control",
           "control", "control", "control", "control", "control", "control")
resultado <- c(24, 43, 58, 71, 43, 49, 61, 44, 67, 49,  53, 56, 59, 52, 62,  54,
               57, 33, 46, 43, 57, 26, 62, 37, 42, 43, 55, 54, 20, 85, 33, 41, 19,
               60,  53, 42, 46, 10, 17, 28, 48, 37, 42, 55)

datos <- data.frame(grupo, resultado)

library(dplyr)
distribucion_permut <- rep(NA, 10000)
n_control <- datos %>% filter(grupo == "control") %>% nrow()
n_tratamiento <- datos %>% filter(grupo == "clases repaso") %>% nrow()

for (i in 1:10000) {
  # mezclado aleatorio de las observaciones
  datos_aleatorizados <- sample(datos$resultado) 
  mean_control <- mean(datos_aleatorizados[1:n_control])
  mean_tratamiento <- mean(datos_aleatorizados[n_control + 1:n_tratamiento])
  distribucion_permut[i] <- mean_control - mean_tratamiento
}

media_control <- datos %>% filter(grupo == "control") %>%
  pull(resultado) %>%
  mean()
media_tratamiento <- datos %>% filter(grupo == "clases repaso") %>%
  pull(resultado) %>%
  mean()
dif_obs <- media_control - media_tratamiento
dif_obs

distribucion_permut

p_value = (sum(abs(distribucion_permut) > abs(dif_obs)))/9999
p_value
