# Javier Rivas Moreno
# Proyecto de análisis de datos
# Base de datos obtenida de https://www.kaggle.com/datasets/lainguyn123/student-performance-factors
# La base de datos nos da infromación del rendimiento académico de estudaiantes 

# Importamos la Base de Datos
library(readr)
StudentPerformanceFactors <- read_csv("data/StudentPerformanceFactors.csv")
View(StudentPerformanceFactors)

# Limpiemos los datos
library(dplyr)

Y = c("Hours_Studied",
      "Attendance",
      "Parental_Involvement",
      "Access_to_Resources",
      "Extracurricular_Activities",
      "Sleep_Hours",
      "Previous_Scores",
      "Motivation_Level",
      "Internet_Access",
      "Tutoring_Sessions",
      "Family_Income",
      "Teacher_Quality",
      "School_Type",
      "Peer_Influence",
      "Physical_Activity",
      "Learning_Disabilities",
      "Parental_Education_Level",
      "Distance_from_Home",
      "Exam_Score")
# Actualizamos nuestra base de datos para que solo tome las variables elegidas
All_Data = StudentPerformanceFactors %>% select(all_of(Y))

# La cuestión principal nace en saber de que depende el desempeño de los alumnos
# Para saber si el desempeño de los alumnos fue mejor o peor que en el periodod pasado generamos lo siguiente
# Creamos una nueva columna que sea la diferencia entre el periodo anterior y la puntuación del examen
# De esta manera sabremos si mejoraron (valor positivo) o empeoraron (valor negativo)
# Y de que depende este cambio

# Hagamos la resta y agreguemos la nueva columna
All_Data = All_Data %>%
  mutate(Rendimiento = Exam_Score - Previous_Scores)

All_Data$Rendimiento = All_Data$Exam_Score - All_Data$Previous_Scores


# La cuestión principal nace en saber de que depende el desempeño de los alumnos
# Para saber si el desempeño de los alumnos fue mejor o peor que en el periodod pasado generamos lo siguiente
# Creamos una nueva columna que sea la diferencia entre el periodo anterior y la puntuación del examen
# De esta manera sabremos si mejoraron (valor positivo) o empeoraron (valor negativo)
# Y de que depende este cambio


# Hay datos los cuales son palabras, los cambiaremos a valores numéricos para observar la relación con el Rendimiento
All_Data = All_Data %>%
  mutate(Parental_Involvement = case_when(
    Parental_Involvement == "Low" ~ 1,
    Parental_Involvement == "Medium" ~ 2,
    Parental_Involvement == "High" ~ 3
  ))

All_Data = All_Data %>%
  mutate(Access_to_Resources = case_when(
    Access_to_Resources == "Low" ~ 1,
    Access_to_Resources == "Medium" ~ 2,
    Access_to_Resources == "High" ~ 3
  ))

All_Data = All_Data %>%
  mutate(Extracurricular_Activities = case_when(
    Extracurricular_Activities == "No" ~ 1,
    Extracurricular_Activities == "Yes" ~ 2
  ))

All_Data = All_Data %>%
  mutate(Motivation_Level = case_when(
    Motivation_Level == "Low" ~ 1,
    Motivation_Level == "Medium" ~ 2,
    Motivation_Level == "High" ~ 3
  ))

All_Data = All_Data %>%
  mutate(Internet_Access = case_when(
    Internet_Access == "No" ~ 1,
    Internet_Access == "Yes" ~ 2
  ))

All_Data = All_Data %>%
  mutate(Family_Income = case_when(
    Family_Income == "Low" ~ 1,
    Family_Income == "Medium" ~ 2,
    Family_Income == "High" ~ 3
  ))

All_Data = All_Data %>%
  mutate(Teacher_Quality = case_when(
    Teacher_Quality == "Low" ~ 1,
    Teacher_Quality == "Medium" ~ 2,
    Teacher_Quality == "High" ~ 3,
    TRUE ~ 0
  ))

All_Data = All_Data %>%
  mutate(School_Type = case_when(
    School_Type == "Public" ~ 1,
    School_Type == "Private" ~ 2,
  ))

All_Data = All_Data %>%
  mutate(Peer_Influence = case_when(
    Peer_Influence == "Negative" ~ 1,
    Peer_Influence == "Neutral" ~ 2,
    Peer_Influence == "Positive" ~ 3
  ))

All_Data = All_Data %>%
  mutate(Learning_Disabilities = case_when(
    Learning_Disabilities == "Yes" ~ -1,
    Learning_Disabilities == "No" ~ 1
  ))

All_Data = All_Data %>%
  mutate(Parental_Education_Level = case_when(
    Parental_Education_Level == "High School" ~ 1,
    Parental_Education_Level == "Collage" ~ 2,
    Parental_Education_Level == "Postgraduate" ~ 3,
    TRUE ~ 0
  ))

All_Data = All_Data %>%
  mutate(Distance_from_Home = case_when(
    Distance_from_Home == "Far" ~ 1,
    Distance_from_Home == "Moderate" ~ 2,
    Distance_from_Home == "Near" ~ 3,
    TRUE ~ 0
  ))

# Ahora veamos la correlación del rendimiento con los demás datos
# Esto lo queremos ordenado entre mayor sea la relación entre los datos
# Lista ordenada con correlaciones más relevantes con el Rednimiento o con el Exam_Score
Totales = list()  # Usamos una lista para almacenar tanto el mensaje como el valor de la correlación

referencia = "Exam_Score" # Aquí podemos colocar Rendimiento o Exam_Score para observar los que coincidan

for (columna in colnames(All_Data)) {
  if (columna != referencia) {
    # Calculamos la correlación entre la columna de referencia y la columna actual
    correlacion = cor(All_Data[[referencia]], All_Data[[columna]])
    
    # Creamos el mensaje con el resultado
    mensaje = paste("Relación de", columna, "con", referencia, "es:", correlacion)
    
    # Almacenamos el valor de la correlación y el mensaje en la lista
    Totales[[columna]] = list(correlacion = correlacion, mensaje = mensaje)
  }
}

# Convertimos la lista en un dataframe para facilitar el ordenamiento
df_totales <- data.frame(
  columna = names(Totales),
  correlacion = sapply(Totales, function(x) x$correlacion),
  mensaje = sapply(Totales, function(x) x$mensaje),
  stringsAsFactors = FALSE
)

# Ordenamos el dataframe por el valor absoluto de las correlaciones en orden descendente
df_totales_ordenado = df_totales[order(abs(df_totales$correlacion), decreasing = TRUE), ]

# Mostrar los resultados ordenados
print(df_totales_ordenado$mensaje)


# De los resultados observamos que las áreas con mayor correlación con los resultados en los examenes son
# 1.- Attendance 
# 2.- Hours_Studied 
# 3.- Previous_Scores 

# Observemos estas en gráficas
# Attendance
plot(All_Data$Attendance,All_Data$Exam_Score,
     main = "Relación entre porcentaje de asistencia y resultados del último examen",
     xlab = "Porcentaje de asistencia",
     ylab = "Resultados del último examen",
     pch = 19)

abline(lm(Exam_Score~Attendance, data = All_Data), col = "red")

# Hours_Studied
plot(All_Data$Hours_Studied,All_Data$Exam_Score,
     main = "Relación entre horas de estudio y resultados del último examen",
     xlab = "Horas de estudio",
     ylab = "Resultados del último examen",
     pch = 19)

abline(lm(Exam_Score~Hours_Studied, data = All_Data), col = "blue")

# Previous_Scores
plot(All_Data$Hours_Studied,All_Data$Exam_Score,
     main = "Relación entre resultados anteriores y resultados del último examen",
     xlab = "Resultados anteriores",
     ylab = "Resultados del último examen",
     pch = 19)

abline(lm(Exam_Score~Previous_Scores, data = All_Data), col = "green")

# Conclusión
# Los alumnos que mas asistieron a clases fueron más propbables de obtener una mejor calificación y mejorar su examen
# Así también los que dedicaban más horas de estudio a sus materias
# Observamos que se mantiene una relación con lo obtenido en periodos pasados con las calificaciones actuales haciendo alución a que a mayoría mantienen sus aclificaciones
# Fuera de esto muchos de los datos no tiene una relación directa con los resultados mostrando que hay variaciones, pero que deben considerarse para estudios particulares



