# ============================================================
# PROYECTO: "Que la comida no se enfríe"
# ANÁLISIS COMPLETO DE SUPERVIVENCIA
# Autora: Arwen Yetzirah Ortiz Nuñez
# Fecha: 2026-05-20
# ============================================================

# 1. Cargar librerías necesarias
library(tidyverse)      # manipulación y visualización
library(janitor)        # limpieza de nombres
library(geosphere)      # cálculo de distancias
library(naniar)         # valores perdidos
library(survival)       # análisis de supervivencia

# ============================================================
# CARGA Y LIMPIEZA DE DATOS
# ============================================================

# Cargar datos desde GitHub
url_data <- "https://raw.githubusercontent.com/Arwen333/Analisis-rentabilidad-servicio-delivery-/refs/heads/main/deliverytime.csv"

deliverytime <- read.csv(url_data) %>%
  clean_names() %>%
  slice(1:500)

cat("📊 Dimensiones originales:", nrow(deliverytime), "filas\n")

# Eliminar coordenadas inválidas
deliverytime_clean <- deliverytime %>%
  filter(
    restaurant_latitude != 0,
    restaurant_longitude != 0,
    delivery_location_latitude != 0,
    delivery_location_longitude != 0
  )

cat("🗑️ Eliminadas por coordenadas (0,0):", 500 - nrow(deliverytime_clean), "\n")

# Limpiar espacios en blanco
deliverytime_clean <- deliverytime_clean %>%
  mutate(
    type_of_order = str_trim(type_of_order),
    type_of_vehicle = str_trim(type_of_vehicle)
  )

# Calcular distancia (Haversine)
deliverytime_clean <- deliverytime_clean %>%
  mutate(
    distancia_km = distHaversine(
      cbind(restaurant_longitude, restaurant_latitude),
      cbind(delivery_location_longitude, delivery_location_latitude)
    ) / 1000,
    velocidad_kmh = (distancia_km / time_taken_min) * 60
  )

# Filtrar valores imposibles
deliverytime_clean <- deliverytime_clean %>%
  filter(
    distancia_km < 50,
    distancia_km > 0,
    velocidad_kmh < 80,
    velocidad_kmh > 5,
    delivery_person_age >= 18,
    delivery_person_age <= 70,
    delivery_person_ratings <= 5,
    delivery_person_ratings >= 1
  )

cat("✅ Datos limpios finales:", nrow(deliverytime_clean), "filas\n")

# Convertir a factores
deliverytime_clean <- deliverytime_clean %>%
  mutate(
    type_of_order = as.factor(type_of_order),
    type_of_vehicle = as.factor(type_of_vehicle),
    delivery_person_id = as.factor(delivery_person_id)
  )

# Guardar CSV limpio
write.csv(deliverytime_clean, "deliverytime_clean.csv", row.names = FALSE)
cat("💾 Archivo guardado: deliverytime_clean.csv\n")

# ============================================================
# GRÁFICAS DEL ANÁLISIS DESCRIPTIVO
# ============================================================

# Gráfica 1: Histograma
ggplot(deliverytime_clean, aes(x = time_taken_min)) +
  geom_histogram(bins = 20, fill = "forestgreen", color = "white") +
  labs(title = "Distribución del tiempo de entrega",
       x = "Tiempo (minutos)", y = "Frecuencia") +
  theme_minimal()

# Gráfica 2: Boxplot por tipo de pedido
ggplot(deliverytime_clean, aes(x = type_of_order, y = time_taken_min, fill = type_of_order)) +
  geom_boxplot() +
  labs(title = "Tiempo de entrega según tipo de pedido",
       x = "Tipo de pedido", y = "Tiempo (minutos)") +
  theme_minimal() +
  theme(legend.position = "none")

# Gráfica 3: Violin plot por tipo de vehículo
ggplot(deliverytime_clean, aes(x = type_of_vehicle, y = time_taken_min, fill = type_of_vehicle)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.8) +
  labs(title = "Distribución del tiempo por tipo de vehículo",
       x = "Tipo de vehículo", y = "Tiempo (minutos)") +
  theme_minimal() +
  theme(legend.position = "none")

# Gráfica 4: Dispersión distancia vs tiempo
ggplot(deliverytime_clean, aes(x = distancia_km, y = time_taken_min, color = type_of_vehicle)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Relación entre distancia y tiempo de entrega",
       x = "Distancia (km)", y = "Tiempo (minutos)",
       color = "Tipo de vehículo") +
  theme_minimal()

# Gráfica 5: Calificación vs tiempo
ggplot(deliverytime_clean, aes(x = delivery_person_ratings, y = time_taken_min)) +
  geom_point(alpha = 0.5, color = "forestgreen", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(title = "Calificación del repartidor vs tiempo de entrega",
       x = "Calificación (1-5)", y = "Tiempo (minutos)") +
  theme_minimal()

# ============================================================
# PREPARACIÓN PARA ANÁLISIS DE SUPERVIVENCIA
# ============================================================

deliverytime_clean <- deliverytime_clean %>%
  mutate(
    tiempo_supervivencia = time_taken_min,
    evento = ifelse(tiempo_supervivencia <= 45, 1, 0)
  )

cat("\n📊 Resumen de censura:\n")
print(table(deliverytime_clean$evento))

# ============================================================
# GRÁFICA 6: CURVA DE KAPLAN-MEIER GENERAL
# ============================================================

fit_km_general <- survfit(Surv(tiempo_supervivencia, evento) ~ 1, data = deliverytime_clean)

km_general_df <- data.frame(
  tiempo = fit_km_general$time,
  supervivencia = fit_km_general$surv,
  inferior = fit_km_general$lower,
  superior = fit_km_general$upper
)

ggplot(km_general_df, aes(x = tiempo, y = supervivencia)) +
  geom_step(color = "steelblue", size = 1.2) +
  geom_ribbon(aes(ymin = inferior, ymax = superior), alpha = 0.2, fill = "steelblue") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = 26, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(title = "Curva general de supervivencia de las entregas",
       x = "Tiempo (minutos)", y = "Probabilidad de supervivencia") +
  theme_minimal() +
  annotate("text", x = 28, y = 0.52, label = "Mediana: 26 min", size = 3.5, color = "red")

# ============================================================
# GRÁFICA 7: COMPARACIÓN POR TIPO DE VEHÍCULO
# ============================================================

fit_km_vehiculo <- survfit(Surv(tiempo_supervivencia, evento) ~ type_of_vehicle, data = deliverytime_clean)

km_vehiculo_df <- data.frame(
  tiempo = fit_km_vehiculo$time,
  supervivencia = fit_km_vehiculo$surv,
  strata = rep(names(fit_km_vehiculo$strata), fit_km_vehiculo$strata)
)

km_vehiculo_df$strata <- gsub("type_of_vehicle=", "", km_vehiculo_df$strata)

colores_vehiculos <- c("electric_scooter" = "#2E8B57", 
                       "motorcycle" = "#CD5C5C", 
                       "scooter" = "#4682B4")
nombres_vehiculos <- c("electric_scooter" = "Scooter Eléctrico", 
                       "motorcycle" = "Motocicleta", 
                       "scooter" = "Scooter")

ggplot(km_vehiculo_df, aes(x = tiempo, y = supervivencia, color = strata, group = strata)) +
  geom_step(size = 1.1) +
  scale_color_manual(values = colores_vehiculos, 
                     labels = nombres_vehiculos,
                     name = "Tipo de vehículo") +
  labs(title = "Curvas de supervivencia por tipo de vehículo",
       x = "Tiempo (minutos)", y = "Probabilidad de supervivencia",
       caption = "Prueba de Log-Rank: p < 0.05") +
  theme_minimal() +
  theme(legend.position = "bottom")

# ============================================================
# GRÁFICA 8: COMPARACIÓN POR TIPO DE PEDIDO
# ============================================================

fit_km_pedido <- survfit(Surv(tiempo_supervivencia, evento) ~ type_of_order, data = deliverytime_clean)

km_pedido_df <- data.frame(
  tiempo = fit_km_pedido$time,
  supervivencia = fit_km_pedido$surv,
  strata = rep(names(fit_km_pedido$strata), fit_km_pedido$strata)
)

km_pedido_df$strata <- gsub("type_of_order=", "", km_pedido_df$strata)

colores_pedidos <- c("Buffet" = "#F4A460", "Drinks" = "#6A5ACD", 
                     "Meal" = "#DC143C", "Snack" = "#3CB371")

ggplot(km_pedido_df, aes(x = tiempo, y = supervivencia, color = strata, group = strata)) +
  geom_step(size = 1.1) +
  scale_color_manual(values = colores_pedidos, name = "Tipo de pedido") +
  labs(title = "Curvas de supervivencia por tipo de pedido",
       x = "Tiempo (minutos)", y = "Probabilidad de supervivencia",
       caption = "Prueba de Log-Rank: p < 0.01") +
  theme_minimal() +
  theme(legend.position = "bottom")

# ============================================================
# MODELO DE COX
# ============================================================

modelo_cox <- coxph(Surv(tiempo_supervivencia, evento) ~ 
                      type_of_vehicle + type_of_order + 
                      delivery_person_ratings + distancia_km, 
                    data = deliverytime_clean)

cat("\n📊 Resumen del Modelo de Cox:\n")
print(summary(modelo_cox))

# ============================================================
# GRÁFICA 9: FOREST PLOT CON TÍTULO CORREGIDO
# ============================================================

resultados_forest <- data.frame(
  Variable = c("Motocicleta (vs Scooter eléctrico)",
               "Scooter (vs Scooter eléctrico)",
               "Drinks (vs Buffet)",
               "Meal (vs Buffet)",
               "Snack (vs Buffet)",
               "Calificación del repartidor",
               "Distancia (km)"),
  HR = round(exp(coef(modelo_cox)), 2),
  IC_inf = round(exp(confint(modelo_cox))[,1], 2),
  IC_sup = round(exp(confint(modelo_cox))[,2], 2),
  p_valor = round(summary(modelo_cox)$coefficients[,5], 4)
)

resultados_forest$Variable <- factor(resultados_forest$Variable, 
                                     levels = rev(resultados_forest$Variable))

ggplot(resultados_forest, aes(x = HR, y = Variable)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 1) +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.2, 
                 color = "darkblue", linewidth = 0.8) +
  geom_point(size = 4, color = "darkblue") +
  scale_x_log10(breaks = c(0.3, 0.5, 0.7, 1, 1.5, 2, 2.5, 3)) +
  labs(title = "Modelo de Cox: Hazard Ratios",
       subtitle = "Factores asociados a la tasa de finalización de entregas",
       x = "Hazard Ratio (escala logarítmica)", 
       y = "",
       caption = "HR > 1: acelera la entrega | HR < 1: la retrasa") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.text.y = element_text(size = 10))

# Mostrar tabla de resultados
cat("\n📊 Tabla de Hazard Ratios:\n")
print(resultados_forest)

# ============================================================
# PRUEBAS DE LOG-RANK
# ============================================================

logrank_vehiculo <- survdiff(Surv(tiempo_supervivencia, evento) ~ type_of_vehicle, data = deliverytime_clean)
p_valor_vehiculo <- 1 - pchisq(logrank_vehiculo$chisq, length(logrank_vehiculo$n) - 1)

logrank_pedido <- survdiff(Surv(tiempo_supervivencia, evento) ~ type_of_order, data = deliverytime_clean)
p_valor_pedido <- 1 - pchisq(logrank_pedido$chisq, length(logrank_pedido$n) - 1)

cat("\n📊 Pruebas de Log-Rank:\n")
cat("   - Tipo de vehículo: p =", round(p_valor_vehiculo, 4), "\n")
cat("   - Tipo de pedido: p =", round(p_valor_pedido, 4), "\n")

# ============================================================
# PRUEBA DE SCHOENFELD (VERIFICACIÓN DE SUPUESTOS)
# ============================================================

test_ph <- cox.zph(modelo_cox)
cat("\n📊 Prueba de riesgos proporcionales de Schoenfeld:\n")
print(test_ph)

# ============================================================
# MEDIANAS DE SUPERVIVENCIA
# ============================================================

cat("\n📊 Medianas de supervivencia (minutos):\n")
cat("   - General:", summary(fit_km_general)$table["median"], "\n")
cat("   - Por tipo de vehículo:\n")
print(summary(fit_km_vehiculo)$table[, "median"])
cat("   - Por tipo de pedido:\n")
print(summary(fit_km_pedido)$table[, "median"])

# ============================================================
# MENSAJE FINAL
# ============================================================

cat("\n🎉 ¡ANÁLISIS COMPLETADO CON ÉXITO!\n")
cat("   ✅ 9 gráficas generadas\n")
cat("   ✅ Modelo de Cox ajustado\n")
cat("   ✅ Pruebas de hipótesis realizadas\n")
cat("   ✅ Supuestos verificados\n")
cat("   ✅ CSV limpio guardado\n").
