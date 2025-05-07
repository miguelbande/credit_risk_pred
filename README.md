# credit_risk_pred

## Autor
Miguel Bande Rodríguez  

# Predicción de Riesgos Bancarios con Regresión Logística en R

Este repositorio contiene un análisis completo de riesgos bancarios utilizando técnicas de análisis de datos categóricos, imputación de datos, balanceo de clases y modelos de regresión logística en R.

## Objetivo
Predecir si un cliente pagará o no su crédito a partir de variables socioeconómicas, usando modelos explicativos y predictivos, y analizar su desempeño.

## Datos
Los datos provienen de una base realista con información sobre:
- Estado del crédito (`Status`)
- Antigüedad laboral, edad, ingresos, activos, deudas...
- Tipo de vivienda (`Home`)
- Estado civil (`Marital`)
- Historial crediticio (`Records`)
- Tipo de empleo (`Job`)

## Metodología

1. **Preprocesado**: limpieza, imputación (MICE), detección de outliers, transformación logarítmica.
2. **Análisis exploratorio**: tablas de contingencia, asociación mediante RR, OR, y Chi-cuadrado.
3. **Modelado**:
   - Regresión logística binaria (predictiva y explicativa)
   - Evaluación con AUC, sensibilidad, precisión y especificidad
   - Comparación entre modelos (maximal, stepwise, con interacciones)
4. **Modelos adicionales**: Regresión logística ordinal para explicar niveles de ingreso.

## 🛠️ Herramientas

- R (caret, mice, MASS, ROCR, gmodels...)
- Estadística categórica, análisis multivariante

