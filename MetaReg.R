#Meta-regresión con metafor
#Cargar paquete
library(metafor)

#Calcular tamaños del efecto
CalcES <- escalc(measure = "SMD", 
                 m1i = m1, sd1i = sd1, n1i = n1,
                 m2i = m2, sd2i = sd2, n2i = n2, 
                 data = MetaregNHE)

#Nuevo DF para trabajar
CalcES


#Generar el MA
maFOR <- rma(yi, vi, data = CalcES)

summary(maFOR)

#Meta-regresión
maREG <- rma(yi, vi, mods = ~ Covar, data = CalcES)

summary(maREG)

#Crear gráfica con metafor
regplot(maREG, shade = TRUE, ylab = "SMD", refline = 0, col = "skyblue3")


#Meta-regresión con meta
#Cargar paquete
library(meta)

#Realizar meta-análisis
maNHE <- metacont(n1,m1,sd1,n2,m2,sd2, data = CalcES, sm = "smd",
                  random = TRUE, common = FALSE, method.smd = "Cohen")

#Realizar meta-regresión
mreg <- metareg(maNHE, Covar)

#Generar gráfica
bubble(mreg, ylim = c(-1,1), xlim = c(22,28), xlab = "Age", ylab = "SMD")

#Alternativa con ggplot2
#Cargar paquete
library(ggplot2)

#Crear gráfico
ggplot(CalcES, aes(x= Covar, y= yi, size = vi)) +
  scale_size(range = c(9, 18))+
  geom_smooth(method = "lm", se = TRUE, color = "black",
              fill = "grey75", alpha = 0.5, size = 1.05, fullrange = TRUE)+
  geom_point(color = "royalblue3", alpha = 0.7)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position = "none") +
  labs(x = "Age (years)", y = "Effect Size (SMD)", color = "Sexo") +
  geom_hline(yintercept = 0, size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  expand_limits(x = c(22, 28)) +
  scale_y_continuous(expand = c(0,0))+
  ylim(-1.5, 1)
