library(readxl)
library(ggrepel)
library(tidyverse)

necessidades_recursos_estatais <- read_excel("necessidades_recursos_estatais.xlsx")

subvencoes_estatais <- read_excel("subvencoes_estatais.xlsx")


necessidades_recursos_estatais <- janitor::clean_names(necessidades_recursos_estatais)
subvencoes_estatais <- janitor::clean_names(subvencoes_estatais)


top_3_subvencoes<-
  necessidades_recursos_estatais %>%
  inner_join(subvencoes_estatais) %>%
  slice_max(order_by = subvencoes_tesouro, n=3)



necessidades_recursos_estatais %>%
  inner_join(subvencoes_estatais) %>%
  mutate(quadrante = case_when(
    subvencoes_tesouro >= 2 & necessidade_de_recursos >= 50 ~ "1",
    subvencoes_tesouro <= 2 & necessidade_de_recursos >= 50 ~ "2",
    subvencoes_tesouro <= 2 & necessidade_de_recursos <= 50 ~ "3"
  )) %>%
  ggplot(aes(x= necessidade_de_recursos, y= subvencoes_tesouro)) +
  geom_point(aes(color = quadrante), show.legend = FALSE) +
  geom_text_repel(aes(label = empresa, color = quadrante), size = 2.5, show.legend = FALSE) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "lightgray") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "lightgray") +
  geom_text(aes(x=72, y=10, label="Quadrante de alta necessidade e alta subvenção"))+
  geom_text(aes(x=63, y=1.4, label=str_wrap("Quadrante de alta necessidade e baixa subvenção",30)))+
  geom_text(aes(x=20, y=1.4, label="Quadrante de baixa necessidade e baixa subvenção"))+ 
  scale_color_manual(values = c("red","orange","darkgreen"))+
  theme_light() +
  xlim(c(0,100)) +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    x= "Necessidade de recursos (%)",
    y= "Subvenções do tesouro (R$ Bilhões)",
    title = "Associação entre necessidade de recursos e subvenções",
    subtitle = "Empresas estatais dependentes federais",
    caption = "Fonte: MGI/SEST. Dados de 2023. Elaboração própria"
  )


ggsave("grafico_estatais_dependentes.png",  width = 10, height = 5, dpi = 300, type = "cairo-png")


necessidades_recursos_estatais %>%
  inner_join(subvencoes_estatais) %>%
  writexl::write_xlsx("estatais_dependentes_economico_financeiro.xlsx")
