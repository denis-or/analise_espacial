url_busca <- 'https://www.saude.mg.gov.br/component/search/'

q_busca <- "Boletim Monitoramento xlsx Dengue"

query <- list(
  "all" = q_busca,
  "area" = "all")

user_agent <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.71 Safari/537.36"

r <- httr::GET(url_busca,
               query = query,
               httr::user_agent(user_agent))

ws <- xml2::read_html(r) |>
  xml2::xml_find_first('//div[@class="content"]//a') |>
  rvest::html_attr('href')

url_base <- 'https://www.saude.mg.gov.br/'
q2 <- paste0(url_base, ws)
r2 <- xml2::read_html(q2)

wf <- r2 |>
  xml2::xml_find_all("//a[contains(@class, 'wf_file')]") |>
  rvest::html_attr('href') |>
  purrr::keep(stringr::str_detect, "xlsx")


q3 <- paste0(url_base, wf)

baixa_arquivos <- function(url){
  tmp <- tempfile()
  curl::curl_download(url, tmp)
  tabela <- readxl::read_xlsx(tmp, skip = 2)
  tabela <- janitor::clean_names(tabela)
  tabela
}

base_grande <- purrr::map(q3,baixa_arquivos)

tabela_casos <- base_grande[[1]]
cabecalho <- tabela_casos[1,]|>as.character()
colnames(tabela_casos)<- cabecalho
tabela_casos <- tabela_casos[-1,]|>janitor::clean_names()

tabela_casos <- tabela_casos|>
  dplyr::mutate(coef_inc_acum = as.numeric(coef_incid_acumulada)) |>
  dplyr::select(codigo_ibge,
                municipio,
                coef_inc_acum,
                incidencia)


tabela_obitos <- base_grande[[2]]

tabela_obitos <- dplyr::rename(
  tabela_obitos,
  codigo_ibge = ibge
) |>
  dplyr::select(codigo_ibge,
                municipio_de_residencia,
                obito = obito_pelo_agravo_notificado)


tab_final <- dplyr::left_join(tabela_casos,tabela_obitos) |> 
  dplyr::select(codigo_ibge, 
                municipio, 
                coef_inc_acum,
                incidencia,
                obito)


dplyr::glimpse(tab_final)

minas_map <- geobr::read_municipality(code_muni = 31,
                                      showProgress = F) |>
  dplyr::mutate(codigo_ibge = as.character(substr(code_muni, 1, 6))) |>
  dplyr::left_join(tab_final)


ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = minas_map,
    ggplot2::aes(fill = coef_inc_acum),
    color = '#bdbdbd',
    size = .1) +
  viridis::scale_fill_viridis(name = "Incidência \n Dengue \n (por 100 mil hab)",
                              discrete = F,
                              direction = -1) +
  ggspatial::annotation_scale(location = 'br', style = "ticks") +
  ggspatial::annotation_north_arrow(location = 'tr',
                                    style = ggspatial:::north_arrow_fancy_orienteering()) +
  ggplot2::annotate(
    "text",
    x = -50.5,
    y = -23,
    label = "Datum SIRGAS 2000 \n Projeção Lat Long",
    size = 3) +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("Incidência da dengue") +
  ggplot2::ylab("Latitude") + ggplot2::xlab("Longitude")


ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = minas_map,
    ggplot2::aes(fill = incidencia),
    color = '#bdbdbd',
    size = .1) +
  viridis::scale_fill_viridis(name = "Incidência da Dengue",
                              discrete = T,
                              direction = -1) +
  ggspatial::annotation_scale(location = 'br', style = "ticks") +
  ggspatial::annotation_north_arrow(location = 'tr',
                                    style = ggspatial:::north_arrow_fancy_orienteering()) +
  ggplot2::annotate(
    "text",
    x = -50.5,
    y = -23,
    label = "Datum SIRGAS 2000 \n Projeção Lat Long",
    size = 3) +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("Incidência da dengue") +
  ggplot2::ylab("Latitude") + ggplot2::xlab("Longitude")


tab_compact_ob <- tab_final |>
  dplyr::filter(!is.na(obito), obito>0) |>
  dplyr::arrange(desc(obito)) |>
  dplyr::mutate(municipio = forcats::fct_reorder(municipio, obito))

gg1 <- ggplot2::ggplot(data = tab_compact_ob,
                       ggplot2::aes(x = obito, y = municipio)) +
  ggplot2::geom_col(fill = "#FDE725FF") +
  ggplot2::geom_text(ggplot2::aes(label = obito),
                     hjust = 1.2,
                     color = "black") +
  ggplot2::labs(x = "Número de óbitos",
                y = "Municípios",
                tag = "A",
                title = "Número de óbitos") +
  ggplot2::theme_bw()


tab_compact_cs <- tab_final |>
  dplyr::arrange(desc(coef_inc_acum)) |>
  head(10) |>
  dplyr::mutate(municipio = forcats::fct_reorder(municipio, coef_inc_acum))

gg2 <- ggplot2::ggplot(data = tab_compact_cs,
                       ggplot2::aes(x = coef_inc_acum, y = municipio)) +
  ggplot2::geom_col(fill = "#1F968BFF") +
  ggplot2::geom_text(ggplot2::aes(label = round(coef_inc_acum, 2)),
                     hjust = 1.2,
                     color = "white") +
  ggplot2::labs(x = "Coeficiente de incidência acumulada",
                y = "Municípios",
                tag = "B",
                title = "Coeficiente de incidência acumulada") +
  ggplot2::theme_bw()


patchwork::wrap_plots(gg1, gg2, nrow = 2)


readr::write_csv2(tab_final, "tab_final.csv")