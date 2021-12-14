url_base <- 'https://www.saude.mg.gov.br/'
url_busca <- 'https://www.saude.mg.gov.br/component/search/'
q_busca <- "Boletim Monitoramento xlsx Dengue"

query <- list(
  "all" = q_busca,
  "exact" = "",
  "any" = "",
  "none" = "",
  "created" = "",
  "modified" = "",
  "area" = "all")

user_agent <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.71 Safari/537.36"

r <- httr::GET(url_busca,
               query = query,
               httr::user_agent(user_agent))

ws <- r |>
  xml2::read_html() |>
  xml2::xml_find_first('//div[@class="content"]//a') |>
  rvest::html_attr('href')

q2 <- paste0(url_base, ws)

r2 <- xml2::read_html(q2)

wf <- r2 |>
  xml2::xml_find_all("//a[contains(@class, 'wf_file')]") |>
  rvest::html_attr('href') |>
  purrr::keep(stringr::str_detect, "xlsx")

q3 <- paste0(url_base, wf)

# casos
tmp1 <- tempfile()
curl::curl_download(q3[1], tmp1)

tabela_casos <- readxl::read_xlsx(tmp1,
                                  sheet = "Dengue",
                                  skip = 3) |>
  janitor::clean_names()

# óbitos
tmp2 <- tempfile()
curl::curl_download(q3[2], tmp2)
tabela_obitos <- readxl::read_xlsx(tmp2, skip = 2) |> 
  janitor::clean_names()



minas <- geobr::read_municipality(code_muni=31)

minas_map <- dplyr::mutate(minas,
                           codigo_ibge = as.numeric(substr(code_muni, 1,6))) |>
  dplyr::left_join(tabela_casos)

ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = minas_map,
    ggplot2::aes(fill = incidencia_14),
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

ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = minas_map,
    ggplot2::aes(fill = coef_incid_acumulada),
    color = '#bdbdbd',
    size = .1) +
  viridis::scale_fill_viridis(name = "Incidência da Dengue",
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

