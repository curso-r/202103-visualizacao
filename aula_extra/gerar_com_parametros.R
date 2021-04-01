purrr::walk2(
  c("ES", "SP", "RS", "AC"), 
  c("1991", "2000", "2010", "1991"), ~{
    rmarkdown::render(
      "aula_extra/flex.Rmd", 
      output_file = stringr::str_glue("flex_{.y}_{.x}.html"),
      params = list(uf = .x, ano_idh = .y)
    )
  })


for (i in 1:10) {
  print(i)
}

purrr::walk(1:10, ~print(.x))
