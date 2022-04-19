## bigquery / unpaywall analytics
library(tidyverse)
library(bigrquery)

oaire_dois <- readr::read_csv("data/oaire_erc_dois.csv")
bq_oaire_dois <- bigrquery::bq_table("subugoe-closed", "dois_for_matching", "erc_oaire")

bigrquery::bq_table_upload(bq_oaire_dois,
                           oaire_dois)

my_project <-  bigrquery::bq_dataset("subugoe-closed", "dois_for_matching")
bigrquery::bq_dataset_query(my_project, query = "SELECT
  repository_institution,
  is_best,
  COUNT(DISTINCT oaire_doi) AS articles
FROM (
  SELECT
    oaire.doi AS oaire_doi,
    oa_locations,
    has_repository_copy,
    journal_is_in_doaj,
    journal_is_oa,
    oaire_result_type,
    genre
  FROM
    `subugoe-collaborative.upw_instant.snapshot` AS upw
  INNER JOIN
    `subugoe-closed.dois_for_matching.erc_oaire` AS oaire
  ON
    upw.doi = LOWER(oaire.doi) ),
  UNNEST(oa_locations)
WHERE
  repository_institution IS NOT NULL
GROUP BY
  repository_institution,
  is_best,
  oaire_result_type
ORDER BY
  articles DESC", destination_table = bigrquery::bq_table("subugoe-closed", "dois_for_matching", "upw_erc_match"))

my_df <- bigrquery::bq_table_download("subugoe-closed.dois_for_matching.upw_erc_match")

write_csv(my_df, "data/oaire_upw_repo.csv")
