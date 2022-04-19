library(tidyverse)
# - moap_ec: EC data 
# - moap_org: OpenAIRE data

###### moap_ec

## 1. Obtain ERC projects from EC data
# - Column 2: topic_unique_programme_part_code
# - Column 3: project_number can be mapped to moap_org.project.code (openaire project id)
project <- readr::read_csv("moap_data/moap_ec/project.csv", col_names = FALSE) %>%
  select(topic_unique_programme_part_descr = 2, project_number = 3)
erc_projects <- project %>%
  filter(grepl("ERC", topic_unique_programme_part_descr))
## 2. Obtain datasets registered with the EC relative to ERC funding
ec_dataset_project <- readr::read_csv("moap_data/moap_ec/dataset_project.csv", col_names = FALSE) %>%
  select(id = 1, project_number = 2) %>%
  # Get datasets linked to ERC
  filter(project_number %in% erc_projects$project_number)
# information on the datasets’ URLs reported to the EC via SyGMA
ec_datasets_accessibility <- readr::read_csv("moap_data/moap_ec/dataset_accessibility.csv", col_names = F) %>%
  select(id = 1, sourceurl = 2) 
## 3. Extract values associated with particular keys in a URL
ec_data_links <- ec_datasets_accessibility %>% 
  inner_join(ec_dataset_project, by = "id") %>%
  mutate(domain = map_df(ec_datasets_accessibility$sourceurl, urltools::url_parse)$domain)
write_csv(ec_data_links, "data/ec_data_links.csv")


###### moap_org

## 1. Obtain ERC projects 
# Column 1: id the OpenAIRE ID of the project
# Column 3: CORDA id
# Column 9: the second level of the funding stream (ERC, RIA, etc.)

oaire_project <- readr::read_csv("moap_data/moap_org/project.csv", 
                                          col_names = FALSE) %>%
  select(oaire_project_id = 1, corda_id = 3, funding_stream = 9)
# filter ERC projects
oaire_erc_projects <- oaire_project %>%
  filter(grepl("ERC", funding_stream))

oaire_erc_projects[oaire_erc_projects$corda_id %in% erc_projects$project_number,]

# 81 ERC projects covered by EC data were not listed in OpenAIRE
erc_projects[!erc_projects$project_number %in% oaire_erc_projects$corda_id,]

## 2. Obtain datasets registered with the OpenAIRE relative to ERC funding

# result_project: contains the relations between the results and their funding projects
# Column 1: id, the OpenAIRE ID of the result
# Column 2: project, the OpenAIRE ID of the project
# Column 6: datasource, if the relation was harvested, the datasource where the result is hosted

oaire_result_project <- readr::read_csv("moap_data/moap_org/result_project.csv", 
                                        col_names = FALSE) %>%
  select(oaire_result_id = 1, oaire_project_id = 2, oaire_datasrc_id = 6)

# datasource Contains information about the data sources 
# (e.g., repositories, journals, aggregators, etc) 
# that either host the results or are the sources OpenAIRE collects the metadata
# of the results from.
#
# Column 1: oaire_datasrc_id datasource
# Column 2: name
# Column 3: the type of the datasource (repository, journal, aggregator, etc.)
oaire_datasrc <- readr::read_csv("moap_data/moap_org/datasource.csv",
                                       col_names = FALSE) %>%
  select(oaire_datasrc_id = 1, datasrc_name = 2, datasrc_type = 3)

# result_sourcetype
# Column 1: id the OpenAIRE ID of the result
# Column 2: source the type of the datasource (journal in DOAJ, repository, etc)
oaire_result_sourcetype <- readr::read_csv("moap_data/moap_org/result_sourcetype.csv",
                                           col_names = FALSE) %>%
  select(oaire_result_id = 1, oaire_src_type = 2)
# result_hostedby 
# contains the data sources in which the results are hosted
# Column 1: id
# Column 2: datasource, the OpenAIRE ID of the datasource that hosts the result
oaire_result_hostedby <- readr::read_csv("moap_data/moap_org/result_hostedby.csv",
                                         col_names = FALSE) %>%
  select(oaire_result_id = 1, oaire_hosted_by_id = 2)


# result_original_pids
# contains the persistent identifiers of the results, along with the datasource 
# that hosts the results
#
# Column 1: id the OpenAIRE ID of the result
# Column 4: pid_type the type of the PID (DOI, PMID, etc)
# Column 5: pid PID
oaire_result_pid <- readr::read_csv("moap_data/moap_org/result_original_pids.csv",
                                    col_names = FALSE) %>%
  select(oaire_result_id = 1, pid_type = 4, pid = 5)
# result
# Contains the publications and datasets relevant to this study and their main metadata
# Column 1: id, the OpenAIRE ID of the result
# Column 10: type, the type of the result (publication/dataset)
oaire_result <- readr::read_csv("moap_data/moap_org/result.csv", col_names = FALSE) %>%
  select(oaire_result_id = 1, oaire_result_type = 10)
# results_url
# contains the URLs of the results
#
# Column 1: id the OpenAIRE ID of the result
# Column 2: url the url of the result
oaire_result_url <- readr::read_csv("moap_data/moap_org/result_urls.csv", col_names = FALSE) %>%
  select(oaire_result_id = 1, oaire_url = 2, data_source_id = 3)
  
oaire_erc_df <- oaire_result_project %>%
  inner_join(oaire_erc_projects, by = "oaire_project_id")

erc_urls <- inner_join(oaire_result_url, oaire_erc_df, by = "oaire_result_id") %>%
  distinct()

erc_urls_df <- erc_urls %>%
  mutate(domain = map_df(erc_urls$oaire_url, urltools::url_parse)$domain)

erc_urls_df_short <- erc_urls_df %>% 
  select(oaire_result_id, data_source_id, corda_id, oaire_url, domain) %>% 
  distinct() %>%
  inner_join(oaire_result, by = "oaire_result_id")

# backup
write_csv(erc_urls_df_short, "data/oaire_erc_urls.csv")

## PID
pids_df <- 
  inner_join(oaire_erc_df, oaire_result_pid, by = "oaire_result_id") %>% 
  inner_join(oaire_result, by = "oaire_result_id") %>% 
  distinct()
write_csv(pids_df, "data/oaire_pids.csv")
doi_df <- pids_df %>%
  filter(pid_type == "Digital Object Identifier") %>%
  distinct(oaire_result_id, doi = pid, oaire_result_type)
write_csv(doi_df, "data/oaire_erc_dois.csv")

# ## Add more comprehensive information about OpenAIRE data sources
library(tidyverse)
library(jsonlite)

# Load MOAP ERC works
my_df <- readr::read_csv("data/oaire_erc_urls.csv")

# Load data source data files from OpenAIRE Research Graph Dump December 2021
my_files <- list.files("oaire_datasrc/", full.names = TRUE)
oaire_src <- tibble::as_tibble(
  purrr::map_df(my_files, function(x) jsonlite::stream_in(file(x)))
)

# Add source info to MOAP publication data
oaire_erc_src_info <- my_df %>%
  # Identifier lack ID prefix
  mutate(scr_id_alt = paste0("10|", data_source_id)) %>%
  left_join(oaire_src, by = c("scr_id_alt" = "id")) %>%
  # Restrict to data points we are interested in
  distinct(oaire_result_id, data_source_id, corda_id, domain, 
           oaire_result_type, originalId, officialname, 
           datasourcetype = datasourcetype$value, openairecompatibility, 
           subjects, certificates) 

# Tidy subject column
oaire_src_subject <- oaire_erc_src_info %>%
  # Just interested in repo subjects
  filter(grepl("repository",datasourcetype, ignore.case = TRUE, fixed = FALSE)) %>%
  select(data_source_id, subjects) %>%
  unnest(subjects) %>% 
  distinct()
# Back-up
readr::write_csv(oaire_src_subject, "data/oaire_erc_src_subjects.csv")

# Obtain OpenDOAR links
opendoar_src <- oaire_erc_src_info %>%
  filter(grepl("opendoar", data_source_id, ignore.case = FALSE, fixed = FALSE)) %>%
  select(data_source_id, originalId) %>%
  tidyr::unnest(cols = c(originalId)) %>%
  filter(grepl("opendoar", originalId, ignore.case = FALSE, fixed = FALSE)) %>%
  mutate(opendoar_link = gsub("opendoar____::", "https://v2.sherpa.ac.uk/id/repository/", originalId)) %>%
  distinct(data_source_id, opendoar_link)

## Feed back to main dataset
oaire_erc_src_info <- oaire_erc_src_info %>%
  left_join(opendoar_src, by = "data_source_id") %>%
  # Remove nested columns, subject info can be found in `data/oaire_erc_src_subjects.csv`
  select(-originalId, -subjects)

# ## Which work is present in MOAP final?
#
# Only peer-reviewed articles were considered in the final report
#
# X1: oaire_id = the OpenAIRE ID of the result
# X 11: open_access whether the result is open access 
# (access rights of the publisher are given priority over the access rights in a 
# repository, i.e. if closed access at publisher and open access at the repository, open_access=0)
# X12: green_oa_any_version whether the result can be found open access in a repository
# X13: green_oa whether the version of record (VOR) or author-accepted manuscript 
# (AAM) of the result can be found open access in a repository (null if no info on version deposited)
# X23: best_openaire_guidelines_score the best validator score of available 
# metadata records for the result, using the OpenAIRE guidelines for literature repositories

moap_final_pubs <- readr::read_csv("moap_data/moap_final/final_pubs.csv", col_names = FALSE) %>%
  distinct(oaire_id = X1, open_access = X11, green_oa_any_version = X12,
           green_oa = X13, best_openaire_guidelines_score = X23)

moap_src_df <- oaire_erc_src_info %>%
  mutate(peer_reviewed = ifelse(oaire_result_id %in% moap_final_pubs$oaire_id, TRUE, FALSE)) %>%
  left_join(moap_final_pubs, by = c("oaire_result_id" = "oaire_id")) 
# Backup
write_csv(moap_src_df, "data/moap_src_df.csv")


moap_src_df_repo_only <- moap_src_df %>%
  filter(grepl("repository", datasourcetype, ignore.case = TRUE, fixed = FALSE)) 
# Backup
write_csv(moap_src_df_repo_only, "data/moap_src_df_repo_only.csv")
