## Classify OpenDOAR data rleative to prototype
library(tidyverse)
library(jsonlite)
doar <- as_tibble(
  jsonlite::stream_in(file("data/opendoar_oaire.json"))
)
## content types (data, literature, code, all)
### 
doar_content <- doar %>%
  select(content_type, opendoar_uri) %>%
 # filter(opendoar_uri %in% c("https://v2.sherpa.ac.uk/id/repository/2659", 
#                             "https://v2.sherpa.ac.uk/id/repository/2995")) %>% 
  # Remove some empty fields
  mutate(content_type = map(content_type, unlist)) %>%
  unnest(content_type)
### tally
doar_content %>%
  count(content_type, sort = TRUE)
### classify
doar_content_tmp <- doar_content %>%
  # Remove types not considered
  filter(!content_type %in% c(NA, "patents", "theses_and_dissertations",
                             "bibliographic_references", "other_special_item_types", 
                             "learning_objects")) %>%
  mutate(content_type_classified = case_when(
   content_type %in% "datasets" ~ "Data",
   content_type %in% "software" ~ "Software",
   content_type != "datasets" ~ "Literature"
  ))
### Determine literature + data repos
multi_content <- doar_content_tmp %>%
  distinct(opendoar_uri, content_type_classified) %>% 
  group_by(opendoar_uri) %>%
  filter(n() > 1) %>%
  mutate(content_type_classified = "All") %>%
  distinct()
### Final
doar_content_classified <- doar_content_tmp %>%
  mutate(content_type_classified = if_else(
    opendoar_uri %in% multi_content$opendoar_uri, "All", content_type_classified)
  ) %>%
  distinct(opendoar_uri, content_type_classified)

## Access to repository `Access to repository` <- "Open"

## Policy 
doar_policy <- doar %>% 
  select(opendoar_uri, policies_info) %>%
  mutate(has_policy = map_lgl(policies_info, function(x) ifelse(nrow(x) > 1, TRUE, FALSE))) %>%
  mutate(policy_urls = map(policies_info, "policy_url") %>% 
           map(unique) %>%
           map_chr(function(x) paste0(x, collapse = "|"))) %>%
  filter(has_policy == TRUE) %>%
  distinct(opendoar_uri, has_policy, policy_urls)

## OpenAIRE compatibility 
doar %>% 
  count(openairecompatibility, sort = TRUE)

### Metadata contains "access rights" field
#### 
access_rights <- c("OpenAIRE 3.0 (OA, funding)", 
                   "OpenAIRE 2.0+ (DRIVER OA, EC funding)", 
                   "OpenAIRE 2.0 (EC funding)", 
                   "OpenAIRE PubRepos v4.0", 
                   "OpenAIRE Data (funded, referenced datasets)")

### Metadata contains "License" field
#### v3 recommended https://guidelines.openaire.eu/en/latest/literature/field_licensecondition.html
#### v4 recommended https://openaire-guidelines-for-literature-repository-managers.readthedocs.io/en/v4.0.0/field_licensecondition.html
license_field <- NA

### Machine-actionable
### All repos with an an API (OAI-PMH)
opendoar_oai <- readr::read_csv("data/opendoar_oai.csv")

actionable <- doar %>%
  filter(opendoar_uri %in% opendoar_oai$opendoar_uri) %>%
  distinct(opendoar_uri) %>%
  mutate(actionable = TRUE)

### Standardized
### OAI PMH is a standard protocoll
standardized <-  doar %>%
  filter(opendoar_uri %in% opendoar_oai$opendoar_uri) %>%
  distinct(opendoar_uri) %>%
  mutate(standardized = TRUE)

### Standard oai dc fields
### Author(s)	Description	Title	Date of publication	Venue of publication/deposit

md_fields <- doar %>%
  filter(opendoar_uri %in% opendoar_oai$opendoar_uri) %>%
  distinct(opendoar_uri) %>%
  mutate(`Author(s)` = TRUE,
         `Description` = TRUE,
         `Title` = TRUE,
         `Date of publication` = TRUE,
         `Venue of publication/deposit` = TRUE)
  
### Funding info
### Mandatory if applicable in 
# c("OpenAIRE 3.0 (OA, funding)", 
#  "OpenAIRE 2.0+ (DRIVER OA, EC funding)", 
#  "OpenAIRE 2.0 (EC funding)", 
#  "OpenAIRE PubRepos v4.0", 
#  "OpenAIRE Data (funded, referenced datasets)")

funding <- doar %>%
  filter(openairecompatibility %in% c("OpenAIRE 3.0 (OA, funding)", 
                                      "OpenAIRE 2.0+ (DRIVER OA, EC funding)", 
                                      "OpenAIRE 2.0 (EC funding)", 
                                      "OpenAIRE PubRepos v4.0", 
                                      "OpenAIRE Data (funded, referenced datasets)")) %>%
  distinct(opendoar_uri) %>%
  mutate(`Funder and Funding Stream` = TRUE,
         `Grant information` = TRUE)

## EC usage 
erc_usage <- readr::read_csv("data/panel_works.csv") 

panels_df <- erc_usage %>% 
  select(opendoar_link, reference_evaluation_panel) %>% 
  filter(!is.na(reference_evaluation_panel)) %>%
  filter(!is.na(opendoar_link)) %>%
  nest(panel_list = c(reference_evaluation_panel)) %>% 
  mutate(panels = map(panel_list, unlist) %>% map_chr(function(x) paste0(x, collapse = "|"))) %>%
  distinct(opendoar_uri = opendoar_link, panels)

## Top 50
top_50 <- erc_usage %>% 
  filter(!is.na(opendoar_link)) %>%
  group_by(opendoar_link) %>%
  summarise(n_projects = sum(projects_with_repo_works)) %>%
  arrange(desc(n_projects)) %>%
  slice(1:52) %>%
  mutate(my_rank = 1:52)

# Bringing it alltogether
# Record PID	Author(s) PID	Organisation PID	Grant PID	Curator	Reason for inclusion	Comments						
prototype_data <- doar %>%
  rename(`Repository name` = repo_name,
         `Organisation name` = organisation_name,
         `URL` = repo_url
         ) %>%
  mutate(`Short name` = NA) %>%
  # Coverage aka repo type
  mutate(`Coverage` = ifelse(repo_type == "institutional", "Institutional", NA)) %>%
  select(-repo_type) %>%
  # ERC Panels
  left_join(panels_df,  by = "opendoar_uri") %>%
  rename(`ERC panels` = panels) %>%
  # Content type
  left_join(doar_content_classified, by = "opendoar_uri") %>%
  rename(`Content type` = content_type_classified) %>%
  mutate(`Access to repository` = TRUE,
         `Access to data upload` = NA,
         `Certification` = NA,
         `Date of last certification` = NA,
         `Source (URL)` = NA,
         `Community Endorsement` = NA,
         `Source of Community Endorsement` = NA,
         `Community URL` = NA
         ) %>%
  # Policies
  left_join(doar_policy, by = "opendoar_uri") %>%
  rename(`Policy` = has_policy, `Policy URL` = policy_urls) %>%
  select(-policies_info) %>%
  # Metadata
  mutate(`Metadata contains "access rights"` = ifelse(openairecompatibility %in% 
                                                        c("OpenAIRE 3.0 (OA, funding)", 
                                                          "OpenAIRE 2.0+ (DRIVER OA, EC funding)", 
                                                          "OpenAIRE 2.0 (EC funding)", 
                                                          "OpenAIRE PubRepos v4.0", 
                                                          "OpenAIRE Data (funded, referenced datasets)")
                                                      , TRUE, NA),
         `Metadata contains "License"` = NA, # only recommended by OpenAIRE guidelines
         `PID Assignment` = NA # OpenAIRE has information about it for a few repos
         ) %>%
  # Actionable
  left_join(actionable, by = "opendoar_uri") %>%
  rename(`Machine-actionable` = actionable) %>%
  # Standardized
  left_join(standardized, by = "opendoar_uri") %>%
  rename(`Standardized` = standardized) %>%
  mutate(`Linked resources` = NA,
         `CC0 or PD or equivalent Metadata` = NA
         ) %>%
  # OAI DC metadata
  left_join(md_fields, by = "opendoar_uri") %>%
  #
  mutate(`Embargo` = NA) %>%
  # EC funder info
  left_join(funding, by = "opendoar_uri") %>%
  mutate(`Record PID` = NA,
         `Author(s) PID` = NA,
         `Organisation PID` = NA,
         `Grant PID` = NA,
         `Curator` = "robot",
         `Reason for inclusion` = glue::glue("Repo is listed in OpenDOAR: {opendoar_uri}"),
         comments = NA) %>%
  left_join(top_50, by = c("opendoar_uri" = "opendoar_link")) %>%
  select(-openairecompatibility, -originalId, -opendoar_uri)
prototype_data %>% 
  select(-my_rank, -n_projects) %>%
  write_csv("data/prototype_data.csv")

# top 50
prototype_data %>%
  arrange(my_rank) %>%
  filter(!is.na(my_rank)) %>%
  select(-my_rank, -n_projects) %>%
  write_csv("data/prototype_data_top50.csv")
  
         
         
         
         
  

  
