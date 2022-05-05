## Match to ERC panels
library(tidyverse)
library(tidytext)
# Internal ERC data
erc_panels <- readxl::read_xlsx("erc_data/erc-grants-domains-panels.xlsx", guess_max = 50000)
erc_tmp <- erc_panels %>%
  filter(PROGRAMME == "H2020") %>%
  select(grant_id = PROPOSAL_ID, acronym = PROPOSAL_ACRONYM,
         title = PROPOSAL_TITLE, programme = PROGRAMME, 
         reference_evaluation_domain = REFERENCE_EVALUATION_DOMAIN,
         reference_evaluation_panel = REFERENCE_EVALUATION_PANEL,
         reference_evaluation_panel_title =  `REFERENCE_EVALUATION _PANEL TITLES`,
         original_grant = `ORIGINAL GRANT (FOR PROOF OF CONCEPT (POC))`
         ) 

# PoC call schema projects (Proof od Concept) are projects that derive from other ERC grants
# and the ERC panel can be derived from the original one (grant no available in 
# column K - ORIGINAL GRANT (FOR PROOF OF CONCEPT (POC))
#
# get poc
orig_poc <- erc_tmp %>%
  filter(!is.na(original_grant)) %>%
  select(grant_id, original_grant) %>%
  inner_join(erc_tmp, by = c("original_grant" = "grant_id")) %>%
  select(-original_grant.y)

erc_df <- erc_tmp %>%
  filter(!grant_id %in% orig_poc$grant_id) %>%
  bind_rows(orig_poc)

# MOAP
moap_src_df_repo_only <- readr::read_csv("data/moap_src_df_repo_only.csv") %>%
  mutate(grant_id = as.character(corda_id)) %>%
  # remove unknown repo
  filter(officialname != "Unknown Repository")

# Key figures

## Number of ERC H2020 projects
no_erc_projects <- erc_df %>%
  distinct(grant_id) %>%
  nrow()
## Number and proportion ERC H2020 projects with at least one repository publication
erc_with_repo <- erc_df %>% 
  filter(grant_id %in% moap_src_df_repo_only$grant_id) %>% 
  distinct(grant_id) %>%
  nrow()

erc_with_repo / no_erc_projects

## by repository type
moap_src_df_repo_only %>%
  group_by(oaire_result_type) %>%
  summarise(n = n_distinct(grant_id)) %>%
  mutate(prop = n / no_erc_projects)

# Reference evaluation domain and panels analysis

## Breakdown projects by domain 
projects_by_domain <- erc_df %>%
  group_by(reference_evaluation_domain) %>%
  summarise(projects = n_distinct(grant_id)) %>%
  mutate(prop = projects / no_erc_projects)
projects_by_domain

## Breakdown projects by panels
projects_by_panel <- erc_df %>%
  group_by(reference_evaluation_panel, reference_evaluation_panel_title) %>%
  summarise(projects = n_distinct(grant_id)) %>%
  mutate(prop = projects / no_erc_projects)
projects_by_panel

# Repo Usage

## Prepare data
panel_df <- inner_join(moap_src_df_repo_only, erc_df, by = c("grant_id"))  %>%
  mutate(officialname = gsub("ZENODO", "Zenodo", officialname)) 

## Number of work by domain
panel_df %>%
  group_by(reference_evaluation_domain) %>%
  summarise(works = n_distinct(oaire_result_id),
            projects_with_repo_works = n_distinct(grant_id)) %>%
  inner_join(projects_by_domain, by = "reference_evaluation_domain") %>%
  mutate(prop = projects_with_repo_works / projects)

## Number of work by panel

panel_df %>%
  group_by(reference_evaluation_panel, officialname) %>%
  summarise(works = n_distinct(oaire_result_id),
            projects_with_repo_works = n_distinct(grant_id)) %>%
  inner_join(projects_by_panel, by = "reference_evaluation_panel") %>%
  mutate(prop = projects_with_repo_works / projects)



panel_df_plot <- panel_df %>%
  filter(oaire_result_type == "publication", !is.na(panel_domain)) %>%
  group_by(panel_domain, officialname) %>%
  summarise(articles = n_distinct(oaire_result_id)) %>%
  mutate(prop = articles / sum(articles)) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(panel_domain = as.factor(panel_domain),
         repo = tidytext::reorder_within(officialname, articles, panel_domain))

pp <-  ggplot(panel_df_plot, aes(reorder(repo, prop), prop, fill = panel_domain)) +
  geom_bar(stat = "identity") +
  scale_fill_manual("ERC Panel Domain", values = c("darkorange","purple","cyan4")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 5L),
                       expand = expansion(mult = c(0, 0.05))) +
  coord_flip() +
  facet_wrap(~panel_domain, scales = "free_y", ncol = 1) +
    cowplot::theme_minimal_vgrid() +
  labs(title = "ERC: Top Literature Repository by usage",
       subtitle = "Preliminary results based on MOAP",
       x = NULL, y = "Percentage") +
  theme(legend.position = "noe",  plot.title.position = "plot")

ggsave(pp, filename = "erc_usage.png", dpi = 300)  
