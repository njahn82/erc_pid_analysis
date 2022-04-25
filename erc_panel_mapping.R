## Match to ERC panels
library(tidyverse)
library(tidytext)
# Internal ERC data
erc_panels <- readxl::read_xlsx("erc_data/erc-grants-domains-panels.xlsx")
erc_df <- erc_panels %>%
  filter(PROGRAMME == "H2020") %>%
  select(grant_id = PROPOSAL_ID, acronym = PROPOSAL_ACRONYM,
         title = PROPOSAL_TITLE, programme = PROGRAMME, 
         panel_domain = REFERENCE_EVALUATION_DOMAIN,
         panel = REFERENCE_EVALUATION_PANEL
         ) 
# MOAP
moap_src_df_repo_only <- readr::read_csv("data/moap_src_df_repo_only.csv") %>%
  mutate(grant_id = as.character(corda_id))

panel_df <- inner_join(moap_src_df_repo_only, erc_df, by = c("grant_id"))  %>%
  mutate(officialname = gsub("ZENODO", "Zenodo", officialname))

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
