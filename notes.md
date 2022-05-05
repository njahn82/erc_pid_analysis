## Potential issues

Preprint and published version not linked, eg

https://www.biorxiv.org/content/10.1101/523431v1 | dedup_wf_001::00098008e047f5bac662a7194c263236


Overexpression of Osmosensitive Ca2+-Permeable Channel TMEM63B Promotes Migration in HEK293T Cells
https://doi.org/10.1021/acs.biochem.9b00224
https://www.biorxiv.org/content/10.1101/626010v1

bioarxiv not listed as repo, but unknown repo

dedup_wf_001::004488dfaa6ce2ffa2e13e1eb3478b2f

in total, we have ~5,578 (3,383 peer reviewed) works where just an unknown repo is listed

```r
panel_df %>%
  distinct(oaire_result_id, officialname) %>%
  group_by(oaire_result_id) %>% 
  filter(n() == 1) %>%
  filter(officialname == "Unknown Repository")
````


## Some repo examples

https://intr2dok.vifa-recht.de/receive/mir_mods_00010930
