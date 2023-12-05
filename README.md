# Replication Materials for Registered Report "The Effects of Sustained Exposure to Fact-checking Information: Evidence from a Field Experiment on Twitter"

This repository contains the replication materials for the Registered Report "The Effects of Sustained Exposure to Fact-checking Information: Evidence from a Field Experiment on Twitter" co-author by Tiago Ventura, Kevin Aslett, Felicia Loecherbach, Joshua Tucker and Solomon Messing. 

### Abstract

>Social media companies and civic society rely heavily on fact-checking to counter misinformation. While numerous studies have shown the efficacy of single-shot corrective interventions, the effects of sustained exposure to fact-checking information in a realistic social media environment have yet to be tested. In this study, we conduct a one-month field intervention implemented in a widely used social media platform to analyze the causal effect of substantially increasing users’ exposure to fact-checking accounts and content on misinformation resilience and downstream attitudinal outcomes. In our design, Twitter users will be randomly assigned to an intervention group that will have a new timeline in their accounts composed of a pre-curated list of fact-checking organizations added to the top of their Twitter feeds, and a control group where nothing is added. Over a four-week period, participants’ compliance with the intervention will be consistently assessed, and two survey waves will measure outcomes of interest. 

### Data

Between May 21 and June 22, 2023, we ran a pilot study that followed the design described in our registered report. We recruited 114 participants for the pilot through Facebook Ads, and from this sample, 104 participants completed the final post-treatment survey (completion rate of 91\%).  We use the pilot data to a) estimate a solid-grounded power analysis using information from the pilot data and b) present our pre-registered models and measurement choice as we commit to do in the full manuscript. In this repo, under the folder data/pilot, you can find the raw and pre-processed data for each stage of our design, as detailed below:

- `all_data_processed_processed.rds`: contains our final processed data merging all the waves of the pilot design. 
- `recruitment/`: contains the raw and pre-processed data collected during the recruitment phase

- `pre_treatment/`: contains the raw and pre-processed data collected during the pre-treatment phase

-  `compliance/`: contains the raw and pre-processed data collected during the compliance tasks

-  `post_treatment/`: contains the raw and pre-processed data collected during the post-treatment survey

- `secured_data/`: contains the randomization data. 


### Code

All the codes to pre-process the data and run analysis are available in the folder `code/survey_code` as described below:

- `01_clean_recruitment_survey.R`: code to clean/pre-process the recruitment survey          
- `02_randomization.R`: code to generate the block-randomization                    
- `03_clean_pretreatment_survey.R`: code to clean/pre-process the pre-treatment survey          
- `04_clean_compliance.R`: code to clean/pre-process the compliance survey 
- `05_clean_post_treatment_survey.R`: code to clean/pre-process the post-treatment survey 
- `06_crosswalk_all_waves.R`: code to merge all the processed datasets
- `07_analysis_main_results_no_sim.R`: code to run all analysis in the registered report
- `07_analysis_main_results_simulations.R`: code to run all analysis with simulated data included in the appendix
- `08_analysis_compliance.R`: code to run analysis to measure compliance
- `utils.R`: supporting functions for the scripts
- `utils_modeling.R`: supporting functions for modelling
- `z_randomcode.R`: random functions used in the above scripts

On `code/power_analysis_code/`, we make available our code to replicate the pre-registered power analysis
