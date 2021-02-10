# ST-GPR Vetting Suite

## Module Overview
- **model_properties.py** is used to summarize models that could be included in the vetting suite.
- **config_generation.py** contains functions to copy parameters and input data from a production ST-GPR model to a vetting suite model.
- **vetting_model.py** is used to run a single vetting model.
- **vetting_workflow.py** has functions to run a suite of multiple vetting models.
- The **util** folder contains constants and helpers that are specific to the vetting suite (i.e. not used elsewhere in ST-GPR).
- The files in the **report** directory contain functions for calculating and plotting coverage, summarizing model parameters and statistics, plotting fits, and plotting scatters.
- The files in the **scripts** directory detail a command-line interface to the above functions.

## Inspect Model Properties
`python inspect_model_properties.py --modelable_entity_id 23668 --decomp_step_id 4` to print model properties associated with the best trichuriasis run from decomp step ID 4.

`python inspect_model_properties.py --modelable_entity_id 23668 --run_id 12345` to print model properties associated with trichuriasis run 12345.

## Run Suite
`python run_vetting_suite.py --suite_run my_vetting_suite_run --run_ids 12345 56789 --project proj_stgpr --path_to_original_binary /ihme/code/st_gpr/miniconda3/envs/stgpr/bin/python --path_to_new_binary /ihme/homes/dferrer/miniconda3/variance_weighting/bin/python --holdouts 3` to run a suite against the test database that uses the inputs from production models 12345 and 56789. The `path_to_binary` arguments specify Python binaries for original (current) and updated (including a methods change) environments.

Optional arguments include:
- **gbd_round_id** to specify for which round models will run (default current active round)
- **decomp_step** to specify for which decomp step models will run (default iterative)
- **project** to specify in which cluster project to run (default proj_centralcomp)
- **holdouts** to specify how many holdouts to use in each model (default 10)
- **draws** to specify how many draws to use in each model (default 100)
- **keep_original_models** to copy original model outputs rather than running original models using original parameters/data with code specified by **path_to_original_binary** (default false)

## View Outputs
- Model output is written to `/ihme/code/st_gpr/vetting/suite_runs/{suite_run}/{me_id}/{original_or_new}`
- PDFs are written to `/ihme/code/st_gpr/vetting/suite_runs/{suite_run}/{me_id}/pdfs`
