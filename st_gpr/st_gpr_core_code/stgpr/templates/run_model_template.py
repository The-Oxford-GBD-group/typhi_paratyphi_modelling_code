import stgpr

# Register model
path_to_config = '/path/to/my/config.csv'
model_index_id = None
run_id = stgpr.register_stgpr_model(path_to_config, model_index_id)

# Submit model
project = 'proj_custom_models'
stgpr.stgpr_sendoff(run_id, project)
