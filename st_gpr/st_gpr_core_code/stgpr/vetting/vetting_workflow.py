import getpass
from typing import Dict, List, Sequence

from jobmon.client.swarm import workflow as wf
from stgpr_helpers.api import internal as stgpr_helpers_internal
from stgpr_helpers.api.constants import parameters

from stgpr.common.constants import paths as stgpr_paths
from stgpr.vetting.util.constants import paths as vetting_paths


def create_vetting_workflow(
        suite_run: str,
        run_ids: Sequence[int],
        gbd_round_id: int,
        decomp_step: str,
        project: str,
        path_to_original_binary: str,
        path_to_new_binary: str,
        holdouts: int,
        draws: int,
        keep_original_models: bool
) -> wf.workflow.Workflow:
    username = getpass.getuser()
    workflow = wf.workflow.Workflow(
        workflow_args=suite_run,
        project=project,
        stderr=stgpr_paths.ERROR_LOG_PATH_FORMAT.format(username=username),
        stdout=stgpr_paths.OUTPUT_LOG_PATH_FORMAT.format(username=username),
        resume=True
    )

    tasks: Dict[str, wf.python_task.PythonTask] = {}
    for run_id in run_ids:
        params = stgpr_helpers_internal.get_parameters(run_id)
        me_id = params[parameters.MODELABLE_ENTITY_ID]
        model_task_names: List[str] = []

        # Config generation task.
        generation_task_name = f'{suite_run}_{me_id}_generate'
        generation_task = wf.python_task.PythonTask(
            name=generation_task_name,
            path_to_python_binary=path_to_original_binary,
            script=vetting_paths.GENERATE_CONFIG_SCRIPT,
            args=[
                '--suite_run', suite_run,
                '--run_id', run_id,
                '--gbd_round_id', gbd_round_id,
                '--decomp_step', decomp_step,
                '--holdouts', holdouts,
                '--draws', draws
            ],
            num_cores=1,
            m_mem_free='3G',
            max_runtime_seconds=600,
            queue='all.q',
            max_attempts=1
        )
        tasks[generation_task_name] = generation_task
        workflow.add_task(generation_task)

        for is_original in [True, False]:
            original_str = \
                vetting_paths.ORIGINAL if is_original else vetting_paths.NEW
            path_to_binary = (
                path_to_original_binary if is_original
                else path_to_new_binary
            )

            # Model task.
            model_task_name = f'{suite_run}_{me_id}_{original_str}_model'
            model_task_args = [
                '--suite_run', suite_run,
                '--modelable_entity_id', me_id,
                '--project', project
            ]
            if is_original:
                model_task_args.append('--is_original')
                if keep_original_models:
                    model_task_args.extend(['--original_run_id', run_id])
            model_task = wf.python_task.PythonTask(
                name=model_task_name,
                path_to_python_binary=path_to_binary,
                script=vetting_paths.RUN_VETTING_MODEL_SCRIPT,
                args=model_task_args,
                num_cores=3,
                m_mem_free='10G',
                max_runtime_seconds=28800,  # 8 hours
                queue='all.q',
                max_attempts=1
            )
            model_task.add_upstream(tasks[generation_task_name])
            workflow.add_task(model_task)
            tasks[model_task_name] = model_task
            model_task_names.append(model_task_name)

        # Scatters task
        scatter_task_name = f'{suite_run}_{me_id}_scatters'
        scatter_task = wf.python_task.PythonTask(
            name=scatter_task_name,
            path_to_python_binary=path_to_original_binary,
            script=vetting_paths.PLOT_SCATTERS_SCRIPT,
            args=[
                '--suite_run', suite_run,
                '--modelable_entity_id', me_id
            ],
            num_cores=1,
            m_mem_free='3G',
            max_runtime_seconds=300,
            queue='all.q',
            max_attempts=1
        )
        for model_task_name in model_task_names:
            scatter_task.add_upstream(tasks[model_task_name])
        workflow.add_task(scatter_task)
        tasks[scatter_task_name] = scatter_task

    return workflow
