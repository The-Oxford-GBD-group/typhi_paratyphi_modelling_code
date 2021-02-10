from stgpr.vetting.util import path_utils


def get_run_id(
        suite_run: str,
        modelable_entity_id: int,
        is_original: bool
) -> int:
    run_id_path = path_utils.get_run_id_path(
        suite_run, modelable_entity_id, is_original
    )
    with open(run_id_path) as run_id_file:
        return int(run_id_file.read())


def save_run_id(
        run_id: int,
        suite_run: str,
        modelable_entity_id: int,
        is_original: bool
) -> None:
    run_id_path = path_utils.get_run_id_path(
        suite_run, modelable_entity_id, is_original
    )
    with open(run_id_path, 'w') as run_id_file:
        run_id_file.write(str(run_id))
