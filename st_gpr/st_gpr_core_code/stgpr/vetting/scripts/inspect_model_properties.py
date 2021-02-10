import argparse

from db_tools import ezfuncs
from stgpr_helpers.api.constants import conn_defs

from stgpr.vetting import model_properties


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--modelable_entity_id',
        type=int,
        required=True,
        help='ID of the ME to inspect'
    )
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument(
        '--decomp_step_id',
        type=int,
        help='ID of the decomp step for which to pull a best model to inspect'
    )
    group.add_argument(
        '--run_id',
        type=int,
        help=(
            'Run ID of the model to inspect (use this argument to bypass '
            'pulling a best run ID)'
        )
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    modelable_entity_id: int = args.modelable_entity_id
    decomp_step_id: int = args.decomp_step_id

    with ezfuncs.session_scope(conn_defs.STGPR) as session:
        run_id: int = (
            args.run_id if args.run_id
            else model_properties.get_best_run(
                modelable_entity_id, decomp_step_id, session
            )
        )
        print(f'Using run_id {run_id}')
        properties = model_properties.ModelProperties(run_id, session)
        print(properties.__dict__)


if __name__ == '__main__':
    main()
