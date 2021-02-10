import argparse
import sys
from typing import List

import gbd

from stgpr.vetting import vetting_workflow


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--suite_run',
        type=str,
        required=True,
        help='Name of the vetting suite run'
    )
    parser.add_argument(
        '--run_ids',
        type=int,
        required=True,
        nargs='+',
        help='Prod run IDs for which to run models and produce output'
    )
    parser.add_argument(
        '--gbd_round_id',
        type=int,
        default=gbd.constants.GBD_ROUND_ID,
        help='GBD round ID for which to run models'
    )
    parser.add_argument(
        '--decomp_step',
        type=str,
        default=gbd.constants.decomp_step.ITERATIVE,
        help='Decomp step for which to run models'
    )
    parser.add_argument(
        '--project',
        type=str,
        default='proj_centralcomp',
        help='Cluster project on which to run the models'
    )
    parser.add_argument(
        '--path_to_original_binary',
        type=str,
        required=True,
        help='Path to the Python binary for the original model'
    )
    parser.add_argument(
        '--path_to_new_binary',
        type=str,
        required=True,
        help='Path to the Python binary for the new model'
    )
    parser.add_argument(
        '--holdouts',
        type=int,
        default=10,
        help='Number of holdouts at which to run each model'
    )
    parser.add_argument(
        '--draws',
        type=int,
        default=100,

        help='Number of draws at which to run each model'
    )
    parser.add_argument(
        '--keep_original_models',
        action='store_true',
        help='Whether to keep the original models or rerun with latest code'
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    suite_run: str = args.suite_run
    run_ids: List[int] = args.run_ids
    gbd_round_id: int = args.gbd_round_id
    decomp_step: str = args.decomp_step
    project: str = args.project
    path_to_original_binary: str = args.path_to_original_binary
    path_to_new_binary: str = args.path_to_new_binary
    holdouts: int = args.holdouts
    draws: int = args.draws
    keep_original_models: bool = args.keep_original_models

    workflow = vetting_workflow.create_vetting_workflow(
        suite_run,
        run_ids,
        gbd_round_id,
        decomp_step,
        project,
        path_to_original_binary,
        path_to_new_binary,
        holdouts,
        draws,
        keep_original_models
    )
    status = workflow.run()
    sys.exit(status)


if __name__ == '__main__':
    main()
