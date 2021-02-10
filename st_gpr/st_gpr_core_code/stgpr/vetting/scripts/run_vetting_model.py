import argparse
from typing import Optional

from stgpr.vetting import vetting_model


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--suite_run',
        type=str,
        required=True,
        help='name of the vetting suite run'
    )
    parser.add_argument(
        '--modelable_entity_id',
        type=int,
        required=True,
        help='ID of the ME for which to run a vetting model'
    )
    parser.add_argument(
        '--is_original',
        action='store_true',
        help='Whether this model is original or updated'
    )
    parser.add_argument(
        '--project',
        type=str,
        required=True,
        help='Cluster project to run on'
    )
    parser.add_argument(
        '--original_run_id',
        type=int,
        default=None,
        help='Run ID of the original model (if copying original outputs)'
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    suite_run: str = args.suite_run
    me_id: int = args.modelable_entity_id
    is_original: bool = args.is_original
    project: str = args.project
    original_run_id: Optional[int] = args.original_run_id

    vetting_model.run_vetting_model(
        suite_run, me_id, is_original, project, original_run_id
    )


if __name__ == '__main__':
    main()
