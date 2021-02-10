import argparse

from stgpr.vetting import config_generation


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--suite_run',
        type=str,
        required=True,
        help='Name of the vetting suite run'
    )
    parser.add_argument(
        '--run_id',
        type=int,
        required=True,
        help='Prod run ID for which to pull model parameters'
    )
    parser.add_argument(
        '--gbd_round_id',
        type=int,
        required=True,
        help='GBD round ID for which to run models'
    )
    parser.add_argument(
        '--decomp_step',
        type=str,
        required=True,
        help='Decomp step for which to run models'
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
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    suite_run: str = args.suite_run
    run_id: int = args.run_id
    gbd_round_id: int = args.gbd_round_id
    decomp_step: str = args.decomp_step
    holdouts: int = args.holdouts
    draws: int = args.draws

    config_generation.save_config_and_data(
        suite_run, run_id, gbd_round_id, decomp_step, holdouts, draws
    )


if __name__ == '__main__':
    main()
