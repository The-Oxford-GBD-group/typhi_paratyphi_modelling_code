import argparse

from stgpr.vetting import scatters


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
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    suite_run: str = args.suite_run
    me_id: int = args.modelable_entity_id

    scatters.plot_scatters(suite_run, me_id)


if __name__ == '__main__':
    main()
