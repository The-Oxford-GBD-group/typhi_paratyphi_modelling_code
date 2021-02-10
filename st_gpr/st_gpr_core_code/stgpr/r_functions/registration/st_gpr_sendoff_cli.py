from argparse import ArgumentParser, Namespace

import stgpr


def parse_args() -> Namespace:
    parser = ArgumentParser()
    parser.add_argument('run_id', type=int)
    parser.add_argument('project', type=str)
    parser.add_argument('--log_path', type=str)
    parser.add_argument('--nparallel', type=int, default=50)
    return parser.parse_args()


def main() -> None:
    args = vars(parse_args())
    stgpr.stgpr_sendoff(**args)


if __name__ == '__main__':
    main()
