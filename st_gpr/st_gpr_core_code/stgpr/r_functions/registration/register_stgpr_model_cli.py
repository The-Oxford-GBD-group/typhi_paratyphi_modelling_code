from argparse import ArgumentParser, Namespace

import stgpr


def parse_args() -> Namespace:
    parser = ArgumentParser()
    parser.add_argument('path_to_config', type=str)
    parser.add_argument('--model_index_id', type=int)
    return parser.parse_args()


def main() -> None:
    args = vars(parse_args())
    stgpr.register_stgpr_model(**args)


if __name__ == '__main__':
    main()
