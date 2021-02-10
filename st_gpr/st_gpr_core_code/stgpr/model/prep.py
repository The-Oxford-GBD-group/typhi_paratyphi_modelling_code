import argparse
import dataclasses

from stgpr_helpers.api import internal as stgpr_helpers_internal

@dataclasses.dataclass
class PrepArgs:
    run_id: int

def parse_args() -> PrepArgs:
    parser = argparse.ArgumentParser()
    parser.add_argument('run_id', type=int)
    args = vars(parser.parse_args())
    return PrepArgs(**args)

def main() -> None:
    args = parse_args()
    stgpr_helpers_internal.prep_data(args.run_id)

if __name__ == "__main__":
    main()
