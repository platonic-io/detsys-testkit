### Lineage-driven fault injection

An implementation of [lineage-driven fault
injection](https://dl.acm.org/doi/10.1145/2723372.2723711).

#### Specification

Given a test-id, a list of run-ids, a failure specification (including an
end-time for finite failures (EFF) when and max amount of node crashes), produce
the smallest set of failures that obstruct the successful outcome from said test
and run-ids, while adhearing to the failure specification.

#### Usage

```
usage: ldfi.py [-h] --eff TIME --crashes INT --test-id TEST_ID --run-ids
               RUN_ID [RUN_ID ...] [--json]

Lineage-driven fault injection.

optional arguments:
  -h, --help            show this help message and exit
  --eff TIME            the time when finite failures end
  --crashes INT         the max amount of node crashes
  --test-id TEST_ID     the test id
  --run-ids RUN_ID [RUN_ID ...]
                        the run ids
  --json                output in JSON format?
```

#### Run testsuite

```
python -m unittest ldfi_test
```
