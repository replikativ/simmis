# Feedback for benchmark coordinators

An operator can admit public grading feedback with the exact inspected benchmark
passed to `simmis_coordinator`. Omit `public-feedback` to retain aggregate-only
results. Criterion names are transport strings; Simmis validates them and binds
their native keyword identities to the exact prepared verifier and invocation.

```json
{
  "benchmark": {
    "name": "my-workflow",
    "expected-bundle": "<inspected bundle UUID>",
    "expected-owned-source": "<inspected source revision UUID>",
    "public-feedback": {
      "version": 1,
      "checks": [
        {"check": "output-contract?", "description": "The answer satisfies the declared output schema."},
        {"check": "label-correct?", "description": "The label agrees with the fixed reference annotation."}
      ]
    },
    "limits": "<ordinary finite benchmark limits>"
  }
}
```

The fragment illustrates the feedback field; supply the normal coordinator and
benchmark limits objects rather than the explanatory placeholders. Public
descriptions are explicit admission text, not extracted from private gold,
calibration or arbitrary checker output. They describe the admitted rubric and
are not independent validation of what the checker measures. Up to 16 unique
criteria are supported, with 512 characters per description and a 4096-byte
native declaration cap including the verifier reference.

The coordinator starts the admitted experiment through
`simmis.benchmarks/start!`, then calls the native `await_child` tool with its Job
UUID. A result entry can include:

```clojure
{:attempt/id #uuid "..."
 :attempt/status :completed
 :run/status :completed
 :run/settlement-status :discarded
 :feedback {:checks {:output-contract? true :label-correct? false}
            :unavailable-checks [:optional-check?]}}
```

An unavailable check was not recorded; it is not a failed check. Certified
Attempt status and current Run/settlement status are distinct facts. Unknown
status values project to `:unavailable`. Paging preserves the same saved result:
follow `:next-offset` rather than starting another experiment. Public descriptions
appear once in the envelope, not once per entry.

The projection validates exact Attempt content, candidate, environment,
experiment, repetition and verifier bindings. It returns only declared boolean
checks; private reference answers, checker source, raw evidence, metrics and
arbitrary error text remain excluded. Existing room/operator admission,
resources, cancellation, physical ownership and settlement are unchanged.

For annotation-based benchmarks, explain the limits of each criterion. For
example, evidence completeness against an empty reference set is vacuous and
does not establish a correct NotMentioned conclusion. Reference agreement is
different from independently established correctness. The coordinator should
retain criterion results, execution outcomes and those limitations in its report
before proposing another revision; it must not alter previously graded answers.
