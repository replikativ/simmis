# Wiki maintenance development benchmark

This is synthetic development evidence, not a real company knowledge base or
a held-out model evaluation. The four workflow program files match the shipped
version-2 `maintain-wiki` source. Gold, calibration answers and case fixtures
belong only in the benchmark authoring workspace; candidate worlds receive
only clean fixtures.

The `supported` case permits either a minimal Company update or a fresh Product
page. Company metadata and content remain protected at their original path.
The `no-change` case already contains the supported knowledge and expects no
output. Both cases use verified frozen manifests with content hashes.

`calibration.edn` includes three positive answers (update, new page and no-op)
and damaged answers for metadata corruption, an unsupported claim and missing
output. Offline calibration must pass before any candidate starts. Structural
checks preserve explicit page/block IDs and canonical metadata, permit new
block insertion, and verify inline-link citations and quotes against frozen
source bytes. These checks do not establish factual truth or authorize adoption.

Use the normal workflow writer and inspection controls, then the normal
benchmark control with split `development` and finite model/resource limits.
Inspect the uploaded bundle ID before submission. This example creates no
allowance, enabled schedule, provider admission or model call by itself. A
successful benchmark is not a wiki proposal adoption; deployment uses the
ordinary workflow/schedule path, which does not execute the checker.
