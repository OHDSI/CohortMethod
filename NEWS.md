CohortMethod 4.0.0
==================

Changes: 

1. Switching from ff to Andromeda for storing large data objects.

Bugfixes:

1. Fixed bug in IPTW.


CohortMethod 3.1.1
==================

Changes:

1. Updating documentation: adding literature reference for IPTW, and using new SqlRender interface in vignettes.

2. Changing default equipoise bounds from 0.25-0.75 to 0.3-0.7 to be consistent with Alec Walker's original paper.

Bugfixes:

1. Fixing some issues when sampling before fitting propensity models.


CohortMethod 3.1.0
==================

Changes:

1. Added plotTimeToEvent function

2. Deprecating addExposureDaysToStart and addExposureDaysToEnd arguments, adding new arguments called startAnchor and endAnchor. The hope is this is less confusing.

3. Fixing random seeds for reproducibility.

4. Changing default equipoise bounds from 0.25-0.75 to 0.3-0.7 to be consistent with Alec Walker's original paper.

Bugfixes:

1. No longer overriding ffmaxbytes and ffbatchbytes in .onLoad. Instead relying on FeatureExtraction to do that. Part of fixing chunk.default error caused by ff package on R v3.6.0 on machines with lots of memory.

2. Correct calculation in original population count when using study end date.


CohortMethod 3.0.2
==================

Changes:

1. (Much) faster variable ratio matching
