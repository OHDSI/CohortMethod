CohortMethod 3.1.0
==================

Changes:

1. Added plotTimeToEvent function

2. Deprecating addExposureDaysToStart and addExposureDaysToEnd arguments, adding new arguments called startAnchor and endAnchor. The hope is this is less confusing.

3. Fixing random seeds for reproducibility.

Bugfixes:

1. No longer overriding ffmaxbytes and ffbatchbytes in .onLoad. Instead relying on FeatureExtraction to do that. Part of fixing chunk.default error caused by ff package on R v3.6.0 on machines with lots of memory.

2. Correct calculation in original population count when using study end date.


CohortMethod 3.0.2
==================

Changes:

1. (Much) faster variable ratio matching
