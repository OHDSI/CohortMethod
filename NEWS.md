CohortMethod 3.0.3
==================

Changes:

1. Added plotTimeToEvent function

Bugfixes:

1. No longer overriding ffmaxbytes and ffbatchbytes in .onLoad. Instead relying on FeatureExtraction to do that. Part of fixing chunk.default error caused by ff package on R v3.6.0 on machines with lots of memory.


CohortMethod 3.0.2
==================

Changes:

1. (Much) faster variable ratio matching
