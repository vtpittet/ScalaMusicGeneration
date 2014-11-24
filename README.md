COMPILING AND RUNNING



The music interface code can be compiled from the MusicInterface directory.

First make sure to have a fresh bin directory.

```bash
rm -rf bin; mkdir bin
```

Then, compile using the command

```bash

fsc -d bin/ \
    src/midiInterface/*.scala \
    src/rythmics/*.scala \
    src/segmentSystem/*.scala \
    src/tonalSystem/*.scala \
    src/utils/*.scala \
    test/caseStudy/*.scala
```
Finally you can run, for example the Recuerdos case study, executing
```bash
scala -cp bin/ caseStudy.Recuerdos
```

## Workflow using SBT

* `sbt run`
* `sbt publish-local` to use from other projects
* in order project, add `libraryDependencies += "epfl" % "irgen_2.11" % "1.0-SNAPSHOT"` to `build.sbt`