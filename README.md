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
=======
MMusicScala
===========

To use :

Take the project from https://github.com/vtpittet/ScalaMusicGeneration
In ScalaMusicGeneration/MusicInterface, do sbt eclipse
Import the project into eclipse
Import this project into eclipse

## Workflow using SBT

* first install (`ScalaMusicGeneration`)[https://github.com/vtpittet/ScalaMusicGeneration] `MusicInterface` project using `sbt local` there

from the `MMusGenScala` directory

* `sbt run`
