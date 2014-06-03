COMPILING AND RUNNING

The music interface code can be compiled from the MusicInterface directory using the command
fsc -d bin/ \
src/midiInterface/*.scala \
src/rythmics/*.scala \
src/segmentSystem/*.scala \
src/tonalSystem/*.scala \
src/utils/*.scala \
test/caseStudy/*.scala

Then to run, for example the Recuerdos case study, execute
scala -cp bin/ caseStudy.Recuerdos