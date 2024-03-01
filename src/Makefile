SHELL := /bin/bash
SRC=../detcorpus-build
BOOKNLP=../booknlp-experimental/multilingualbooknlp

vpath %.txt $(SRC)

txtfiles := $(patsubst $(SRC)/%,%,$(wildcard $(SRC)/[12]*/*.txt))
entfiles := $(patsubst %.txt,%,$(txtfiles))

python-venv := . ../booknlp-experimental/venv/bin/activate ; PYTHONPATH=$(BOOKNLP) python

% : %.txt
        $(python-venv) $(BOOKNLP)/booknlp.py -l ru -i $< --id $(notdir $*) -o $* --modelPath $(BOOKNLP)/ru

all: entities

print-%:
        @echo $(info $*=$($*))

entities: $(entfiles)
