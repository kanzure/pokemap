GENERATED := $(basename .m4, $(wildcard *.m4))

default: ${GENERATED}

clean:
	rm -f ${GENERATED}

.PHONY: clean default

%: %.m4
	m4 $< > $@
