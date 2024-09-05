COBC=cobc
COBCFLAGS=-c -std=default
LDEXEFLAGS=-x

SOURCES=modify-client.cbl
OBJECTS=$(SOURCES:.cbl=.o)
EXECUTABLE=write

all: $(EXECUTABLE)

$(EXECUTABLE): write.cbl $(OBJECTS)
	$(COBC) $(LDEXEFLAGS) -o $@ write.cbl $(OBJECTS)

%.o: %.cbl
	$(COBC) $(COBCFLAGS) -o $@ $<

clean:
	rm -f $(OBJECTS) $(EXECUTABLE)

.PHONY: all clean
