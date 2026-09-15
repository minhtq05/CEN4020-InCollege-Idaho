COBC := cobc
TARGET := InCollege
SOURCES := InCollege.cob \
           InCollege-IO.cob \
           InCollege-Account.cob \
           InCollege-Profile.cob \
           InCollege-ProfileEntries.cob \
           InCollege-ProfileView.cob \
           InCollege-Skills.cob
COPYBOOKS := InCollege-Common.cpy \
             InCollege-AccountRecord.cpy \
             InCollege-AccountTable.cpy \
             InCollege-ProfileRecord.cpy \
             InCollege-ProfileTable.cpy \
             InCollege-ProfileFindRow.cpy

.PHONY: all run clean

all: $(TARGET)

$(TARGET): $(SOURCES) $(COPYBOOKS)
	$(COBC) -x $(SOURCES) -o $(TARGET)

run: all
	./$(TARGET)

clean:
	rm -f $(TARGET) *.o
