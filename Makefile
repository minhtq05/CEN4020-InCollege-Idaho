COBC := cobc
TARGET := InCollege
SOURCES := InCollege.cob \
           InCollege-IO.cob \
           InCollege-Account.cob \
           InCollege-Profile.cob \
           InCollege-ProfileEntries.cob \
           InCollege-ProfileView.cob \
           InCollege-UserSearch.cob \
           InCollege-ViewConnections.cob \
           InCollege-Skills.cob \
           InCollege-PendingRequestFun.cob \
           InCollege-Jobs.cob \
           InCollege-SavedJobs.cob
COPYBOOKS := InCollege-Common.cpy \
             InCollege-AccountRecord.cpy \
             InCollege-AccountTable.cpy \
             InCollege-ProfileRecord.cpy \
             InCollege-ProfileTable.cpy \
             InCollege-ProfileFindRow.cpy \
             InCollege-PendingRequest.cpy \
             InCollege-PendingRequestTable.cpy \
             InCollege-ApplicationRecord.cpy \
             InCollege-JobTable.cpy

.PHONY: all run test clean

all: $(TARGET)

$(TARGET): $(SOURCES) $(COPYBOOKS)
	$(COBC) -x $(SOURCES) -o $(TARGET)

run: all
	./$(TARGET)

test:
	COBC="$(COBC)" python3 -m unittest discover -s test -p 'test_*.py' -v

clean:
	rm -f $(TARGET) *.o
