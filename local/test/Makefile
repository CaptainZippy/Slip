
.SUFFIXES:

SLIP_EXE := ../../x64/Debug/slip.exe

TEST_SRC := $(wildcard ../../test/*.slip)
TEST_CPP := $(notdir $(TEST_SRC:.slip=.cpp))
TEST_EXE := $(TEST_CPP:.cpp=.exe)
TEST_LOG := $(TEST_CPP:.cpp=.log)

.PRECIOUS: $(TEST_CPP) $(TEST_EXE)
.PHONY: test run what clean

test: $(TEST_LOG)
	@echo OK

clean:
	-rm $(TEST_CPP) $(TEST_LOG)

what:
	@echo SRC=$(TEST_SRC)
	@echo CPP=$(TEST_CPP)
	@echo EXE=$(TEST_EXE)
	@echo LOG=$(TEST_LOG)

%.cpp : ../../test/%.slip $(SLIP_EXE)
	$(SLIP_EXE) $<

%.exe : %.cpp util/main.cpp
	clang++ -std=c++14 $< util/main.cpp -o $@

%.log : %.exe
	./$< > $@
