CC=ghc
DEPSNOIO=../src/HarmLang/Types.hs  \
	 ../src/HarmLang/Parser.hs  \
	 ../src/HarmLang/Interpreter.hs  \
	 ../src/HarmLang/QuasiQuoter.hs  \
	 ../src/HarmLang/Utility.hs  \
	 ../src/HarmLang/Expression.hs  \
	 ../src/HarmLang/InitialBasis.hs \
	 ../src/HarmLang/Probability.hs \
	 ../src/HarmLang/HarmonyDistributionModel.hs \
	 ../src/HarmLang/ChordProgressionDatabase.hs
DEPS=$(DEPSNOIO)  \
	 ../src/HarmLang/IO.hs

all: cycle blues melody stylisticinference remove_object_files 
	 ../src/HarmLang/ChordProgressionDatabase.hs \
	 ../src/HarmLang/IO.hs

all: cycle blues melody arpeggiatesong remove_object_files 

arpeggiatesong: arpeggiatesong.hs
	$(CC) $(DEPS) arpeggiatesong.hs

blues: bluesmachine.hs
	$(CC) $(DEPS) bluesmachine.hs

cycle: cycle.hs
	$(CC) $(DEPS) cycle.hs

melody: melody.hs
	$(CC) $(DEPS) melody.hs

stylisticinference: stylisticinference.hs
	$(CC) $(DEPSNOIO) stylisticinference.hs

remove_object_files:
	rm *.o *.hi *.dyn*

clean:
	rm -f cycle bluesmachine melody arpeggiatesong
