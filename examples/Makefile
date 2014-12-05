HC=ghc
DEPSNOIO=../src/HarmLang/Types.hs  \
	 ../src/HarmLang/Parser.hs  \
	 ../src/HarmLang/Interpreter.hs  \
	 ../src/HarmLang/QuasiQuoter.hs  \
	 ../src/HarmLang/Utility.hs  \
	 ../src/HarmLang/Expression.hs  \
	 ../src/HarmLang/InitialBasis.hs \
	 ../src/HarmLang/Probability.hs \
	 ../src/HarmLang/HarmonyDistributionModel.hs \
	 ../src/HarmLang/ChordProgressionDatabase.hs \
	 ../src/HarmLang/Priors.hs
DEPS=$(DEPSNOIO)  \
	 ../src/HarmLang/IO.hs

all: arpeggiatesong blues cycle melody stylisticinference remove_object_files

arpeggiatesong: arpeggiatesong.hs
	$(HC) $(DEPS) arpeggiatesong.hs

blues: bluesmachine.hs
	$(HC) $(DEPS) bluesmachine.hs

cycle: cycle.hs
	$(HC) $(DEPS) cycle.hs

melody: melody.hs
	$(HC) $(DEPS) melody.hs

stylisticinference: stylisticinference.hs
	$(HC) $(DEPSNOIO) stylisticinference.hs -o stylisticinference

remove_object_files:
	rm *.o *.hi *.dyn*

clean:
	rm -f arpeggiatesong blues cycle melody stylisticinference
