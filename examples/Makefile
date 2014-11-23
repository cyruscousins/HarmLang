CC=ghc
DEPS=../src/HarmLang/Types.hs  \
	 ../src/HarmLang/Parser.hs  \
	 ../src/HarmLang/Interpreter.hs  \
	 ../src/HarmLang/QuasiQuoter.hs  \
	 ../src/HarmLang/Utility.hs  \
	 ../src/HarmLang/Expression.hs  \
	 ../src/HarmLang/InitialBasis.hs

all: cycle blues melody remove_object_files 

blues:
	$(CC) $(DEPS) bluesmachine.hs

cycle:
	$(CC) $(DEPS) cycle.hs


melody:
	$(CC) $(DEPS) melody.hs

remove_object_files:
	rm *.o *.hi *.dyn*

clean:
	rm -f cycle bluesmachine melody
