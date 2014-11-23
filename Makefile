.PHONY: all clean

all:
	ghc --make cnb.hs -o cnb

clean:
	-@rm *.hi *.o cnb 2&> /dev/null || true
