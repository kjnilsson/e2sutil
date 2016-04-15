FSC=fsharpc
FILES = midi.fsx e2spat.fsx e2sutil.fsx

build:
	$(FSC) $(FILES) --out:e2sutil.exe

release:
	$(FSC) -O $(FILES) --out:e2sutil.exe --standalone

clean: 
	rm *.exe
