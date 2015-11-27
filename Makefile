.PHONY: clean

clean:
	find . \( -name '*.hi' -or -name '*.o' \) -and -not -path '*/\.*' -delete
