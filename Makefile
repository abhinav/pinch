.PHONY: clean

clean:
	find . \( -name '*.hi' -or -name '*.o' \) -and -not -path '*/\.*' -delete

test:
	stack test

test-32:
	vagrant up
	vagrant provision
	vagrant rsync
	vagrant ssh -c "cd /vagrant; stack setup && stack test"
	vagrant halt
