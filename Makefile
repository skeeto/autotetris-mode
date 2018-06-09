.POSIX:
EMACS = emacs

compile: autotetris-mode.elc

clean:
	rm -f autotetris-mode.elc

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -Q -batch -f batch-byte-compile $<
