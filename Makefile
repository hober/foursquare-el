EMACS = emacs

README.md: foursquare.el
	@$(EMACS) -q --no-site-file -batch \
		-eval "(mapc (lambda (dir) (add-to-list 'load-path dir)) (parse-colon-path (getenv \"LOAD_PATH\")))" \
		-l md-readme \
		-f mdr-generate-batch $< $@
