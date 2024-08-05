.PHONY: tags

tags:
	find . -name "*.el" -print | etags -
