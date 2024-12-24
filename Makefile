.PHONY: all docs

docs: docs/build/cliff-command-line-interface-functional-framework/html/%.html

docs/build/cliff-command-line-interface-functional-framework/html/%.html: docs/manual.scr docs/manifest.lisp
	./build-docs.ros
	mkdir -p docs/build/cliff-command-line-interface-functional-framework/html/assets/
	rsync -avHAX docs/assets/ docs/build/cliff-command-line-interface-functional-framework/html/assets/
	cd docs/build/cliff-command-line-interface-functional-framework/html/ && \
		mv assets/favicon.ico . && \
		rm -f index.html && \
		ln -s overview.html index.html
