.PHONY: all docs

HTML_DIR=docs/build/cliff-command-line-interface-functional-framework/html

docs: $(HTML_DIR)/%.html #$(HTML_DIR)/manual.pdf

			#--metadata "author=Daniel Jay Haskin" \
			#--metadata "title=CLIFF: Command Line Interface Functional Framework"

#$(HTML_DIR)/manual.pdf: $(HTML_DIR)/%.html
#	pandoc  -t pdf \
#			-f html \
#			-o $(HTML_DIR)/manual.pdf \
#			--shift-heading-level-by=-1 \
#			--filter ./foo \
#			--lua-filter docs/eliminate-header.lua \
#			$(HTML_DIR)/overview.html \
#			$(HTML_DIR)/api-reference.html \
#			$(HTML_DIR)/tutorial.html

$(HTML_DIR)/%.html: docs/manual.scr docs/manifest.lisp
	./docs/build-docs.ros
	mkdir -p docs/build/cliff-command-line-interface-functional-framework/html/assets/
	rsync -avHAX docs/assets/ docs/build/cliff-command-line-interface-functional-framework/html/assets/
	cd docs/build/cliff-command-line-interface-functional-framework/html/ && \
		mv assets/favicon.ico . && \
		rm -f index.html && \
		ln -s overview.html index.html
