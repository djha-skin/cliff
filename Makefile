.PHONY: all docs

BUILD_DIR=docs/build/cliff-command-line-interface-functional-framework/
SIMPLE_DIR=$(BUILD_DIR)/simple
HTML_DIR=$(BUILD_DIR)/html
HTML_FILES=$(HTML_DIR)/overview.html $(HTML_DIR)/tutorial.html $(HTML_DIR)/api-reference.html
SIMPLE_HTML_FILES=$(SIMPLE_DIR)/overview.html $(SIMPLE_DIR)/tutorial.html $(SIMPLE_DIR)/api-reference.html

docs: $(HTML_DIR)/manual.pdf $(HTML_FILES)

#			-V 'sansfont=CMU Sans Serif' \
#			-V 'mainfont=CMU Serif' \
#			-V 'monofont=CMU Typewriter Text' \
	#		--pdf-engine=xelatex \
			#--shift-heading-level-by=1 \

$(HTML_DIR)/manual.pdf: $(SIMPLE_HTML_FILES)
	pandoc  -t pdf \
			-f html \
			-o $(HTML_DIR)/manual.pdf \
			--toc \
			--metadata "author=Daniel Jay Haskin" \
			--metadata "title=CLIFF: Command Line Interface Functional Framework" \
			--file-scope \
			--indented-code-classes=lisp \
			-V colorlinks=true \
			-V 'fontsize=12pt' \
			-V 'geometry=margin=1in' \
			$(SIMPLE_HTML_FILES)

$(HTML_FILES): docs/manual.scr docs/manifest.lisp
	./docs/build-docs.ros
	mkdir -p docs/build/cliff-command-line-interface-functional-framework/html/assets/
	rsync -avHAX docs/assets/ docs/build/cliff-command-line-interface-functional-framework/html/assets/
	cd docs/build/cliff-command-line-interface-functional-framework/html/ && \
		mv assets/favicon.ico . && \
		rm -f index.html && \
		ln -s overview.html index.html

$(SIMPLE_DIR)/%.html: $(HTML_DIR)/%.html
	mkdir -p $(SIMPLE_DIR)
	xmlstarlet format --omit-decl --recover --html $< | \
		xmlstarlet edit \
		    --pf --omit-decl \
			--rename "//h3" -v "h4" \
			--rename "//h2" -v "h3" \
			--rename "//h1" -v "h2" \
			--rename "//h2[@class='doc-title']" -v "h1" \
			--delete "//aside" \
			--delete '//footer' | \
			sed -e 's|CLIFF: Command Line Interface Functional Framework &#xBB; ||g' \
			> $@

