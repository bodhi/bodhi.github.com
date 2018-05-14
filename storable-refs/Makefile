../_posts/2018-05-14-storable-refs.markdown: src/Main.lhs
	pandoc -s -f markdown+lhs -t markdown-fenced_code_attributes src/Main.lhs > $@

run:
	stack build --exec storable-refs

all: ../_posts/2018-05-14-storable-refs.markdown run
