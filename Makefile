run:
	love .

clean:
	rm -rf dist
	rm -f game.zip
	rm -f game.love

build: clean
	mkdir dist
	cp main.lua *.fnl dist/
	cp -R lib/ dist/lib/
	cp -R img/ dist/img/
	cp -R fonts/ dist/fonts/
	cd dist/; zip -9 -r game.love .
	mv dist/game.love .

