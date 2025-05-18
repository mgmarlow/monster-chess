install:
	npm install -g love.js

run:
	fennel --compile main.fnl > main.lua; love .

clean:
	rm -rf dist
	rm game.zip
	rm game.love

build: clean
	fennel --compile main.fnl > main.lua
	mkdir dist
	cp main.lua dist/main.lua
	cp -R img/ dist/img/
	cp -R fonts/ dist/fonts/
	cd dist/; zip -9 -r game.love .
	mv dist/game.love .

js: build
	love.js game.love game -c
