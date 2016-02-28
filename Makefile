all : ; cp src/php/* bin.jsexe/php/ ; ghcjs --make src/AcmeWeb.hs -isrc/ -odir odir -hidir hidir  -o bin -O2; cp -R bin.jsexe/* ~/srv/http/AcmeWeb

clean :
	rm -rf src/*.js*
