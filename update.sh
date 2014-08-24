git pull origin master
cd OpenGL
git pull mfpi master
cd ../OpenGLRaw
git pull mfpi master
cd ../spacepart
git pull origin master
cd ../freetype2
git pull origin cabalization
cd ../halo
git pull origin master
cd ../hsys
git pull origin master
cd ../harfbuzz
git pull origin master
cd ../imagemagick
git pull mfpi master
cd ../games/boom
git pull origin master

cabal clean
cabal configure
cabal build boom
