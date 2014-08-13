git clone https://github.com/mfpi/henge
cd henge
git clone https://github.com/mfpi/OpenGL
git clone https://github.com/mfpi/OpenGLRaw
git clone https://github.com/mfpi/data-spacepart
git clone https://github.com/mfpi/freetype2
git clone https://github.com/mfpi/halo
git clone https://github.com/mfpi/hsys
git clone https://github.com/mfpi/imagemagick
mkdir games
cd games
git clone https://github.com/mfpi/boom
cd boom
cabal sandbox init
cabal sandbox add-source ../../OpenGL
cabal sandbox add-source ../../OpenGLRaw
cabal sandbox add-source ../../data-spacepart
cabal sandbox add-source ../../imagemagick
cabal sandbox add-source ../../freetype2
cabal sandbox add-source ../../halo
cabal sandbox add-source ../../hsys
cabal configure
cabal install --dependencies-only
cabal build

