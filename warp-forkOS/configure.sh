if [ ! -f Utils.hs ]; then
    cp ../Utils.hs src/Utils.hs
fi

cabal configure
