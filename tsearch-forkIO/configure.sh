if [ ! -f Utils.hs ]; then
    cp ../Utils.hs src/Utils.hs
fi

if [ ! -d data ]; then
    unzip data.zip
fi

cabal configure
