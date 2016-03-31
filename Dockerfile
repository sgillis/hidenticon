FROM samdoshi/haskell-stack

WORKDIR /home
RUN git clone https://github.com/sgillis/hidenticon.git
WORKDIR /home/hidenticon
RUN stack setup
RUN stack install --ghc-options='-static -optc-static -optl-static -optl-pthread'