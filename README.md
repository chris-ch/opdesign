# opdesign

Start with:
> stack init

And then simply run tests using:
> stack test

This will even install GHC if not already installed.

Running the program:
> stack run opdesign -- --help

Notice the first argument has to be -- in order to escape stack's options.

Example running exe:
> stack run opdesign-exe -- --pattern=2015051 --timezone=EDT data/SBH6%20Comdty.zip
> stack run opdesign-exe -- --pattern=20150618 --timezone=EDT localdata/CL1%20Comdty.zip
> stack run opdesign-exe -- --pattern=20150618 --timezone=BST localdata/CO1%20Comdty.zip

Helpful command for inspecting zip content:
> zipinfo data/data-small.zip 'data-small/*.csv'
