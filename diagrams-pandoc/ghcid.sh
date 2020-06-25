LD_LIBRARY_PATH=./foreign/lib:$LD_LIBRARY_PATH ghcid -c "stack ghci --main-is diagrams-pandoc:thesis --only-main --ghci-options '-w $1'"

