find . \! -path "*/.cabal-sandbox/*" \! -path "*/dist/*" \( -name "*.dyn_hi" -delete -o -name "*.hi" -delete -o -name "*.dyn_o" -delete -o -name "*.o" -delete -o -name "*hi" -delete \)
