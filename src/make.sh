#make executable with chmod u+x make.sh
#!/bin/bash
ghc lemmy_search_main.hs -XParallelListComp
