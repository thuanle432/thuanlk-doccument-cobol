cobc -x -o P0001 P0001.cob
cobc -c -std=default -o ITEMIO.o ITEMIO.cob
cobc -x -std=default -o P0001 P0001.cob ITEMIO.o
./P0001