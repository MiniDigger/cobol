#!/bin/bash
echo "alte output.txt loeschen"
rm "bin/output.txt"
echo "output start" > bin/output.txt

for i in input*.txt; do
    echo "input datei $i kopieren..."
    cp $i bin/input.txt
    echo "programm starten..."
    cd bin
    ./main
    cd ..
done
echo "output kopieren"
cp bin/output.txt output.txt
