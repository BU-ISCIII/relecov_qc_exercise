#!/bin/bash

# chmod +x cambiar_nombres.sh

for i in `find ./ -name "*.xlsx" -type f`; do

    nombre=`echo $i | cut -d "/" -f4`
    extension="-resultados"
    cp $i ./$nombre$extension.xlsx

done

