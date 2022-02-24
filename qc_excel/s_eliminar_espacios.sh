#!/bin/bash

# chmod+x cambiar_nombres.sh

while read -d $'\0' f; do mv -v "$f" "${f// /_}"; done