#!/bin/bash

# Vérifier si un fichier est spécifié en argument
if [ $# -ne 1 ]; then
    echo "Usage: $0 fichier"
    exit 1
fi

# Vérifier si le fichier existe
if [ ! -f "$1" ]; then
    echo "Le fichier $1 n'existe pas."
    exit 1
fi

# Convertir les majuscules en minuscules et écrire le résultat dans un nouveau fichier
tr '[:upper:]' '[:lower:]' < "$1" > "${1%.txt}_minuscules.txt"

echo "Conversion terminée. Le fichier converti est : ${1%.txt}_minuscules.txt"
 
