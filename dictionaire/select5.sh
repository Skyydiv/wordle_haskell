 
#!/bin/bash

# Chemin vers le fichier contenant les mots français
french_words_file="dictionnaire_francais.txt"

# Chemin vers le fichier de sortie pour les mots à 5 lettres
output_file="french_5_letters.txt"

# Utiliser grep pour filtrer les mots à 5 lettres
grep -E "^[a-zA-Z]{5}$" "$french_words_file" > "$output_file"

echo "Les mots français corrects à 5 lettres ont été écrits dans $output_file."
