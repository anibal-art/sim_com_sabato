#!/bin/bash

# Función que cuenta las líneas de un archivo
contar_lineas() {
    local archivo="$1"
    if [[ -f "$archivo" ]]; then
        # wc -l cuenta las líneas, awk extrae solo el número
        lineas=$(wc -l < "$archivo")
        echo "El archivo '$archivo' tiene $lineas líneas."
    else
        echo "Error: el archivo '$archivo' no existe."
    fi
}

# Zona main
if [[ $# -eq 0 ]]; then
    echo "Uso: $0 nombre_archivo"
    exit 1
fi

contar_lineas "$1"

