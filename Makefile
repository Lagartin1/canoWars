# Makefile

# Nombre del ejecutable
TARGET = bin/canowars

# Directorios
SRC_DIR = src
BIN_DIR = bin

# Archivos fuente
SOURCES = $(SRC_DIR)/main.hs $(SRC_DIR)/utils.hs

# Comando de compilación
GHC = ghc

# Opciones de compilación (puedes agregar -Wall para mostrar advertencias)
GHC_FLAGS = -o $(TARGET)

# Regla principal de compilación
all: clean $(TARGET)

# Compilación del ejecutable
$(TARGET): $(SOURCES) | $(BIN_DIR)
	$(GHC) $(GHC_FLAGS) $(SOURCES)

# Crear el directorio bin si no existe
$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# Limpiar archivos generados
clean:
	rm -f $(BIN_DIR)/* $(SRC_DIR)/*.hi $(SRC_DIR)/+*.o
