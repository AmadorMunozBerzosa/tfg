# Trabajo de Fin de Grado
#### Amador Muñoz Berzosa

Este repositorio contiene el código fuente correspondiente al Trabajo de Fin de Grado para la titulación de Doble Grado en Matemáticas e Ingeniería Informática: Tecnologías Informáticas, presentado en la convocatoria del 24 de junio de 2026.

Con este código se presentan dos herramientas:
- Un Lenguaje Específico de Dominio para definir documentos HTML y verificar que son semánticamente correctos (según el _HTML Livin Standard_) en tiempo de compilación. Se encuentran definidos en el módulo `src/DSL.idr`.
- Una herramienta de línea de comandos para `Nodejs` que permite verificar archivos HTML, devolviendo una lista de errores en lenguaje natural.

### Requisitos
Para poder compilar el código fuente, es necesario:
- Una instalación de `Idris 2`. La versión utilizada actualmente está vinculada a la que utiliza gestor de paquetes `pack` (https://github.com/stefan-hoeck/idris2-pack). Para instrucciones detalladas sobre cómo instalar Idris, consúltese el manual del lenguaje (https://idris2.readthedocs.io/en/latest/tutorial/starting.html)
- Una instalación de `Nodejs 26.3`. Se puede obtener de la página oficial (https://nodejs.org/en/download).

### Compilación y uso
Situado en esta carpeta, ejecútese `idris2 --build`. Esto debería crear un archivo ejecutable `cli` en el directorio `./build/exec`.

Una vez creado este archivo, se puede ejecutar la interfaz de línea de comandos llamando a `node` sobre el directorio `./html-validate`. La interfaz espera un único parámetro, siendo este la ruta del archivo que se desea verificar.