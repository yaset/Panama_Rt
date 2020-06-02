## Analisis de la dinamica del Brote en Panama

El presente proyecto tiene como fin describir la dinámica de la pandemia de COVID en Panamá.
Los archivos que contiene son:

00-configuracion: Permite crear carpetas e instalar paquetes que no existian antes en el pc

01-Data_Extraction: De la base de datos original selecciona las siguientes variables de interes:

- type_patient: Estado de paciente entre ambulatorio / desconocido / fallecido / hospitalizado

- sex: Genero

- age: Edad, años

- region: ### se debe clarificar la convención de las regiones###

- corregimiento: ### clarificar la convención de los corregimientos###

- fis (fecha de inicio de sintomas): Formato %Y-%m-%d (Ejem: 2020-06-01)

- Fecha_de_recibo: Fecha de recepción de la muestra

- Fecha_de_reporte: Fecha de notificación del resultado

- tiempodelay: dias estimado de demora

_ Type_of_case: Estado de estudio del caso importado / locales

- exposition / type_exposition: Lugar de exposicion al virus


02- Incidence:
Es el script que recrea los graficos de incidencias. 
