/* Primero Mostramos cuáles son las bases de datos disponibles */
SHOW DATABASES;

/*Ahora indicamos que usaremos la base de datos y vemos de cuántas tablas se compone esta base */
USE Databi;

SHOW TABLES;

/* Esta base sólo tiene tres variables, El ID de la categoría, El nombre de la sub categoría y el nombre de su categoría*/
SELECT* FROM dbo.Categorias_Subcategorias;

/* Esta base tiene 6 variables un ID único de los ITEM (otras irrelevantes) y una foreign ID para unir con la subcategoría ID*/
SELECT * FROM dbo.Items;

/* Esta base tiene 4 variables y dos son ID una para el leadSource ID que es como el proveedor de la mercancía y otra el marketplace ID*/
SELECT * FROM dbo.Marketplace;

/* Esta base tiene 5 variables y es muy relevante ya que contiene el ID de la transacción de la venta, su precio unitario, la cantidad de productos que fueron comprados y el monto (debe ser unitario*/
SELECT * FROM dbo.Ordenes_Venta_Detalle

/* Finalmente tenemos esta base que contiene el ID de la transacción y su fecha para poder unirlas en el reporte interactivo*/
SELECT * FROM DBO.Ordenes_Venta_Resumen

/* Ahora podemos exportar las bases a Poer BI*/