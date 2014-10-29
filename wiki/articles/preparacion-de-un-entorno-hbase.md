Entorno general
---------------

![Arquitectura general de Apache HBase](ArquitecturaHBase.png "Arquitectura general de Apache HBase")

-   Hadoop Filesystem (HDFS) es el sistema de archivos sobre el que se monta el motor de base HBase
-   HBase es la base, que utiliza
    -   ZooKeeper como servicio de autenticación contra el servidor Master que particiona los shards de HBase
-   Opcionalmente podemos instalar GUIs para manipular las entidades en HBase, recomendamos instalar HUE (Hadoop User Experience) que trae un HBase Explorer

Todo esto debe hacerse en un sistema operativo Linux (ya sea como sistema operativo principal de tu máquina o dentro de una VM)

Instalación Hadoop
------------------

Descargar y seguir los pasos que se indican a continuación:

<http://hadoop.apache.org/docs/r2.5.0/hadoop-project-dist/hadoop-common/SingleCluster.html>

Instalación HBase
-----------------

### Download

Ingresar a esta URL

<http://www.apache.org/dyn/closer.cgi/hbase/>

Elegir un mirror, recomendamos bajar el último release estable (directorio stable) que utilice *Hadoop 2*

**Nota:** Al 29/10/2014 la URL es <http://apache.dattatec.com/hbase/stable/hbase-0.98.7-hadoop2-bin.tar.gz>

### Configuración

<http://hbase.apache.org/book/quickstart.html> (seguir los pasos de instalación en modo Standalone)
