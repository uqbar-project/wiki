---
layout: article
title: Preparacion de un entorno hbase
---

{% link_image Hbase_logo.png %}

# Entorno general

{% link_image ArquitecturaHBase.png  %}

-   Hadoop Filesystem (HDFS) es el sistema de archivos sobre el que se monta el motor de base HBase
-   HBase es la base, que utiliza
    -   ZooKeeper como servicio de autenticación contra el servidor Master que particiona los shards de HBase
-   Opcionalmente podemos instalar GUIs para manipular las entidades en HBase, recomendamos instalar HUE (Hadoop User Experience) que trae un HBase Explorer

Todo esto debe hacerse en un sistema operativo Linux (ya sea como sistema operativo principal de tu máquina o dentro de una VM)


# Instalación

## Download y configuración

Ingresar a esta URL

<https://hbase.apache.org/book.html#quickstart>

y seguir los pasos de instalación en modo Standalone.


# Instalación HUE

<http://tutorialforlinux.com/2014/05/23/how-to-install-hue-hadoop-web-gui-on-ubuntu-14-04-trusty-lts-easy-guide/>
