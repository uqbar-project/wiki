Puede haber un poco de confusión con la palabra Máquina Virtual (VM).

La imagen de Pharo corre sobre una máquina virtual, llamada Pharo VM o Squeak VM o Cog VM. Esta VM es un programa más o menos común y corriente, sobre el cual se carga una imagen de Pharo. Esta VM es indispensable.

Mas allá de que existe una diferencia técnica enorme, en una primera aproximación podés comparar a la VM de Pharo con el intérprete de Hugs o swi-prolog: son programas responsables de ejecutar tu código. Luego, podés comparar a la imágen de Pharo con los archivos .pl o .hs: contienen código y datos de tu programa.

Por otro lado, en el laboratorio de sistemas, la VM de Pharo está instalada directamente sobre el sistema operativo (un Windows), mientras que los interpretes de Haskell y Prolog están instalados, por cuestiones prácticas para la gente del laboratorio, en un sistema operativo virtualizado montado sobre una VM VMware, pero bien podrían haberlo instalado sobre el mismo Windows.

<img src="VMsComparadas.png" title="VMsComparadas.png" alt="VMsComparadas.png" width="500" />

Esta última VM (VMWare) es de naturaleza totalmente distinta a la VM de Pharo: emula un hardware completo, y se la conoce como Máquina Virtual de Sistema. La máquina virtual de Pharo, en cambio, emula sólo algunos algunos aspectos del hardware, y tiene como única finalidad correr código en Smalltalk. Se la conoce como Máquina Virtual de Aplicación (en azul):

Recalco que la virtualización en amarillo es totalmente opcional, en el labo está así porque ayuda a organizar el software para cada materia, pero normalmente en tu casa no vas a necesitar hacer eso para instalar el software de PDP.
