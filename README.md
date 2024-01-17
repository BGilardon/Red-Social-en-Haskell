Programación Funcional y Redes Sociales

El objetivo de este Trabajo Práctico es aplicar los conceptos de programación funcional para programar un ejemplo de red social 

En nuestro ejemplo, los usuarios solamente pueden realizar las siguientes acciones:
- Relacionarse entre sí, es decir, ser amigos.
- Postear publicaciones de texto.
- Dar me gusta a las publicaciones de sus amigos.

La red social se define a partir de los usuarios, las publicaciones de cada uno y las relaciones de amistad entre
ellos, donde:

- Cada usuario está representado con una tupla de 2 elementos, donde el primero corresponde al número de identificación (id) y el segundo a su nombre de usuario. Todos los usuarios de la red se encuentran en una lista de usuarios.
- Una publicación es una tupla de 3 elementos compuesta por: el autor de dicha publicación, el texto publicado y el conjunto de los usuarios que le dieron me gusta. Todas las publicaciones de la red est´an en la lista publicaciones.
- La amistad entre dos miembros de la red está representada con una tupla de dos usuarios. Las relaciones de amistad
se encuentran en la lista relaciones.
- Luego, podemos definir nuestra red social como una tupla de 3 elementos que contenga los tres conceptos antes mencionados, es decir: (usuarios,relaciones,publicaciones)
