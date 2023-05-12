-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

-- describir qué hace la función: .....
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red   = proyectarNombres us
                        where us = usuarios red

proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (s:us) = (nombreDeUsuario s) : (proyectarNombres us)

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red us = relacionados rel us
                where rel = relaciones red

relacionados :: [Relacion] -> Usuario -> [Usuario]
relacionados [] _ = []
relacionados (rel:relas) us     | us == u1 = u2 : relacionados relas us
                                | us == u2 = u1 : relacionados relas us
                                | otherwise = relacionados relas us
                                where   u1 = fst rel
                                        u2 = snd rel

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red us = longitud (amigosDe red us)
                        where   longitud [] = 0
                                longitud (x:xs) = 1 + longitud xs


-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = compararUsuarios red us 
                        where us = usuarios red

compararUsuarios :: RedSocial -> [Usuario] -> Usuario
compararUsuarios red [us] = us 
compararUsuarios red (u1:u2:us) = compararUsuarios red ((compararAmigos red u1 u2) : us)

compararAmigos :: RedSocial -> Usuario -> Usuario -> Usuario
compararAmigos red u1 u2    | cantidadDeAmigos red u1 >= cantidadDeAmigos red u2 = u1
                            | cantidadDeAmigos red u2 >= cantidadDeAmigos red u1 = u2

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red   = existeUsuarioMillonAmigos red us
                        where us = usuarios red
existeUsuarioMillonAmigos :: RedSocial -> [Usuario] -> Bool
existeUsuarioMillonAmigos red [] = False
existeUsuarioMillonAmigos red (u:us)    | (cantidadDeAmigos red u) > 1000000 = True
                                        | otherwise = existeUsuarioMillonAmigos red us

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red us  = publicacionesDeAux pub us
                        where pub = publicaciones red

publicacionesDeAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeAux [] _ = []
publicacionesDeAux (p:ps) us    | (usuarioDePublicacion p) == us = p : (publicacionesDeAux ps us) 
                                | otherwise = (publicacionesDeAux ps us)
-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red us    = publicacionesQueLeGustanAAux pub us
                                    where pub = publicaciones red

publicacionesQueLeGustanAAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAAux [] _ = []
publicacionesQueLeGustanAAux (p:ps) us  | elem us (likesDePublicacion p) = p : (publicacionesQueLeGustanAAux ps us) -- la funcion "elem x xs" remplaza a la funcion pertenece
                                        | otherwise = (publicacionesQueLeGustanAAux ps us)



-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2) 

mismosElementos :: (Eq a) => [a] -> [a] -> Bool
mismosElementos xs ys = todosPertenecen xs ys && todosPertenecen ys xs 

todosPertenecen :: (Eq a) => [a] -> [a] -> Bool
todosPertenecen [] _ = True
todosPertenecen (x:xs) ys = elem x ys && todosPertenecen xs ys


-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u       = existeSeguidorFiel us pubs
                                where   us = usuarios red
                                        pubs = publicacionesDe red u

existeSeguidorFiel :: [Usuario] -> [Publicacion] -> Bool
existeSeguidorFiel _ [] = False
existeSeguidorFiel [] _ = False
existeSeguidorFiel (u:us) pubs = estaEnTodos u (todosLosLikes pubs) || existeSeguidorFiel us pubs

todosLosLikes :: [Publicacion] -> [[Usuario]]
todosLosLikes [] = []
todosLosLikes (x:xs) = likesDePublicacion x : (todosLosLikes xs)

estaEnTodos :: (Eq a) => a -> [[a]] -> Bool
estaEnTodos k [] = True
estaEnTodos e (x:xs) = elem e x && estaEnTodos e xs  

-- describir qué hace la función: .....

--Todavia no esta resuelto
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2       = existeSecuenciaDeAmigosAux rel u1 u2
                                        where rel = relaciones red

existeSecuenciaDeAmigosAux :: [Relacion] -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigosAux [] _ _ = False
existeSecuenciaDeAmigosAux (r:rs) u1 u2 | not (tieneAmigos u1 (r:rs))                   = False                                         -- Si u1 no tiene amigos en la red, listo, False 
                                        | (u1,u2) == r || (u2,u1) == r                  = True                                          -- Si r (Relacion) es direactamente la relacion de u1 y u2 True
                                        | esParte u1 r && (tieneAmigos u' rs)           = existeSecuenciaDeAmigosAux rs u' u2           -- Si u1 es parte de esta relacion y u' tiene amigos en la red busco llegar a u2 con u'
                                        | esParte u1 r && not (tieneAmigos u' rs)       = existeSecuenciaDeAmigosAux rs u1 u2           -- Si u' (el relacionado con u1, que no es u2), no tiene amigos en la red, veo si hay otra relacion que contenga a u1
                                        | otherwise                                     = existeSecuenciaDeAmigosAux (rs ++ [r]) u1 u2  -- si u1 no es parte de la relacion, pongo esa relacion al final de la cadena y pruebo con la siguiente         
                                        where u'        | fst r == u1 = snd r
                                                        | snd r == u1 = fst r

tieneAmigos :: Usuario -> [Relacion] -> Bool
tieneAmigos u []        = False
tieneAmigos u (r:rs)    = esParte u r || tieneAmigos u rs 

esParte :: Usuario -> Relacion -> Bool
esParte u r     | fst r == u = True
                | snd r == u = True
                | otherwise  = False

------------------------------------------------------------------------------------------------------------------------

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion4_5 = (usuario4, usuario5)
relacion3_5 = (usuario3, usuario5)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

publicacionesC = [publicacion3_3, publicacion1_3]
relacionesC = [relacion1_2, relacion2_3, relacion4_5]
redC = (usuariosB, relacionesC, publicacionesC)