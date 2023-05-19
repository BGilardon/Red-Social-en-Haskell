module Solucion where

import Test.HUnit

-- Completar con los datos del grupo
--
-- Nombre de Grupo: Fila3
-- Integrante 1: Bautista Gilardon, bautistagilardon@gmail.com, 742/21
-- Integrante 2: Juan Pablo Gervasi, gervasi.juanpablo@gmail.com, 499/21
-- Integrante 3: Carolina Yañez, carolina.m.yanez@gmail.com, 425/20
-- Integrante 4: Franco Pomi, pomifranco@gmail.com, 935/22

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

proyectarNombresRepetidos :: [Usuario] -> [String]
proyectarNombresRepetidos [] = []
proyectarNombresRepetidos (s:us) = (nombreDeUsuario s) : (proyectarNombresRepetidos us)

quitarUsuariosRepetidos :: (Eq a) => [a] -> [a]
quitarUsuariosRepetidos [] = []
quitarUsuariosRepetidos (x:xs)  | pertenece x (quitarUsuariosRepetidos xs) = quitarUsuariosRepetidos xs
                                | otherwise = x : (quitarUsuariosRepetidos xs)

proyectarNombres :: [Usuario] -> [String]
proyectarNombres us = quitarUsuariosRepetidos (proyectarNombresRepetidos us)

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
compararAmigos red u1 u2        | cantidadDeAmigos red u1 >= cantidadDeAmigos red u2 = u1
                                | cantidadDeAmigos red u2 >= cantidadDeAmigos red u1 = u2

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red   = existeUsuarioMillonAmigos red us
                        where us = usuarios red
existeUsuarioMillonAmigos :: RedSocial -> [Usuario] -> Bool
existeUsuarioMillonAmigos red [] = False
existeUsuarioMillonAmigos red (u:us)    | (cantidadDeAmigos red u) > 1000000 = True
                                        | otherwise = existeUsuarioMillonAmigos red us

existeusuario10amigos :: RedSocial -> [Usuario] -> Bool -- Unicamente para probar que funciona
existeusuario10amigos red [] = False
existeusuario10amigos red (u:us)        | (cantidadDeAmigos red u) > 10 = True
                                        | otherwise = existeusuario10amigos red us

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
publicacionesQueLeGustanAAux (p:ps) us  | pertenece us (likesDePublicacion p) = p : (publicacionesQueLeGustanAAux ps us)
                                        | otherwise = (publicacionesQueLeGustanAAux ps us)

pertenece :: (Eq x) => x -> [x] -> Bool
pertenece _ [] = False
pertenece y (x:xs) = y == x || pertenece y xs 

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2) 

mismosElementos :: (Eq a) => [a] -> [a] -> Bool
mismosElementos xs ys = todosPertenecen xs ys && todosPertenecen ys xs 

todosPertenecen :: (Eq a) => [a] -> [a] -> Bool
todosPertenecen [] _ = True
todosPertenecen (x:xs) ys = pertenece x ys && todosPertenecen xs ys


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
estaEnTodos e (x:xs) = pertenece e x && estaEnTodos e xs  

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

