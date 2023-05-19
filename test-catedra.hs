import Test.HUnit
import Solucion

main = runTestTT tests

tests = test [
    -- Test ejercicio 1
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],
    " nombresDeUsuarios | redVacia" ~: (nombresDeUsuarios redVacia) ~?= [],
    " nombresDeUsuarios | un solo usuario" ~: (nombresDeUsuarios redUnicoUsuario) ~?= ["Juan"],
    " nombresDeUsuarios | mas de un usuario, sin repetidos" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"], -- es el mismo test porque sirve
    " nombresDeUsuarios | usuarios repetidos" ~: (nombresDeUsuarios redConNombresRepetidos) ~?= ["Juan","Natalia"],

    -- Test ejercicio 2
    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],
    " amigosDe | un solo usuario" ~: (amigosDe redUnicoUsuario usuario1) ~?= [],
    " amigosDe | mas de un usuario, sin relaciones" ~: (amigosDe redB usuario5) ~?= [],
    " amigosDe | mas de un usuario, con relaciones" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4], -- es el mismo test porque sirve


    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,

    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,

    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

    --Comprobar que las redes funcionan por separado aun usando los mismos usuarios, y aunque a estos no les guste ninguna publicacion en esa red social 
    --redA
    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],
    " publicacionesQueLeGustanA 2 " ~: (publicacionesQueLeGustanA redA usuario2) ~?= [publicacion1_1, publicacion3_2, publicacion4_1],
    " publicacionesQueLeGustanA 3 " ~: (publicacionesQueLeGustanA redA usuario3) ~?= [],
    " publicacionesQueLeGustanA 4 " ~: (publicacionesQueLeGustanA redA usuario4) ~?= [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2],
    
    --redB
    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redB usuario1) ~?= [],
    " publicacionesQueLeGustanA 2 " ~: (publicacionesQueLeGustanA redB usuario2) ~?= [publicacion1_3, publicacion3_2, publicacion3_3],
    " publicacionesQueLeGustanA 3 " ~: (publicacionesQueLeGustanA redB usuario3) ~?= [],
    " publicacionesQueLeGustanA 5 " ~: (publicacionesQueLeGustanA redB usuario5) ~?= [publicacion1_3, publicacion1_5, publicacion3_3],

    --redC
    " publicacionesQueLeGustanA 1 " ~: (publicacionesQueLeGustanA redC usuario1) ~?= [publicacion4_3],
    " publicacionesQueLeGustanA 3 " ~: (publicacionesQueLeGustanA redC usuario3) ~?= [publicacion4_3],
    " publicacionesQueLeGustanA 4 " ~: (publicacionesQueLeGustanA redC usuario4) ~?= [publicacion1_1, publicacion1_2],
    " publicacionesQueLeGustanA 5 " ~: (publicacionesQueLeGustanA redC usuario5) ~?= [publicacion1_5, publicacion3_3],

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,
    " lesGustanLasMismasPublicaciones | a ambos les gusta una misma publicacion" ~: (lesGustanLasMismasPublicaciones redC usuario1 usuario3) ~?= True,
    " lesGustanLasMismasPublicaciones | a ambos les gustan las mismas publicaciones" ~: (lesGustanLasMismasPublicaciones redD usuario2 usuario5) ~?= True,
    " lesGustanLasMismasPublicaciones | a ambos les gustan cosas diferentes" ~: (lesGustanLasMismasPublicaciones redC usuario4 usuario5) ~?= False,  
    " lesGustanLasMismasPublicaciones | al primero le gustan las mismas, excepto por una" ~: (lesGustanLasMismasPublicaciones redB usuario2 usuario5) ~?= False,    
    " lesGustanLasMismasPublicaciones | al segundo le gustan las mismas, excepto por una" ~: (lesGustanLasMismasPublicaciones redB usuario5 usuario2) ~?= False,  
    " lesGustanLasMismasPublicaciones | solo el primero, no tiene publicaciones que le gusten" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario2) ~?= False,
    " lesGustanLasMismasPublicaciones | solo el segundo, no tiene publicaciones que le gusten" ~: (lesGustanLasMismasPublicaciones redB usuario2 usuario1) ~?= False,
    
    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,
    " tieneUnSeguidorFiel | el usuario tiene mas de un seguidor fiel" ~: (tieneUnSeguidorFiel redD usuario1) ~?= True,
    " tieneUnSeguidorFiel | el usuario no tiene publicaciones" ~: (tieneUnSeguidorFiel redB usuario5) ~?= False,
    " tieneUnSeguidorFiel | el ususario tiene likes, pero no un seguidor fiel" ~: (tieneUnSeguidorFiel redC usuario1) ~?= False,
    " tieneUnSeguidorFiel | el ususario tiene likes, pero no un seguidor fiel (pub sin likes)" ~: (tieneUnSeguidorFiel redB usuario1) ~?= False,
    " tieneUnSeguidorFiel | el usuario tiene una sola publicacion y tiene un like" ~: (tieneUnSeguidorFiel redC usuario3) ~?= True,
    " tieneUnSeguidorFiel | el usuario tiene una sola publicacion y tiene mas de un like" ~: (tieneUnSeguidorFiel redC usuario4) ~?= True,
    " tieneUnSeguidorFiel | el usuario tiene una sola publicacion y no tiene like" ~: (tieneUnSeguidorFiel redD usuario2) ~?= False,

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True,
    " existeSecuenciaDeAmigos | red de solo 2 & u1 amigo u2" ~: (existeSecuenciaDeAmigos redDeSolo2Relacionados usuario1 usuario2) ~?= True,
    " existeSecuenciaDeAmigos | red de solo 2 & u1 no amigo u2" ~: (existeSecuenciaDeAmigos redDeSolo2NoRelacionados usuario1 usuario2) ~?= False,
    " existeSecuenciaDeAmigos | u1 no tiene amigos" ~: (existeSecuenciaDeAmigos redB usuario5 usuario3) ~?= False,
    " existeSecuenciaDeAmigos | u1 tiene un solo amigo & u' no tiene mas amigos" ~: (existeSecuenciaDeAmigos redRelacion usuario5 usuario3) ~?= False,
    " existeSecuenciaDeAmigos | u1 tiene un solo amigo & u' es amigo de u2" ~: (existeSecuenciaDeAmigos redRelacion usuario1 usuario3) ~?= True,
    " existeSecuenciaDeAmigos | u1 tiene un solo amigo & hay secuencia entre u' y u2" ~: (existeSecuenciaDeAmigos redRelacion usuario1 usuario4) ~?= True,
    " existeSecuenciaDeAmigos | u1 tiene un solo amigo & no hay secuencia entre u' y u2" ~: (existeSecuenciaDeAmigos redRelacion usuario1 usuario5) ~?= False,
    " existeSecuenciaDeAmigos | u1 tiene amigos, & es amigo de u2" ~: (existeSecuenciaDeAmigos redRelacion usuario3 usuario4) ~?= True,
    " existeSecuenciaDeAmigos | u1 tiene amigos, & puede llegar a u2" ~: (existeSecuenciaDeAmigos redMasRelaciones usuario1 usuario4) ~?= True,
    " existeSecuenciaDeAmigos | u1 tiene amigos, & no puede llegar a u2" ~: (existeSecuenciaDeAmigos redRelacion usuario1 usuario5) ~?= False
 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Andres")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion1_5 = (usuario1, usuario5)
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion5_6 = (usuario5, usuario6)

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

-- NO VARIAR
usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

--Redes utilies
redVacia = ([], [], [])

redUnicoUsuario = ([usuario1], [], [])

redConNombresRepetidos = ([usuario1, usuario2, usuario5], [], [])

redDeSolo2NoRelacionados = ([usuario1, usuario2], [], [])
redDeSolo2Relacionados = ([usuario1, usuario2], [(usuario1, usuario2)], [])

usuariosRelacion = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6]
relacionesRedRelaciones = [relacion1_2, relacion2_3, relacion2_4, relacion3_4, relacion5_6]
redRelacion = (usuariosRelacion, relacionesRedRelaciones, [])

redMasRelaciones = ([usuario1, usuario2, usuario3, usuario4, usuario5, usuario6], [relacion1_2, relacion2_3, relacion3_4], [])

-- POMI
usuariosC = [usuario1, usuario3, usuario4, usuario5]
relacionesC = [relacion3_4]
publicacionesC = [publicacion1_1, publicacion1_2, publicacion1_5, publicacion3_3, publicacion4_3]
redC = (usuariosC, relacionesC, publicacionesC)

usuariosD = [usuario1, usuario2, usuario3, usuario5]
relacionesD = [relacion2_3]
publicacionesD = [publicacion1_3, publicacion2_1, publicacion3_3]
redD = (usuariosD, relacionesD, publicacionesD)