import Test.HUnit
import Solucion

main = runTestTT tests

tests = test [
    " nombresDeUsuarios 1" ~: (testSuitEj1),

    " amigosDe 1" ~: (testSuitEj2),

    " cantidadDeAmigos 1" ~: (testSuitEj3),

    " usuarioConMasAmigos 1" ~: (testSuitEj4),

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False
{-
    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

   -- " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True -}
    ]

-- expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)



testSuitEj1 = test [
    "redVacia" ~: nombresDeUsuarios redV ~?= [],
    "soloUnUsuarioEnRed" ~: nombresDeUsuarios redUno ~?= ["Tiago"],
    "redNombreRepetido" ~: nombresDeUsuarios redB ~?= ["Tiago","Lautaro","Rafael", "Sabrina"],
    "casoNormal" ~: nombresDeUsuarios redA ~?= ["Tiago","Lautaro","Rafael", "Sabrina"]
    ]

testSuitEj2 = test [
    "soloUnUsuarioEnRed" ~: amigosDe redUno usuario1 ~?= [],
    "usuarioSinAmigos" ~: amigosDe redA usuario1 ~?= [],
    "casoNormal" ~: amigosDe redA usuario2 ~?= [usuario3, usuario4]
    ]


testSuitEj3 = test [
    "soloUnUsuarioEnRed" ~: cantidadDeAmigos redUno usuario1 ~?= 0,
    "usuarioSinAmigos" ~: cantidadDeAmigos redA usuario1 ~?= 0,
    "casoNormal" ~: cantidadDeAmigos redA usuario2 ~?= 2
    ]    

testSuitEj4 = test [
    "soloUnUsuarioEnRed" ~: usuarioConMasAmigos redUno ~?= usuario1,
    "nadieTieneAmigos" ~: usuarioConMasAmigos redDos ~?= usuario1,
    "mismaCantAmigos" ~: usuarioConMasAmigos redC ~?= usuario1,
    "casoNormal" ~: usuarioConMasAmigos redB  ~?=  usuario2
    ]

testSuitEj5 = test [
     "soloUnUsuarioEnRed" ~: estaRobertoCarlos redUno ~?= False,
     "nadieTieneDiezOMasAmigos" ~: estaRobertoCarlos redC ~?= False,
     "CasoNormal" ~: estaRobertoCarlos redD ~?= True
    ]

usuariosV = []
relacionesV = []
publicacionesV = []
redV = (usuariosV, relacionesV, publicacionesV)

usuariosUno = [usuario1]
relacionesUno = []
publicacionesUno = []
redUno = (usuariosUno, relacionesUno, publicacionesUno)

usuariosDos = [usuario1, usuario2]
relacionesDos = []
publicacionesDos = []
redDos = (usuariosDos, relacionesDos, publicacionesDos)


--redA: 1 no tiene relaciones con otros usuarios ni publicaciones
usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_3]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario4, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

usuariosC = [usuario1, usuario2, usuario3, usuario4, usuario5]
relacionesC = [relacion1_2, relacion2_3, relacion1_4]
publicacionesC = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redC = (usuariosC, relacionesC, publicacionesC)

usuariosD = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11]
relacionesD = [relacion1_2, relacion1_3, relacion1_4, relacion1_5, relacion1_6, relacion1_7, relacion1_8, relacion1_9, relacion1_10, relacion1_11]
publicacionesD = [publicacion1_3, publicacion1_4, publicacion1_5]
redD = (usuariosD, relacionesD, publicacionesD)



-- Ejemplos
usuario1 = (1, "Tiago")
usuario2 = (2, "Lautaro")
usuario3 = (3, "Rafael")
usuario4 = (4, "Sabrina")
usuario5 = (5, "Lautaro")
usuario6 = (6, "Juan")
usuario7 = (7, "Micaela")
usuario8 = (8, "Camila")
usuario9 = (9, "Jorgina")
usuario10 = (10, "Berta")
usuario11 = (11, "Magali")


relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion1_5 = (usuario5, usuario1)
relacion1_6 = (usuario1, usuario6)
relacion1_7 = (usuario7, usuario1)
relacion1_8 = (usuario8, usuario1)
relacion1_9 = (usuario9, usuario1)
relacion1_10 = (usuario1, usuario10)
relacion1_11 = (usuario1, usuario11)
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

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
