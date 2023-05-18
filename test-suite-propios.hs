import Test.HUnit
import Solucion

main = runTestTT tests

tests = test [
    " nombresDeUsuarios 1" ~: (testSuiteEj1),

    " amigosDe 1" ~: (testSuiteEj2),

    " cantidadDeAmigos 1" ~: (testSuiteEj3),

    " usuarioConMasAmigos 1" ~: (testSuiteEj4),

    " estaRobertoCarlos 1" ~: (testSuiteEj5),

    " publicacionesDe 1" ~: (testSuiteEj6),

    " publicacionesQueLeGustanA 1" ~: (testSuiteEj7),

    " lesGustanLasMismasPublicaciones 2" ~: (testSuiteEj8),

    " tieneUnSeguidorFiel 1" ~: (testSuiteEj9)

   -- " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redF usuario1 usuario3) ~?= True -}
    ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- Test Suite por ejercicio -- 

testSuiteEj1 = test [
    "Caso 1: redVacia" ~: nombresDeUsuarios redV ~?= [],
    "Caso 2: soloUnUsuarioEnRed" ~: nombresDeUsuarios redUno ~?= ["Tiago"],
    "Caso 3: redNombreRepetido" ~: nombresDeUsuarios redE ~?= ["Tiago","Lautaro","Rafael", "Sabrina"],
    "Caso 4: casoNormal" ~: nombresDeUsuarios redF ~?= ["Tiago","Lautaro","Rafael", "Sabrina"]
    ]

testSuiteEj2 = test [
    "Caso 1: soloUnUsuarioEnRed" ~: amigosDe redUno usuario1 ~?= [],
    --"Caso 2: usuarioSinAmigos" ~: amigosDe redF usuario1 ~?= [],  -- dilema si sobretestea o no, mismo para 3 y 4.
    "Caso 3: casoNormal" ~: amigosDe redF usuario2 ~?= [usuario1, usuario3, usuario4]
    ]


testSuiteEj3 = test [
    "Caso 1: soloUnUsuarioEnRed" ~: cantidadDeAmigos redUno usuario1 ~?= 0,
    --"Caso 2: usuarioSinAmigos" ~: cantidadDeAmigos redF usuario1 ~?= 0,
    "Caso 3: casoNormal" ~: cantidadDeAmigos redF usuario2 ~?= 3
    ]    

testSuiteEj4 = test [
    "Caso 1: soloUnUsuarioEnRed" ~: usuarioConMasAmigos redUno ~?= usuario1,
    --"Caso 2: nadieTieneAmigos" ~: usuarioConMasAmigos redDos ~?= usuario1,
    "Caso 3: usuariosConMismaCantAmigos" ~: expectAny (usuarioConMasAmigos redF) [usuario2, usuario3, usuario4],
    "Caso 4: casoNormal" ~: usuarioConMasAmigos redD ~?=  usuario1
    ]

testSuiteEj5 = test [
     "Caso 1: redVacia" ~: estaRobertoCarlos redV ~?= False,
     "Caso 2: soloUnUsuarioEnRed" ~: estaRobertoCarlos redUno ~?= False,
     "Caso 3: nadieTieneDiezOMasAmigos" ~: estaRobertoCarlos redC ~?= False,
     "Caso 4: CasoNormal" ~: estaRobertoCarlos redD ~?= True
    ]

testSuiteEj6 = test [
     "Caso 1: soloUnUsuarioEnRed" ~: publicacionesDe redUno usuario1 ~?= [],
     --"Caso 2: usuarioSinPublicaciones" ~: publicacionesDe redE usuario4 ~?= []
     "Caso 3: casoNormal" ~: publicacionesDe redF usuario2 ~?= [publicacion2_1, publicacion2_2] 
    ]

testSuiteEj7 = test [
     "Caso 1: soloUnUsuarioEnRed" ~: publicacionesQueLeGustanA redUno usuario1 ~?= [],
     "Caso 2: Usuario no likeo ninguna publicacion" ~: publicacionesQueLeGustanA redF usuario3 ~?= [],
     "Caso 3: Caso normal" ~: publicacionesQueLeGustanA redF usuario4 ~?= [publicacion2_1, publicacion2_2]
    ] 

testSuiteEj8 = test [
    "Caso 1: No les gustan las mismas publicaciones" ~: lesGustanLasMismasPublicaciones redF usuario1 usuario4 ~?= False,
    "Caso 2: Ambos usuarios no les gusta ninguna publicacion" ~: lesGustanLasMismasPublicaciones redDos usuario1 usuario2 ~?= True
   -- "Caso 3: Ambos usuarios les gusta las mismas publicaciones"
    ]


testSuiteEj9 = test [
     "Caso 1: Usuario sin publicaciones" ~: tieneUnSeguidorFiel redC usuario4 ~?= False, 
     "Caso 1: Usuario sin likes en sus publicaciones" ~: tieneUnSeguidorFiel redG usuario1 ~?= False,
     "Caso 3: Usuario con publicaciones sin seguidor fiel" ~: tieneUnSeguidorFiel redF usuario3 ~?= False,
     "Caso 4: Usuario con seguidor fiel" ~: tieneUnSeguidorFiel redF usuario2 ~?= True
    ]

{-
testSuiteEj10 = test [
     "Caso 1: Usuario 1 y usuario 2 son amigos" ~: existeSecuenciaDeAmigos   ~?= True,
     "Caso 2: Existe posible cadena de amigos entre usuario 1 y usuario 2" ~: existeSecuenciaDeAmigos   ~?= True,
     "Caso 3: No existe posible cadena de amigos entre usuario 1 y usuario 2" ~: existeSecuenciaDeAmigos  ~?= False
    ]
-}

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

--redA y redB son las proporcionadas por test de catedra
usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

-- redes armadas por nosotros
usuariosC = [usuario1, usuario2, usuario3, usuario4, usuario5]
relacionesC = [relacion1_2, relacion1_5, relacion2_3, relacion3_5, relacion1_4]
publicacionesC = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redC = (usuariosC, relacionesC, publicacionesC)

usuariosD = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11]
relacionesD = [relacion1_2, relacion1_3, relacion1_4, relacion1_5, relacion1_6, relacion1_7, relacion1_8, relacion1_9, relacion1_10, relacion1_11, relacion1_12, relacion2_3, relacion3_5]
publicacionesD = [publicacion1_3, publicacion1_4, publicacion1_5]
redD = (usuariosD, relacionesD, publicacionesD)

usuariosE = [usuario1, usuario2, usuario3, usuario4, usuario5]
relacionesE = [relacion1_2, relacion1_5, relacion2_3, relacion3_5]
publicacionesE = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redE = (usuariosE, relacionesE, publicacionesE)

usuariosF = [usuario1, usuario2, usuario3, usuario4]
relacionesF = [relacion1_2, relacion2_3, relacion2_4, relacion3_4, relacion3_5]
publicacionesF= [publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_3]
redF = (usuariosF, relacionesF, publicacionesF)

usuariosG = [usuario1, usuario2]
relacionesG = [relacion1_2]
publicacionesG = [publicacion1_4]
redG = (usuariosG, relacionesG, publicacionesG)

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
usuario12 = (12, "Duki")

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
relacion1_12 = (usuario1, usuario12)

relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)

relacion3_4 = (usuario4, usuario3)
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
