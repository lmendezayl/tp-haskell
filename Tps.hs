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

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red | usuarios red == [] = []
                      | otherwise= [nombreDeUsuario(head (usuarios red))] ++ nombresDeUsuarios (tail(usuarios red),relaciones red,publicaciones red)

-- describir qué hace la función: .....

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red  usu | relaciones red == [] = []
                  | nombreDeUsuario (fst(head(relaciones red))) == (nombreDeUsuario usu) = [snd(head(relaciones red))] ++ amigosDe (usuarios red,tail (relaciones red),publicaciones red) usu
                  | nombreDeUsuario (snd(head(relaciones red))) == (nombreDeUsuario usu) = [fst(head(relaciones red))] ++ amigosDe (usuarios red,tail (relaciones red),publicaciones red) usu

-- describir qué hace la función: .....
largo :: [a] ->Int
largo []=0
largo (x:xs) = 1 + largo xs

cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red usu = largo  ( amigosDe red usu )

-- describir qué hace la función: .....

usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red | largo (usuarios red) == 2 && cantidadDeAmigos red (usuarioN (usuarios red) 1) >= cantidadDeAmigos red (usuarioN (usuarios red) 2) = usuarioN (usuarios red) 1
                        | largo (usuarios red) == 2 && cantidadDeAmigos red (usuarioN (usuarios red) 1) <  cantidadDeAmigos red (usuarioN (usuarios red) 2) = usuarioN (usuarios red) 2
                        | cantidadDeAmigos red (usuarioN (usuarios red) 1) >= cantidadDeAmigos red (usuarioN (usuarios red) 2) = usuarioConMasAmigos (quitarUsuarios  red 2) 
                        | cantidadDeAmigos red (usuarioN (usuarios red) 1) <  cantidadDeAmigos red (usuarioN (usuarios red) 2) = usuarioConMasAmigos (quitarUsuarios red 1)

usuarioN :: [Usuario] -> Integer -> Usuario
usuarioN (u:us) x | x == 1 = u
                  | otherwise = usuarioN us (x-1)

quitarUsuarios :: RedSocial -> Integer -> RedSocial
quitarUsuarios (u:us, rels, pubs) 1 = (us, rels, pubs)
quitarUsuarios ((u1:u2:us), rels, pubs) 2 = ((u1:us), rels, pubs)

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red | usuarios red == [] = False
                      | cantidadDeAmigos red (head(usuarios(red))) >1000000 = True
                      | otherwise= estaRobertoCarlos (tail(usuarios red),relaciones red,publicaciones red)

-- describir qué hace la función: .....

comparar :: RedSocial -> Usuario -> Bool
comparar (a,b,(usu,texto,listaDeUsuarios):resto) usuario | usu == usuario = True
                                                         | otherwise= False

publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red usuar | publicaciones red == [] = []
                          | (comparar red usuar == True) = [head (publicaciones red)]++publicacionesDe (usuarios red,relaciones red, tail(publicaciones red)) usuar
                          | otherwise= publicacionesDe (usuarios red,relaciones red, tail(publicaciones red)) usuar


-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u | publicaciones red == [] = []
				 | u == (head (likesDePublicacion (head (publicaciones red)))) =  [head (publicaciones red)] ++  publicacionesQueLeGustanA (usuarios red, relaciones red, tail(publicaciones red)) u
				 |  otherwise= publicacionesQueLeGustanA (usuarios red, relaciones red, tail(publicaciones red)) u

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....

quitarTodasLasRelaciones :: RedSocial -> [Usuario] -> RedSocial
quitarTodasLasRelaciones (a,(usu1,usu2):resto,b) (x:xs) | usu== head xs = quitar (usuarios red, tail (relaciones red),publicaciones red) 
						  | otherwise=


existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos =undefined















-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@






















-- Completar con los datos del grupo
--
-- Nombre de Grupo: Monitor
-- Integrante 1: Germán Santoandré, germansantoandre@gmail.com, LU 83/21
-- Integrante 2: Agustín Fernández, fernandez.agus2000@gmail.com, LU 258/21
-- Integrante 3: Daniel Tomchinsky, danitomchinsky@gmail.com, DNI 40731816
-- Integrante 4: Rubén Diego Martín, rdmartin38@gmail.com, LU 0278/84


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

-- Funcion agregada
textoDePublicacion :: Publicacion -> String
textoDePublicacion ( _, txt, _) = txt

--------------------------------------------------------------------------
-- Ejercicios

-- Ejercicio 1
-- Se le pasa como parametro una RedSocial y devuelve
-- una lista con los nombres de los usuarios de esa RedSocial
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres (usuarios red)


-- Se le pasa como parametro una lista de Usuarios: (id,"nombre") y devuelve
-- una lista solo con los nombres de los Usuarios.
-- Si dos Usuarios se llamam igual, deja uno solo en la lista
proyectarNombres :: [Usuario] -> [String]
proyectarNombres []       = []
proyectarNombres [us]     = [nombreDeUsuario us]
proyectarNombres (us:usx) | pertenece us usx = proyectarNombres usx
                          | not (pertenece us usx) = nombreDeUsuario us : proyectarNombres usx
-- ###########################################################################################


-- Ejercicio 2
-- Se le pasa como parametros una RedSocial y un Usuario y devuelve
-- una lista de Usuarios (sin repetir) que estan relacionados (son amigos) con el Usuario pasado como parametro
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (_, [_], _) u            = []
amigosDe (_, (u1, u2): rels, _) u | u1 == u && not (pertenece (u1,u2) rels) && not (pertenece (u2,u1) rels) = u2 : amigosDe ([], rels, []) u
                                  | u2 == u && not (pertenece (u1,u2) rels) && not (pertenece (u2,u1) rels) = u1 : amigosDe ([], rels, []) u
                                  | otherwise = amigosDe ([], rels, []) u
-- ################################################################################################


-- Ejercicio 3
-- Se le pasa como parametro una RedSocial y un Usuario y devuelve
-- la cantidad de amigos relacionados con el Usuario pasado como parametro
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u)
-- ################################################################################################


-- Ejercicio 4
-- Se le pasa como parametro una RedSocial y devuelve
-- el Usuario (Id, Nombre) con mayor cantidad de amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([u1], _, _) = u1
usuarioConMasAmigos (u1: u2: ux, rels, _) | cantidadDeAmigos ([u1], rels, []) u1 > cantidadDeAmigos ([u2], rels, []) u2 = usuarioConMasAmigos (u1: ux, rels, [])
                                          | otherwise = usuarioConMasAmigos (u2: ux, rels, [])
-- ################################################################################################


-- Ejercicio 5
-- True si algún Usuario de la RedSocial tiene mas de 10 de Relaciones (Amigos)
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([u1], rels, _)   = cantidadDeAmigos ([u1], rels, []) u1 > 10
estaRobertoCarlos (u1:usx, rels, _) | cantidadDeAmigos (u1:usx, rels, []) u1 > 10 = True
                                    | otherwise = estaRobertoCarlos (usx, rels, [])
-- ################################################################################################


-- Ejercicio 6
-- Se le pasa como parametro una RedSocial y un Usuario y devuelve
-- una lista de las Publicaciones a las que el Usuario pasado como parametro
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_,_,[]) _ = []
publicacionesDe (_,_,(us,pub,likes):pubs) u | us == u && not (pertenece (us,pub,likes) pubs) = (us,pub,likes) : publicacionesDe ([],[],pubs) u
                                            | otherwise = publicacionesDe ([],[],pubs) u
-- ################################################################################################


-- Ejercicio 7
-- Se le pasa como parametro una RedSocial y un Usuario y devuelve
-- una lista de las Publicaciones a las que el Usuario pasado como parametro le dio Like
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = proyectarPublicacionesDe (publicaciones red) u

proyectarPublicacionesDe :: [Publicacion] -> Usuario -> [Publicacion]
proyectarPublicacionesDe [] _ = []
proyectarPublicacionesDe (pub:pubs) u | pertenece u (likesDePublicacion pub) && not (pertenece pub pubs) = pub : proyectarPublicacionesDe pubs u
                                      | otherwise = proyectarPublicacionesDe pubs u
-- #############################################################################################################################################


-- Ejercicio 8
-- Se le pasa como parametro una RedSocial y dos Usuarios y devuelve
-- True si y solo si a ambos Usuarios les gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)
-- ##############################################################################################################################


-- Ejercicio 9
-- Se le pasa como parametro una RedSocial y un Usuario y devuelve
-- True si hay otro usuario de la red, que le dio Like a todas las publicaciones del Usuario pasado como parametro
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = longitud (publicacionesDe red u)>0 && buscarSeguidorFiel red u (quitarTodos u (usuarios red))


buscarSeguidorFiel :: RedSocial -> Usuario -> [Usuario] -> Bool
buscarSeguidorFiel red u [us]      = esSeguidorFiel red u us
buscarSeguidorFiel red u (us:usx)  = esSeguidorFiel red u us || buscarSeguidorFiel red u usx 

esSeguidorFiel :: RedSocial -> Usuario -> Usuario -> Bool
esSeguidorFiel red usp usl = estaIncluido  uspPublica uslLike
                             where uspPublica = usuarioYlikeDePublicacion (publicacionesDe red usp)
                                   uslLike    = usuarioYlikeDePublicacion (publicacionesQueLeGustanA red usl)

usuarioYlikeDePublicacion :: [Publicacion] -> [(Usuario, [Usuario])]
usuarioYlikeDePublicacion []         = []
usuarioYlikeDePublicacion [pubs]     = [(usuarioDePublicacion pubs,likesDePublicacion pubs)]
usuarioYlikeDePublicacion (pub:pubs) = (usuarioDePublicacion pub,likesDePublicacion pub) : usuarioYlikeDePublicacion pubs
-- ######################################################################################################################


-- Ejercicio 10
-- Se le pasa como parametro una RedSocial y dos Usuarios y devuelve
-- True si y solo si hay una cadenaDeAmigos entre los dos usuarios pasados como parametros
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = longitud (usuarios red)>=2 &&
                                    empiezaCon u1 (usuarios red) && terminaCon u2 (usuarios red) &&
                                    sonDeLaRed red (usuarios red) && cadenaDeAmigos (usuarios red) red
-- ########################################################################################################



--------------------------------------------------------------------------
-- Predicados Auxiliares

-- Verifica que al menos un elemento está en la lista
-- Si la lista está vacia devuelve False
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece y (x:xs)  | y==x = True
                    | otherwise = pertenece y xs

-- Verifica que todos los elementos de una lista estan en la otra lista y ambas listas tienen la misma cantidad de elementos
-- Si las listas pasadas estan vacias devuelve False
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = False
mismosElementos xs ys = longitud xs == longitud ys && estaIncluido xs ys && estaIncluido ys xs

-- Verifica que la RedSocial cumpla que tenga Usuarios Validos, Relaciones Validas y Publicaciones Validas
redSocialValida :: RedSocial -> Bool
redSocialValida red = usuariosValidos (usuarios red) &&
                      relacionesValidas (usuarios red) (relaciones red) &&
                      publicacionesValidas (usuarios red) (publicaciones red)
-- ################################################################################################


-- Verifica que dada una lista de Usuarios, todos tienen una ID>0, el nombre al menos tenga un caracter
-- y que no haya ID repetidos en dicha lista de Usuarios
usuariosValidos :: [Usuario] -> Bool
usuariosValidos []   = True
usuariosValidos [us] = usuarioValido us 
usuariosValidos us   = usuarioValido (head us) && noHayIdRepetidos us && usuariosValidos (tail us)

-- Verifica que dado un Usuario, su ID>0 y el nombre tenga por lo menos un caracter
usuarioValido :: Usuario -> Bool
usuarioValido u = idDeUsuario u > 0 && longitud (nombreDeUsuario u) >0

-- Verifica que dada una lista de Usuarios no haya ID repetidos en dicha lista
-- Si no hay Usuarios o hay un solo Usuario, devuelve True
noHayIdRepetidos :: [Usuario] -> Bool
noHayIdRepetidos []          = True
noHayIdRepetidos [_]         = True
noHayIdRepetidos ((id,_):us) = not (pertenece id (proyectarID us)) && noHayIdRepetidos us
-- ###############################################################################################


-- relacionesValidas: son los que sus integrantes sean Usuarios de la red y
--                    que la relacion no esté armada con un unico usuario
relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas us rels = usuariosDeRelacionValidos us rels &&
                            relacionesAsimetricas rels &&
                            noHayRelacionesRepetidas rels 

-- Verifica que para todos los Usuarios de la lista pasado como parametro, si tienen alguna Relaciones definida,
-- la otra parte de la Relacion, es un Usuario valido y es diferente al Usuario que se está validando
usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos _ []                = True
usuariosDeRelacionValidos us [(us1,us2)]      = pertenece us1 us && pertenece us2 us && us1/=us2
usuariosDeRelacionValidos us ((us1,us2):rels) = pertenece us1 us && pertenece us2 us && us1/=us2 &&
                                                usuariosDeRelacionValidos us rels

-- En todo el conjunto de relaciones (amistades) no hay permutaciones de relaciones
-- True : significa que todas las relaciones son únicas
-- False: significa que encontró una permutacion entre las relaciones
relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [_]            = True
relacionesAsimetricas ((u1,u2):rels) = not (pertenece (u2,u1) rels) && relacionesAsimetricas rels

-- Si no hay Relacion o hay solo una Relacion, entonces noHayRelacionesRepetidas devuelve True
-- Si hay mas de una Relacion verifica que no se repita una relacion
noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas []         = True
noHayRelacionesRepetidas [_]        = True
noHayRelacionesRepetidas (rel:rels) = noHayRelacionRepetida rel rels && noHayRelacionesRepetidas rels

-- Dada una Relacion (Amistad) pasada como parametro, verifica que en el conjunto de todas las Relaciones
-- no se repita con dicha relacion
noHayRelacionRepetida :: Relacion -> [Relacion] -> Bool
noHayRelacionRepetida _ []     = True
noHayRelacionRepetida _ [_]    = True
noHayRelacionRepetida rel rels = (fst (fst rel) /= fst (fst (head rels)) || fst (snd rel) /= fst (snd (head rels))) &&
                                 noHayRelacionRepetida rel (tail rels)
-- ################################################################################################


-- Una Publicacion es valida si cumple las tres condiciones siguientes
-- usuariosDePublicacionSonUsuariosDeRed 
-- usuariosDeLikeDePublicacionSonUsuariosDeRed
-- noHayPublicacionesRepetidas pubs
publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas us pubs = usuariosDePublicacionSonUsuariosDeRed us pubs && 
                               usuariosDeLikeDePublicacionSonUsuariosDeRed us pubs &&
                               noHayPublicacionesRepetidas pubs

-- Dada una lista de Publicaciones verifica que los Usuarios de cada Publicacion
-- sean Usuarios de la RedSocial. Si no hay Publicaciones devuelve True
-- Si hay una o mas publicaciones devuelve True si todas las Publicaciones son de Usuarios de la Red
-- Devuelve False si por lo menos una de las Publicaciones no fue hecha por ningun Usuario de la Red
usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed _ []          = True
usuariosDePublicacionSonUsuariosDeRed us [pub]      = pertenece (usuarioDePublicacion pub) us
usuariosDePublicacionSonUsuariosDeRed us (pub:pubs) = pertenece (usuarioDePublicacion pub) us &&
                                                      usuariosDePublicacionSonUsuariosDeRed us pubs

-- Verifica que los Usuarios que dan Like a una Publicacion son Usuarios de la Red
-- Si no hay Publicaciones devuelve True
usuariosDeLikeDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDeLikeDePublicacionSonUsuariosDeRed _ []          = True
usuariosDeLikeDePublicacionSonUsuariosDeRed us [pub]      = usuariosLikeValidos us (likesDePublicacion pub)
usuariosDeLikeDePublicacionSonUsuariosDeRed us (pub:pubs) = usuariosLikeValidos us (likesDePublicacion pub) &&
                                                            usuariosDeLikeDePublicacionSonUsuariosDeRed us pubs

-- Verifica que la lista de Usuarios pasado como segundo parametro, esten en la otra lista de Usuarios
-- pasado como primer paramtro
-- Si no hay Usuarios como segundo parametro, devuelve True
usuariosLikeValidos :: [Usuario] -> [Usuario] -> Bool
usuariosLikeValidos _ []         = True
usuariosLikeValidos us [usl]     = pertenece usl us
usuariosLikeValidos us (usx:usl) = pertenece usx us && usuariosLikeValidos us usl

-- Verifica que todas las publicaciones pasadas como parametros, tienen "Usuarios distintos" o "textos distintos"
-- Si no hay Publicaciones o hay una sola Publicacion, devuelve True
noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas []                = True
noHayPublicacionesRepetidas [_]               = True
noHayPublicacionesRepetidas ((us,txt,_):pubs) = not (pertenece (us,txt) ustxt) && noHayPublicacionesRepetidas pubs
                                                where ustxt = usuarioYtextoDePublicacion pubs
-- ########################################################################################################################


-- Verifica que dada una lista de Usuarios, del primero al ultimo de dicha lista,
-- estan relacionados (son Amigos) uno respecto al que le siguiente hasta el final
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [us1,us2] red = relacionadosDirecto us1 us2 red
cadenaDeAmigos (us1:usx) red = relacionadosDirecto us1 (head usx) red && cadenaDeAmigos usx red

-- Verifica que dado dos Usuarios y una RedSocial, dichos Usuarios estan relacionados (Son Amigos)
-- Es indistinto en que orden esten en la Relacion
relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 red = pertenece (u1,u2) (relaciones red) || pertenece (u2,u1) (relaciones red)
-- #####################################################################################################


sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed red []       = False
sonDeLaRed red [us]     = pertenece us (usuarios red)
sonDeLaRed red (us:usx) = pertenece us (usuarios red) && sonDeLaRed red usx

empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon e l = e == head l

terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon e l = e == ultimo l

sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos []     = True
sinRepetidos [_]    = True
sinRepetidos (x:xs) = not (pertenece x xs) && sinRepetidos xs


-- Otras funciones auxiliares utilizadas
principio :: [t] -> [t]
principio [x] = []
principio (x:xs) = x : principio xs

ultimo :: [t] -> t
ultimo [xs] = xs
ultimo xs = ultimo (tail xs)

longitud :: [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

estaIncluido :: (Eq t) => [t] -> [t] -> Bool
estaIncluido [x] ys = pertenece x ys
estaIncluido (x:xs) ys = pertenece x ys && estaIncluido xs ys

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos e [] = []
quitarTodos e [x]    | e==x = []
quitarTodos e (x:xs) | e==x = quitarTodos e xs
                     | otherwise = x : quitarTodos e xs

maximo :: [Int] -> Int
maximo []     = 0
maximo [x]    = x
maximo (x:xs) | x >= maximo xs = x
              | otherwise = maximo xs

-- Devuelve una lista de Usuarios y texto de la Publicaciones
usuarioYtextoDePublicacion :: [Publicacion] -> [(Usuario, String)]
usuarioYtextoDePublicacion []         = []
usuarioYtextoDePublicacion [pubs]     = [(usuarioDePublicacion pubs,textoDePublicacion pubs)]
usuarioYtextoDePublicacion (pub:pubs) = (usuarioDePublicacion pub,textoDePublicacion pub) : usuarioYtextoDePublicacion pubs

-- Se le pasa como parametro una lista de Usuarios: (ID,"nombre") y devuelve
-- una lista solo con los ID de los Usuarios 
proyectarID :: [Usuario] -> [Integer]
proyectarID []       = []
proyectarID [us]     = [idDeUsuario us]
proyectarID (us:usx) = idDeUsuario us : proyectarID usx

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden de los usuarios es indistinto
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


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, 
                  publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario5, usuario2, usuario3]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, 
                  publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

test01 = nombresDeUsuarios redA
test02 = amigosDe redA usuario1
test03 = cantidadDeAmigos redA usuario1
test04 = usuarioConMasAmigos redA
test05 = estaRobertoCarlos redA
test06 = publicacionesDe redA usuario2
test07 = publicacionesQueLeGustanA redA usuario1
test08 = lesGustanLasMismasPublicaciones redB usuario1 usuario3
test09 = tieneUnSeguidorFiel redA usuario1
test10 = existeSecuenciaDeAmigos redA usuario1 usuario3