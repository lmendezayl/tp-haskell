module Solucion where
-- Completar con los datos del grupo
--
-- Nombre de Grupo: No balls
-- Integrante 1: Tiago Busso, bussotiago@gmail.com, 570/23
-- Integrante 2: Lautaro Mendez Ayala, lmendezayl@gmail.com, 799/23
-- Integrante 3: Rafael Monges Luces, rafaml2003@gmail.com, 888/23
-- Integrante 4: Sabrina Veronica Koch, sabriverokoch@gmail.com, 684/23

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
--1
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = sumarSegundosDistintos (usuarios red)

--Funciones auxiliars ejercicio "nombresDeUsuarios"

--La funcion sumarSegundosDistintos suma los segundos elementos de las tuplas del type Usuario siempre y cuando sean distintos 
sumarSegundosDistintos :: [Usuario] -> [String]
sumarSegundosDistintos [] = []
sumarSegundosDistintos ((_,nombre):xs) | estaEnLista nombre xs = sumarSegundosDistintos xs 
                                       | otherwise = nombre : sumarSegundosDistintos xs 

--La funcion estaEnLista devuelve un booleano dependiendo de si el String que se ingresa esta en [Usuario]
estaEnLista :: String -> [Usuario] -> Bool
estaEnLista _ [] = False
estaEnLista y ((_,nombre):xs) = y == nombre || estaEnLista y xs




--2
--La funcion amigosDe devuelve los usuarios relacionados con el usuario ingresado, ademas asegura que no haya res repetidos.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u  = sumarUsuariosDistintos u (relaciones red)  

--La funcion sumarUsuariosDistintos concatena los usuarios que no esten repetidos.
sumarUsuariosDistintos :: Usuario -> [Relacion] -> [Usuario]
sumarUsuariosDistintos u [] = []
sumarUsuariosDistintos u ((u1,u2):xs) | u == u1 && not(estaRepetidaRel (u1, u2) xs) = [u2] ++ sumarUsuariosDistintos u xs
                                      | u == u2 && not(estaRepetidaRel (u1, u2) xs) = [u1] ++ sumarUsuariosDistintos u xs
                                      | otherwise = sumarUsuariosDistintos u xs


estaRepetidaRel :: Relacion -> [Relacion] -> Bool
estaRepetidaRel _ [] = False
estaRepetidaRel (u1, u2) ((u3, u4) : xs) | u1 == u3 && u2 == u4 = True
                                         | u1 == u4 && u2 == u3 = True
                                         | otherwise = False


--3
-- La funcion cantidadDeAmigos devuelve la longitud de la lista amigosDe
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u)

-- Funcion auxiliar "longitud" devuelve la longitud de la lista asociada
longitud :: [Usuario] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--4
-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = comparaUsuarios red (usuarios (red))


comparaUsuarios :: RedSocial -> [Usuario] -> Usuario 
comparaUsuarios red us  | longitud us == 1 = head us 
                        | cantidadDeAmigos red (head (usuarios red)) >= cantidadDeAmigos red (head (tail (usuarios red))) = comparaUsuarios red ((head (usuarios red)) : tail (tail (usuarios red))) --Compara la cantidad de amigos del primer usuario con la cantidad del segundo.
                        | otherwise = comparaUsuarios red (tail(usuarios red))


-- 5
-- La funcion estaRobertoCarlos ingresa una red, evalua cual es el usuario con mas amigos de esa red
-- y a ese usuario le aplica la cantidad de 
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red | pertenece (usuarioConMasAmigos red) (usuarios red) && cantidadDeAmigos red (usuarioConMasAmigos red) > 10 = True
                      | otherwise = False

--6
-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = sumaPublisDistintas (publicaciones red) u 

sumaPublisDistintas :: [Publicacion] -> Usuario -> [Publicacion]
sumaPublisDistintas (x:xs) u = if  laPublicacionEsDeU x u && not(estaRepetidaLaPubli x xs)
                                            then x : sumaPublisDistintas xs u   
                                                 else sumaPublisDistintas xs u

laPublicacionEsDeU :: Publicacion -> Usuario -> Bool
laPublicacionEsDeU (u1,str,l) u | u1 == u = True
                                | otherwise = False

estaRepetidaLaPubli :: Publicacion -> [Publicacion] -> Bool
estaRepetidaLaPubli _ [] = False
estaRepetidaLaPubli (u1, str1, l1) ((u2, str2, l2) : xs) | u1 == u2 && str1 == str2 = True
                                                         | otherwise = False


-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
