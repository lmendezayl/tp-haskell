module Solucion where

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

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

--1
-- Devuelve una lista de nombres de usuario dada una red.
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red | usuarios red == [] = [] 
                      | longitud (usuarios red) == 1 = [nombreDeUsuario (head (usuarios red))]
                      | otherwise = eliminarRepetidos (proyectarNombres red)

-- Devuelve una lista con todos los nombres de usuario de la red.
proyectarNombres ::  RedSocial -> [String]
proyectarNombres red | usuarios red == [] = []
                     | otherwise = [nombreDeUsuario (head (usuarios red))] ++ proyectarNombres (tail (usuarios red), relaciones red, publicaciones red)

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

--2
-- Devuelve los usuarios relacionados con el usuario ingresado.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u  = sumarUsuariosDistintos u (relaciones red)  

-- Concatena los usuarios que no esten repetidos.
sumarUsuariosDistintos :: Usuario -> [Relacion] -> [Usuario]
sumarUsuariosDistintos u [] = []
sumarUsuariosDistintos u ((u1,u2):rels) | u == u1 && not(pertenece (u1, u2) rels) = [u2] ++ sumarUsuariosDistintos u rels
                                        | u == u2 && not(pertenece (u1, u2) rels) = [u1] ++ sumarUsuariosDistintos u rels
                                        | otherwise = sumarUsuariosDistintos u rels

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

--3
-- Devuelve la cantidad de amigos de un usuario.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u)

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

--4
-- Devuelve cual es el usuario con mas amigos de la red.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red | longitud (usuarios red) == 1 = head (usuarios red)
                        | cantidadDeAmigos red (head (usuarios red)) >= cantidadDeAmigos red (head (tail (usuarios red))) = usuarioConMasAmigos ((head (usuarios red): tail (tail (usuarios red))), relaciones red, publicaciones red)
                        | otherwise = usuarioConMasAmigos (tail (usuarios red), relaciones red, publicaciones red)
               
-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

-- 5
-- Verifica si existe un usuario que posea mas de 10 amigos en la red.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red | pertenece (usuarioConMasAmigos red) (usuarios red) && cantidadDeAmigos red (usuarioConMasAmigos red) > 10 = True
                      | otherwise = False

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

--6
-- Devuelve la lista de todas las publicaciones subidas por un usuario de la red.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = sumaPublisDistintas (publicaciones red) u 

-- Devuelve una lista que contiene a las publicaciones de un usuario sin repetirse.
sumaPublisDistintas :: [Publicacion] -> Usuario -> [Publicacion]
sumaPublisDistintas [] u = []
sumaPublisDistintas (pub:pubs) u | (usuarioDePublicacion pub == u && not(pertenece pub pubs)) == True = pub : sumaPublisDistintas pubs u   
                                 | otherwise = sumaPublisDistintas pubs u

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

--7
-- Devuelve la lista de publicaciones que le gustan a un usuario de la red.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = sumaSiUsuarioDioLike (publicaciones red) u

-- Devuelve una lista de publicaciones a las que un usuario dio like.
sumaSiUsuarioDioLike :: [Publicacion] -> Usuario -> [Publicacion]
sumaSiUsuarioDioLike [] _ = [] 
sumaSiUsuarioDioLike (pub:pubs) u | pertenece u (likesDePublicacion pub) && not (pertenece pub pubs) = pub : sumaSiUsuarioDioLike pubs u
                                  | otherwise = sumaSiUsuarioDioLike pubs u

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

--8
-- Verifica si las publicaciones que le gustan a un usuario son exactamente las mismas que las que le gustan a otro usuario.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 | publicacionesQueLeGustanA red u1 == publicacionesQueLeGustanA red u2 = True
                                          | otherwise = False 

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

-- 9 
-- Verifica si en todas las publicaciones de un usuario u existe u2 que le haya puesto like a todas.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = seguidorFiel (amigosDe red u) (publicacionesDe red u) 

-- Verifica si en una lista de publicaciones existe algun usuario de una lista de usuarios que le haya dado like a todas.
seguidorFiel :: [Usuario] -> [Publicacion] -> Bool
seguidorFiel _ [] = False
seguidorFiel [] _ = False  
seguidorFiel (u:us) (pub:pubs) | longitud (pub:pubs) == 1 && pertenece u (likesDePublicacion pub) = True
                               | pertenece u (likesDePublicacion pub) = seguidorFiel (u:us) pubs
                               | otherwise = seguidorFiel us (pub:pubs)

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

-- 10
-- Verifica si existe una cadena de amigos en comun posible entre un usuario y otro en la red.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = pertenece u2 (amigosDe red u1 ++ listaDeAmigos red (amigosDe red u1))

-- Devuelve una lista donde concatena recursivamente a los amigos de los amigos del usuario.
listaDeAmigos :: RedSocial -> [Usuario] -> [Usuario]
listaDeAmigos _ [] = []
listaDeAmigos red (u:us) | amigosDe red u == [] = listaDeAmigos red us
                         | otherwise = amigosDe red u ++ listaDeAmigos red us

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

-- # Funciones auxiliares # --

-- Verifica si un elemento pertenece a una lista.
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e s | longitud s == 0 = False
              | e == head s = True
              | otherwise = pertenece e (tail s)

 -- Devuelve la longitud de la lista asociada como entero.
longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Devuelve la cantidad de veces que se repite un elemento en una lista.
cantidadDeApariciones :: (Eq t) => t -> [t] -> Int
cantidadDeApariciones _ [] = 0
cantidadDeApariciones e (x:xs) | e == x = 1 + cantidadDeApariciones e xs
                               | e /= x = cantidadDeApariciones e xs
                               
-- Devuelve la lista ingresada sin el elemento ingresado. 
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos x xs | xs == [] = []
                 | x == head xs = [] ++ quitarTodos x (tail xs)
                 | otherwise = [head xs] ++ quitarTodos x (tail xs)

-- Devuelve una lista con todas las apariciones adicionales de elementos de la lista borrados.
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = [x] ++ quitarTodos x xs
                         | otherwise = [x] ++ eliminarRepetidos xs
