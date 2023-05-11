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

--La funcion sumarSegundosDistintos suma los segundos elementos de las tuplas del type Usuario siempre y cuando sean distintos 
sumarSegundosDistintos :: [Usuario] -> [String]
sumarSegundosDistintos [] = []
sumarSegundosDistintos (x:xs) | estaEnLista (snd x) xs = sumarSegundosDistintos xs 
                              | otherwise = (snd x) : sumarSegundosDistintos xs 

estaEnLista :: String -> [Usuario] -> Bool
estaEnLista _ [] = False
estaEnLista y (x:xs) = y == snd x || estaEnLista y xs

--2
--La funcion amigosDe devuelve los usuarios relacionados con el usuario ingresado, ademas asegura que no haya res repetidos.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u  = sumarUsuariosDistintos u (relaciones red)  

--La funcion sumarUsuariosDistintos concatena los usuarios que no esten repetidos.
sumarUsuariosDistintos :: Usuario -> [Relacion] -> [Usuario]
sumarUsuariosDistintos u [] = []
sumarUsuariosDistintos u ((u1,u2):xs) | u == u1 && not(pertenece (u1, u2) xs) = [u2] ++ sumarUsuariosDistintos u xs
                                      | u == u2 && not(pertenece (u1, u2) xs) = [u1] ++ sumarUsuariosDistintos u xs
                                      | otherwise = sumarUsuariosDistintos u xs



--3
-- La funcion cantidadDeAmigos devuelve la longitud de la lista amigosDe
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u)



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
-- Devuelve la lista de todas las publicaciones subidas por un usuario de la red
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = sumaPublisDistintas (publicaciones red) u 

sumaPublisDistintas :: [Publicacion] -> Usuario -> [Publicacion]
sumaPublisDistintas [] u = []
sumaPublisDistintas (x:xs) u = if  (laPublicacionEsDeU x u && not(pertenece x xs))
                                            then x : sumaPublisDistintas xs u   
                                                 else sumaPublisDistintas xs u

laPublicacionEsDeU :: Publicacion -> Usuario -> Bool
laPublicacionEsDeU (u1,str,l) u | u1 == u = True
                                | otherwise = False



--7
-- La funcion devuelve la lista de publicaciones que le gustan a un usuario de la red
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = sumaSiUDioLike (publicaciones red) u


sumaSiUDioLike :: [Publicacion] -> Usuario -> [Publicacion]
sumaSiUDioLike l u | l == [] = []
                   | uDioLike u (head l) && not (pertenece (head l) (tail l)) = head l :  sumaSiUDioLike (tail l) u
                   | otherwise = sumaSiUDioLike (tail l) u


uDioLike :: Usuario -> Publicacion -> Bool
uDioLike u p | pertenece u (likesDePublicacion p) = True
             | otherwise = False


--8
-- La funcion devuelve un booleano dependiendo de si las publicaciones que le gustan a un usuario son exactamente las mismas que las que le gustan a otro usuario
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = if publicacionesQueLeGustanA red u1 == publicacionesQueLeGustanA red u2 
                                               then True
                                                    else False 

--9
-- La funcion devuelve un booleano dependiendo de si todas para todas las publicaciones de un usuario u existe u2 que le haya puesto like
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = undefined


-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined


-- Funciones auxiliares
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e s | longitud s == 0 = False
              | e == head s = True
              | otherwise = pertenece e (tail s)

 -- Funcion auxiliar "longitud" devuelve la longitud de la lista asociada como entero.
longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
