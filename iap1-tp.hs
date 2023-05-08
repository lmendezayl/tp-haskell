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

sumarUsuariosDistintos :: Usuario -> [Relacion] -> [Usuario]
sumarUsuariosDistintos u [] = []
sumarUsuariosDistintos u ((u1,u2):xs) | u == u1 && estaUsuarioEnLista u xs = [u2] ++ sumarUsuariosDistintos u xs
                                      | u == u2 && estaUsuarioEnLista u xs= [u1] ++ sumarUsuariosDistintos u xs
                                      | otherwise = sumarUsuariosDistintos u xs


estaUsuarioEnLista :: Usuario -> [Relacion] -> Bool
estaUsuarioEnLista _ [] = False
estaUsuarioEnLista u ((u1,u2):xs) | u == u1 || u == u2 = True
                                  | otherwise = estaUsuarioEnLista u xs




-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos = undefined

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

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
