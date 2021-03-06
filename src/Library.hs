module Library where
import PdePreludat

-- Parcial Functional Master Series

-- Nombre: Olmedo, Rodrigo Marcos
-- Legajo: 1719804 

type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String

esVocal :: Char -> Bool
esVocal letra = flip elem "aeiou" letra || flip elem "áéíóú" letra
 
tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

{-
Determinar si dos palabras riman. Es decir, si generan una rima, ya sea asonante o consonante, pero teniendo en cuenta que dos palabras
iguales no se consideran una rima.
-} 

takeLast :: Number -> [a] -> [a]
takeLast cantidad = reverse.(take cantidad).reverse 

esRimaAsonante :: Palabra->Palabra->Bool
esRimaAsonante palabra1 palabra2 = (ultimasNSegun 2 esVocal palabra2)==(ultimasNSegun 2 esVocal palabra1)

ultimasNSegun :: Number->(Char->Bool)->Palabra->Palabra
ultimasNSegun cantidad condicion = (takeLast cantidad).(filter (condicion))

sonIguales :: Palabra -> Palabra -> Bool
sonIguales palabra1 = (==palabra1)

esRimaConsonante :: Palabra->Palabra->Bool
esRimaConsonante palabra1 palabra2 = takeLast 3 palabra1 == takeLast 3 palabra2

esRima :: Palabra -> Palabra -> Bool
esRima palabra1 palabra2 = (esRimaAsonante palabra1 palabra2 || esRimaConsonante palabra1 palabra2) && ((not.(sonIguales palabra1)) palabra2)
--Las clases de equivalencia, dado que lo que nosotros queremos hacer es saber si dos palabras se consideran rima, son 3:
--(palabras que rimen de alguna forma y no sean iguales, palabras que rimen de alguna forma pero sean iguales y palabras que no rimen
--  de ni una forma y sean iguales)

{-
por medio de rimas: dos versos se conjugan con rima cuando logran rimar las últimas palabras de cada uno.
anadiplosis: sucede cuando el segundo verso comienza con la misma palabra con la que termina el primero.
-}
type Conjugacion = Verso -> Verso -> Bool

versosRiman :: Conjugacion
versosRiman verso1 = cumplen ultimaPalabra esRima verso1

esAnadiplosis :: Conjugacion
esAnadiplosis verso1 verso2 = ultimaPalabra verso1 == (head.words) verso2

ultimaPalabra :: Verso->Palabra
ultimaPalabra = head.(takeLast 1).words

{-Patrones
 Los artistas llevan eso a otro nivel al armar patrones en cada estrofa. Un patrón es una forma de articular los versos dentro de una 
 estrofa

Simple: es un patrón en el que riman 2 versos, el 1 y 4, pero no el 1 y 3
-}

type Patron = Estrofa->Bool

simple :: Number->Number->Patron
simple indice indice2 estrofa = (versosRiman (versoN indice estrofa) (versoN indice2 estrofa)) && (demasVersosNoRiman (versoN indice estrofa) (versoN indice2 estrofa) estrofa)

demasVersosNoRiman :: Verso -> Verso -> Estrofa -> Bool
demasVersosNoRiman verso1 verso2 = not.(versosRiman verso1).head.(sacarVersos verso2).(sacarVersos verso1)

demasVersos :: Number -> Number -> Estrofa -> Estrofa
demasVersos indice indce2 estrofa = (...)

sacarVersos :: Verso -> Estrofa -> Estrofa
sacarVersos verso = filter (not.(tieneAlVerso verso)) 

tieneAlVerso :: Verso -> Verso-> Bool
tieneAlVerso verso verso2 = verso == verso2

versoN :: Number-> [Verso]->Verso
versoN numero = flip (!!) (numero-1) 
{-
Esdrújulas: Todos los versos terminan con palabras en esdrújula. Diremos que una palabra es esdrújula cuando la antepenúltima vocal
está acentuada. 
-}
esdrujulas :: Patron
esdrujulas = todasLasBarrasCumplen terminaEsdrujula

{-
esdrujulas :: Patron
esdrujulas = todasLasBarrasCumplen rimanConEsdrujula

contarRimasVersoSegun :: Number->Conjugacion->[Verso]->Number
contarRimasVersoSegun _ _ [] = 0
contarRimasVersoSegun contador condicion (x:xs) | foldl1 (meQuedoConVersoSiSegun condicion) (x:xs)==x = (max (4-contador) 0)
                                                | otherwise = contarRimasVersoSegun (contador+1) condicion xs

meQuedoConVersoSiSegun :: (Conjugacion)->Verso->Verso->Verso
meQuedoConVersoSiSegun condicion verso1 verso2 | condicion verso1 verso2 = verso1
                                               | otherwise = verso2 

rimanConEsdrujula :: Conjugacion
rimanConEsdrujula verso1 verso2 = terminaEsdrujula verso1 && terminaEsdrujula verso2
-}

terminaEsdrujula :: Verso -> Bool
terminaEsdrujula = esEsdrujula.ultimaPalabra

esEsdrujula :: Palabra->Bool
esEsdrujula = tieneTilde.head.(ultimasNSegun 3 esVocal)

{-
Anáfora: Todos los versos comienzan con la misma palabra. -}

anafora :: Patron
anafora estrofa = todasLasBarrasCumplen (empiezanIgual (primeraPalabraDelVerso estrofa)) estrofa

primeraPalabraDelVerso :: Estrofa -> String
primeraPalabraDelVerso = head.words.head

empiezanIgual :: Verso -> Verso -> Bool
empiezanIgual verso1 verso2 = (head.words) verso1 == (head.words) verso2 

todasLasBarrasCumplen :: (Verso->Bool)->Estrofa->Bool
--todasLasBarrasCumplen condicion = (==4).(contarRimasVersoSegun 0 condicion)
todasLasBarrasCumplen condicion estrofa = (length.(filter condicion)) estrofa == length estrofa


{-
Cadena: Es un patrón que se crea al conjugar cada verso con el siguiente, usando siempre la misma conjugación.
La conjugación usada es elegida por el artista mientras está rapeando. Por ejemplo, una cadena de anadiplosis sería:
-}
cadena :: Conjugacion -> Patron
cadena conjugacion estrofa = foldl1 (funcionLoca conjugacion) estrofa == versoN 4 estrofa

funcionLoca :: Conjugacion ->Verso -> Verso-> Verso
funcionLoca conjugacion verso1 verso2 | conjugacion verso1 verso2 =verso2
                                      | otherwise = verso1

{-
CombinaDos: Dos patrones cualesquiera se pueden combinar para crear un patrón más complejo, y decimos que una estrofa lo cumple cuando 
cumple ambos patrones a la vez. 
-}

combinaDos :: Patron->Patron->Patron
combinaDos patron1 patron2 estrofa =  patron1 estrofa && patron2 estrofa

{-
aabb = patrón simple entre 1 y 2 + otro simple entre 3 y 4
abab = patrón simple entre 1 y 3 + otro simple entre 2 y 4
abba = patrón simple entre 1 y 4 + otro simple entre 2 y 3
hardcore = patrón de cadena de rimas + esdrújulas

¿Se podría saber si una estrofa con infinitos versos cumple con el patrón hardcore? ¿Y el aabb? Justifique en cada 
caso específicamente por qué (no valen respuestas genéricas).
-}

aabb :: Patron
aabb = combinaDos (simple 1 2) (simple 3 4) 

abab :: Patron
abab = combinaDos (simple 1 3) (simple 2 4)

abba :: Patron
abba = combinaDos (simple 1 4) (simple 2 3)

hardcore :: Conjugacion-> Patron 
hardcore conjugacion = combinaDos (cadena conjugacion) esdrujulas


--No, con el patron hardcore no se podria evaluar, dado que cadena y esdrujulas trabajan con que toda los versos de la estrofa cumplan con la 
-- condicion.
--En cambio, con el patron aabb si, dado que solo evalua 4 versos de la estrofa

{-
Puestas en escena

Gritar: aumenta la potencia en un 50%

Responder un acote: conociendo su efectividad, aumenta la potencia en un 20%, y además el público queda exaltado si la respuesta fue efectiva,
sino no lo queda.

Tirar técnicas: se refiere a cuando el artista deja en evidencia que puede lograr algún patrón en particular,
aumenta la potencia en un 10%, además el público se exalta si la estrofa cumple con dicho patrón, sino no.
-}

data PuestaEnEscena = UnaPuesta {
    publicoExaltado :: Bool,
    potencia :: Number,
    estrofa :: Estrofa,
    artista :: Artista
}deriving(Show)

type Estilo = PuestaEnEscena -> PuestaEnEscena

gritar :: Estilo
gritar = aumentarPotencia 50

responderAcote :: Bool -> Estilo
responderAcote efectividad = (modificarEstadoPublico efectividad).(aumentarPotencia 20)

aumentarPotencia :: Number -> PuestaEnEscena->PuestaEnEscena
aumentarPotencia porcentaje puesta = puesta{potencia= potencia puesta + (potencia puesta)*(porcentaje/100)}

modificarEstadoPublico :: Bool-> PuestaEnEscena -> PuestaEnEscena
modificarEstadoPublico estado puesta = puesta{publicoExaltado = estado}

tirarTecincas :: Patron -> Estilo
tirarTecincas patron puesta = ((modificarEstadoPublico (patron (estrofa puesta))).(aumentarPotencia 10)) puesta

{-
Hacer que un artista se tire un freestyle a partir de la estrofa que quiere decir y el estilo que le quiera dar a su puesta en escena. 
Para ello se parte siempre de una puesta base que tiene potencia 1 y el público tranquilo, la que luego varía según el estilo utilizado.
El resultado de que un artista se tire un freestyle es una puesta en escena.
-}

puestaBase :: Artista->PuestaEnEscena
puestaBase nombre = UnaPuesta {potencia = 1, publicoExaltado = False, estrofa=[], artista = nombre}

tirarFreestyle :: Estrofa->Estilo->Artista->PuestaEnEscena
tirarFreestyle  versos estilo = estilo.(establecerEstrofa versos).puestaBase

establecerEstrofa :: Estrofa ->PuestaEnEscena-> PuestaEnEscena
establecerEstrofa versos puesta = puesta{estrofa = versos}

{-

Jurados
Nos contaron que cada jurado define sus propios criterios para puntuar una puesta en escena, estos criterios se basan en alguna condición que 
debe cumplir la puesta y un puntaje que se le otorga si la cumple. Para que nos demos una idea, nos comentaron algunos ejemplos de estos 
criterios: que el freestyle cumpla con un patrón determinado, si el público está exaltado, si la potencia cumple algún requisito, etc.
Es importante entender que cada jurado define sus propias condiciones y puntajes para armar los criterios que le parecen decisivos.

Se pide
Definir el jurado alToke con los siguientes criterios:
- Si el freestyle cumple con el patrón aabb, entonces suma 0.5 punto
- Si el freestyle cumple con el patrón combinado de esdrújulas y simple entre 1 y 4, entonces suma 1 punto
- Si el público está exaltado, entonces suma 1 punto
- Si tiene una potencia mayor a 1.5, entonces suma 2 puntos
Calcular el puntaje que le daría un jurado a una puesta en escena, considerando que es la suma de los puntajes de todos los criterios que 
cumpla la puesta, teniendo como máximo 3 puntos en total.
-}
type Jurado = [PuestaEnEscena->Number]
type CriterioJurado = PuestaEnEscena->Bool

alToke :: Jurado
alToke = [cumpleCriterio (cumplePatron aabb) 0.5, cumpleCriterio (cumplePatron (combinaDos esdrujulas (simple 1 4))) 1, cumpleCriterio (potenciaSuficiente 1.5) 2, cumpleCriterio publicoExaltado 1]

puntaje :: PuestaEnEscena->Jurado->Number
puntaje puesta = (min 3).sum.(cumplirCriterios [] puesta)

cumpleCriterio :: CriterioJurado->Number -> PuestaEnEscena->Number
cumpleCriterio condicion puntaje puesta  | condicion puesta = puntaje
                                         | otherwise = 0 

cumplirCriterios :: [Number]-> PuestaEnEscena ->[PuestaEnEscena->Number]->[Number]
cumplirCriterios listaPuntajes puesta [] = listaPuntajes
cumplirCriterios listaPuntajes puesta (x:xs)  = cumplirCriterios (x puesta : listaPuntajes) puesta xs

potenciaSuficiente :: Number-> CriterioJurado
potenciaSuficiente potSuf puesta = potencia puesta>potSuf

cumplePatron :: Patron->CriterioJurado
cumplePatron patron = patron.estrofa
{-
{-
BONUS: 3, 2, 1… ¡Tiempo! (este punto es opcional)
Por último, llega el momento de las batallas.
Una batalla se da siempre entre dos artistas. Cada artista deberá presentar diferentes puestas en escenas y la batalla consistirá de todas 
esas puestas en escena. El primer artista comenzará haciendo una puesta en escena, luego el segundo hará la suya, y de esta manera se irán 
turnando (de forma indefinida) hasta que la batalla termine.
Al final, el artista ganador es quien haya sumado más puntos por parte del conjunto de jurados de la batalla y es quien se lleva el cinto a 
la casa.

A partir de una batalla y un conjunto de jurados, saber qué artista se lleva el cinto a la casa.
-}
-}  

ganadorBatalla :: [PuestaEnEscena]->[Jurado]->Artista
ganadorBatalla puestas jurados | puntajeArtistaN 1 0 puestas jurados> puntajeArtistaN 2 0 puestas jurados = artistaNBatalla 1 puestas
                               | otherwise = artistaNBatalla 2 puestas

puntajeArtistaN :: Number -> Number -> [PuestaEnEscena] -> [Jurado] -> Number
puntajeArtistaN _ puntajeTotal _ [] = puntajeTotal
puntajeArtistaN nroArtista puntajeInicial puestas (x:xs) = puntajeArtistaN nroArtista (sum(map (flip puntaje x) (puestasArtistaN nroArtista puestas))) puestas xs

puestasArtistaN :: Number -> [PuestaEnEscena] -> [PuestaEnEscena]
puestasArtistaN nroCompetidor puestas = filter (esPuestaArtistaN (artistaNBatalla nroCompetidor puestas)) puestas

artistaNBatalla :: Number -> [PuestaEnEscena]->Artista
artistaNBatalla nroCompetidor = artista.(puestaN nroCompetidor)

puestaN :: Number-> [PuestaEnEscena]->PuestaEnEscena
puestaN numero = flip (!!) (numero-1)

esPuestaArtistaN :: Artista->PuestaEnEscena -> Bool
esPuestaArtistaN artista1= (==artista1).artista

