{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions -- Para mostrar <Function> en consola cada vez que devuelven una
import Data.List -- Para métodos de colecciones que no vienen por defecto (ver guía de lenguajes)
import Data.Maybe -- Por si llegan a usar un método de colección que devuelva “Just suElemento” o “Nothing”.




data Usuario = Usuario {
  nombre :: String,
  billetera :: Float,
  nivel :: Int
} deriving (Show)

quedaIgual :: Usuario -> Usuario
quedaIgual usuario = usuario
nuevaBilletera unMonto unUsuario = unUsuario {billetera = unMonto}
cierraCuenta usuario = nuevaBilletera 0 usuario
subeNivel usuario = usuario {nivel = nivel usuario + 1 , billetera = (billetera usuario) * 1.2}
deposita unMonto usuario = nuevaBilletera ((billetera usuario) +  unMonto) usuario

extraccion unMonto usuario
  | (billetera usuario) - unMonto < 0 = nuevaBilletera 0 usuario
  | otherwise = nuevaBilletera ((billetera usuario) - unMonto) usuario

upgrade usuario
  | (nivel usuario) < 10 = subeNivel usuario
  | otherwise = quedaIgual usuario


tocoMeVoy usuario = (cierraCuenta.upgrade.(deposita 15)) usuario
ahorranteErrante usuario = (deposita 10 .upgrade .deposita 8 .extraccion 1 .deposita 2 .deposita 1) usuario