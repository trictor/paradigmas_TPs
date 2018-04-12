{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions -- Para mostrar <Function> en consola cada vez que devuelven una
import Data.List -- Para métodos de colecciones que no vienen por defecto (ver guía de lenguajes)
import Data.Maybe -- Por si llegan a usar un método de colección que devuelva “Just suElemento” o “Nothing”.
import Test.Hspec


------------TEST---------------------------------

usuarioCon10Monedas = Usuario "Pepe" 10 10 
  

ejecutarTest = hspec $ do

  describe "Pruebas sobre los eventos" $ do

    it "Depositar 10 más. Debería quedar con 20 monedas." $ (billetera.deposita 10) usuarioCon10Monedas  `shouldBe` 20 
    it "Extraer 3: Debería quedar con 7" $ (billetera.extraccion 3) usuarioCon10Monedas `shouldBe` 7
    it "Extraer 15: Debería quedar con 0" $ (billetera .extraccion 15) usuarioCon10Monedas `shouldBe` 0
    it "Un upgrade: Debería quedar con 12." $  (billetera.upgrade) usuarioCon10Monedas `shouldBe` 12
    it "Cerrar la cuenta: 0." $ billetera (cierraCuenta usuarioCon10Monedas) `shouldBe` 0
    it "Queda igual: 10." $ (billetera.quedaIgual) usuarioCon10Monedas `shouldBe` 10
    it "Depositar 1000, y luego tener un upgrade: 1020." $ (billetera.upgrade.deposita 1000) usuarioCon10Monedas `shouldBe` 1020
    it "Toco y me voy, deberia quedar con 0." $ (billetera.tocoMeVoy) usuarioCon10Monedas `shouldBe` 0
    it "ahorrante errante, deberia quedar con 34." $ (billetera.ahorranteErrante) usuarioCon10Monedas `shouldBe` 34

  describe "Pruebas sobre las transiciones" $ do
    
    it "Lucho toca y se va con billetera de 10 monedas, deberia quedar 0 monedas." $ (billetera.luchoTocaYSeVa lucho) usuarioCon10Monedas `shouldBe` 0
    it "Lucho es ahorrante errante con billetera de 10 monedas, deberia quedar 34 monedas." $ (billetera.luchoEsUnAhorranteErrante lucho) usuarioCon10Monedas `shouldBe` 34
    it "pepe le paga 7 monedas a lucho, deberia quedarle 3 monedas" $ (billetera.pepeLeDa7UnidadesALucho pepe) usuarioCon10Monedas `shouldBe` 3
    it "lucho recibe 7 monedas de pepe, deberia quedarle 17 monedas" $ (billetera.pepeLeDa7UnidadesALucho lucho) usuarioCon10Monedas `shouldBe` 17
    it "lucho cierra la cuenta aplicada a pepe, deberia quedar pepe sin cambios" $ luchoCierraLaCuenta pepe pepe `shouldBe` pepe
    it "pepe le da 7 unidades a Lucho, deberia quedar lucho con 9 monedas" $ pepeLeDa7UnidadesALucho lucho lucho `shouldBe` nuevaBilletera 9 lucho
    it "pepe le da 7 unidades a Lucho y despues pepe deposita 5 monedas, deberia quedar con 9 monedas" $ (pepeLeDa7UnidadesALucho pepe.pepeDeposita5Monedas pepe) pepe `shouldBe` nuevaBilletera 8 pepe

-----------TIPOS DE DATOS-----------------

type Evento = Usuario -> Usuario
type Transaccion = Evento
type Pago = Evento
type Monedas = Float
type Nombre = String
type Nivel = Int 

data Usuario = Usuario {
  nombre :: Nombre,
  billetera :: Monedas,
  nivel :: Nivel
} deriving (Show,Eq) 

--------------USUARIOS-------------------

pepe = Usuario "Jose" 10 0
lucho = Usuario "luciano" 2 0

-----------EVENTOS-------------------------
quedaIgual usuario = usuario
nuevaBilletera unMonto unUsuario = unUsuario {billetera = unMonto}
cierraCuenta usuario = nuevaBilletera 0 usuario
subeNivel usuario = usuario {nivel = nivel usuario + 1 , billetera = (billetera usuario) * 1.2}
deposita unMonto usuario = nuevaBilletera ((billetera usuario) +  unMonto) usuario
tocoMeVoy usuario = (cierraCuenta.upgrade.deposita 15) usuario
ahorranteErrante usuario = (deposita 10 .upgrade .deposita 8 .extraccion 1 .deposita 2 .deposita 1) usuario


extraccion unMonto usuario
  | (billetera usuario) - unMonto < 0 = nuevaBilletera 0 usuario
  | otherwise = nuevaBilletera ((billetera usuario) - unMonto) usuario

upgrade usuario
  | (billetera.subeNivel) usuario - billetera usuario < 10 = subeNivel usuario
  | otherwise = nuevaBilletera (billetera usuario + 10) usuario

------------------TRANSACCIONES------------------

luchoCierraLaCuenta :: Usuario -> Transaccion
luchoCierraLaCuenta unUsuario | compararUsuarios unUsuario lucho = cierraCuenta 
                              | otherwise = quedaIgual

pepeDeposita5Monedas :: Usuario -> Transaccion
pepeDeposita5Monedas unUsuario | compararUsuarios unUsuario pepe = deposita 5 
                               | otherwise = quedaIgual  

luchoTocaYSeVa :: Usuario -> Transaccion
luchoTocaYSeVa unUsuario | compararUsuarios unUsuario lucho = tocoMeVoy 
                         | otherwise = quedaIgual  

luchoEsUnAhorranteErrante :: Usuario -> Transaccion
luchoEsUnAhorranteErrante unUsuario | compararUsuarios unUsuario lucho = ahorranteErrante 
                                    | otherwise = quedaIgual  



-----------------------PAGOS----------------------

pepeLeDa7UnidadesALucho :: Usuario -> Pago
pepeLeDa7UnidadesALucho unUsuario | compararUsuarios unUsuario pepe = extraccion 7 
                                  | compararUsuarios unUsuario lucho = deposita 7
                                  | otherwise = quedaIgual 


-----------------------BLOQUES----------------------------------

bloque1 unUsuario =  (luchoCierraLaCuenta unUsuario
                      .pepeDeposita5Monedas unUsuario
                      .pepeDeposita5Monedas unUsuario
                      .pepeDeposita5Monedas unUsuario
                      .luchoTocaYSeVa unUsuario
                      .luchoEsUnAhorranteErrante unUsuario
                      .pepeLeDa7UnidadesALucho unUsuario
                      .luchoTocaYSeVa unUsuario) unUsuario
 
-------------FUNCIONES AUXILIARES-----------------

compararUsuarios usuario1 usuario2 = nombre usuario1 == nombre usuario2






