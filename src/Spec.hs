module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

cuartoDeLibra :: Hamburguesa
cuartoDeLibra =
    Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

pdepBurger :: Hamburguesa
pdepBurger =
    descuento 20 . agregarIngrediente Cheddar . agregarIngrediente Panceta . agrandar . agrandar $ cuartoDeLibra

dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Cheddar . agregarIngrediente Carne $ cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 5" $ do
        it "la pdepBurger tiene precio final 110" $ do
            precioFinal pdepBurger `shouldBe` 110

        it "el dobleCuarto tiene precio final 84" $ do
            precioFinal dobleCuarto `shouldBe` 84

        it "el bigPdep tiene precio final 89" $ do
            precioFinal bigPdep `shouldBe` 89

        it "dobleCuartoVegano es un dobleCuarto veggie con pan integral." $ do
            (agregarIngrediente PanIntegral . hacerVeggie $ dobleCuarto) `shouldBe` Hamburguesa { precioBase = 20, ingredientes = [PanIntegral, QuesoDeAlmendras, PatiVegano, Pan, PatiVegano, QuesoDeAlmendras, Pan]}
