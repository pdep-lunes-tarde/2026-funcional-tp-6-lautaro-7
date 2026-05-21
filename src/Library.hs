module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | PatiVegano | Papas | BaconDeTofu | PanIntegral
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente BaconDeTofu = 12
precioIngrediente PanIntegral = 3

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

precioFinal :: Hamburguesa -> Number
precioFinal hamburguesa = precioBase hamburguesa + (sum . map  precioIngrediente . ingredientes) hamburguesa


agregarIngrediente:: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente nuevoIngrediente hamburguesa = hamburguesa { ingredientes = nuevoIngrediente : ingredientes hamburguesa}

getIngrediente :: Hamburguesa -> Ingrediente
getIngrediente hamburguesa = head . filter esIngredienteBase . ingredientes $ hamburguesa
esIngredienteBase ingrediente = elem ingrediente [PatiVegano, Carne, Pollo]

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa = agregarIngrediente ( getIngrediente hamburguesa ) hamburguesa

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje hamburguesa = hamburguesa { precioBase = precioBase hamburguesa * (1 - porcentaje /100) }


delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = agregarIngrediente Papas . descuento 30 $ hamburguesa

cambiarIngredientes :: Ingrediente -> Ingrediente
cambiarIngredientes Carne = PatiVegano
cambiarIngredientes Pollo = PatiVegano
cambiarIngredientes Cheddar = QuesoDeAlmendras
cambiarIngredientes Panceta = BaconDeTofu
cambiarIngredientes ingrediente = ingrediente

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa = hamburguesa { ingredientes = map cambiarIngredientes . ingredientes $ hamburguesa }

cambiarPan :: Ingrediente -> Ingrediente
cambiarPan Pan = PanIntegral
cambiarPan ingrediente = ingrediente

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa = hamburguesa { ingredientes = map cambiarPan . ingredientes $ hamburguesa }
