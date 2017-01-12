
-- TODO: move the general stuff to higher modules

module Chemistry.Physical.Thermodynamics exposing (..)


-- UNITS ----------------------------------------------------------------------

-- TODO: more prefixes
type Prefix = Milli
            | Centi
            | Self
            | Mega
            | Kilo

-- TODO: more units
type Unit = Mole
          | Unit_ Prefix Gram
          | Unit_ Prefix Newton
          | Unit_ Prefix Meter
          | Unit_ Prefix Pascal
          | Square_  (     Unit)
          | Cubic_   (     Unit)
          | Per_     (     Unit)
          | Product_ (List Unit)

type Value = Value_ Float Unit
           | Undefined

type Symbol x = Value
              | x
              | Sum        Symbol Symbol
              | Difference Symbol Symbol
              | Product    Symbol Symbol
              | Dividend   Symbol Symbol

type Square x  = Square_ x
type Cubic  x  = Cubic_  x

type Per x y   = Per_ x y

multiply : Value -> Value -> Value

divide : Value -> Value -> Value


-- AMOUNT ---------------------------------------------------------------------

type Amount = Mole_  Float
            | Mole__ Pressure Volume Temp

avogadroConstant : Mole
avogadroConstant = Mole_ <| 6.02 * (10^23)

-- C_12 has an atomic weight of 12, by definition
-- 1 amu is 1/12 the mass of a C_12 atom
-- there are 6.02*10^23 atoms in 12g of C_12
mole : Float -> Mole
mole = Mole_ << (*) <| toFloat avogadroConstant

type Molar x = Molar_ Mole x

-- type alias MolarMass = Per Mass Amount

molarMass : { mass : Mass, amount : Amount } -> Molar Mass
molarMass { mass, amount } = Molar_ amount mass


-- FORCE ----------------------------------------------------------------------

type Mass = Mass_       (Molar Mass) Amount  --- e.g. g/mol*mol
          | Gram_       Float                --- g
          | AtomicMass_ Float                --- amu

massToValue : Mass -> Value


-- FORCE ----------------------------------------------------------------------

type Force = Netwon_ Float

forceToValue : Force -> Value


-- PRESSURE -------------------------------------------------------------------

type Pressure = Pascal_  Pascal
              | Pascal__ Per Newton (Square Meter) 

-- P = F/A
-- 1Pa = 1 N/(m^2)
-- pressure is the magnitude of the perpendicular force per unit area exerted by the system on its surroundings
pascal : Per Newton (Square Meter) -> Pascal
pascal (Per_ (Newton_ n) (Square_ (Meter_ mm)))
  = Pascal_ <| n / mm

-- one torr (or 1 mmHg) is the pressure exerted at 0C by a column of Hg one mm high with gravitation acceleration
-- 1 bar =~ 750 torr
-- 1 bar = 10^5 Pa
-- TODO: ?

pressureToValue : Temperature -> Value


-- TEMPERATURE ----------------------------------------------------------------

type Temperature = Kelvin_     Float
                 | Centigrade_ Float

waterTriplePoint : Temperature
waterTriplelPoint = Kelvin_ 273.16

-- centigrade : Kelvin -> Centigrade
-- centigrade = Centigrade_ << flip (-) 273.15 << toFloat

temperatureToValue : Temperature -> Value


-- LENGTH ---------------------------------------------------------------------

type Length = Meter_ Float
            | Centimeter_ Float

type alias Area = Square Length

type Volume = Volume_  (Cubic Length)               --- cm^3
            | Volume__ Pressure Amount Temperature  --- V = nRT/P
            | Liter_   Float

lengthToValue : Length -> Value


-- DENSITY --------------------------------------------------------------------

type Density = Density_  Mass Volume                        --- rho = m/V
             | Density__ (Molar Mass) Pressure Temperature  --- rho = (MP)/(RT)

densityToValue : Density -> Value


-- GAS CONSTANT ---------------------------------------------------------------

-- TODO: R (gas constant)
-- R = 82.06 (cm^3 atm)/(mol K)
-- R = 8.3145 J/(mol K) = 8.314 (m^3 Pa)/(mol K)
-- R = 1.987 cal/(mol K)


-- IDEAL GAS ------------------------------------------------------------------

-- TODO: use setters and getters that hold to the General Ideal-Gas Equation
-- TODO: create union type with all valid permutations?
-- PV = nRT
type alias IdealGas 
  = { pressure : Maybe Pressure
    , volume   : Maybe (Cubic Space)
    , amount   : Maybe Amount
    , temp     : Maybe Temperature
    }

getDensity : IdealGas -> Element -> Result String Density
getDensity { pressure, volume, amount, temp } e
  = case ( pressure, volume, amount, temp ) of
      ( Just pressure_,            _,            _, Just temp_ ) -> Ok <| Density__ 
      _                                                          -> Err "could not get amount"

getAmount : IdealGas -> Result Err Amount
getAmount { pressure, volume, amount, temp } e
  = case ( pressure, volume, amount, temp ) of
      (              _,            _, Just amount_,          _ ) -> Ok amount_
      ( Just pressure_, Just volume_,            _, Just temp_ ) -> Ok <| Mole__ pressure_ volume_ temp_
      _                                                          -> Err "could not get amount"

getMass : IdealGas -> Element -> Result String Mass
getMass { pressure, volume, amount, temp } e
  = case ( pressure, volume, amount, temp ) of
      (              _,            _, Just amount_,          _ ) -> Ok <| Mass_ (Element.getMolarMass e) amount_
      _                                                          -> Err "could not get mass"

getVolume : IdealGas -> Result Err Volume
getVolume { pressure, volume, amount, temp }
  = case ( pressure, volume, amount, temp ) of
      (              _, Just volume_,            _,          _ ) -> Ok    volume_
      ( Just pressure_,            _, Just amount_, Just temp_ ) -> Ok <| Volume__ pressure_ amount_ temp_
      _                                                          -> Err "could not get volume"


