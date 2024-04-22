import Test.HUnit
import Dibujo 


dibujo = figura ()

testRotar :: Test
testRotar = TestCase $ do
    let expected = dibujo 
        actual = rotar $ rotar $ rotar $ rotar $ dibujo
    assertEqual "rotar" expected actual

testRotar180 :: Test
testRotar180 = TestCase $ do
    let expected = dibujo
        actual = r180 $ r180 $ dibujo
    assertEqual "rotar 180" expected actual

testRotar270 :: Test
testRotar270 = TestCase $ do
    let expected = dibujo
        actual = r270 $ r270 $ r270 $ r270 $ dibujo
    assertEqual "rotar 270" expected actual

testEspejarDoble :: Test
testEspejarDoble = TestCase $ do
    let expected = dibujo
        actual = espejar $ espejar $ dibujo
    assertEqual "espejar doble" expected actual

testApilar :: Test
testApilar = TestCase $ do
    let expected = apilar 1 1 dibujo dibujo
        actual = dibujo .-. dibujo
    assertEqual "apilar" expected actual


testJuntar :: Test
testJuntar = TestCase $ do
    let expected = juntar 1 1 dibujo dibujo
        actual = dibujo /// dibujo
    assertEqual "juntar" expected actual

testEncimar :: Test
testEncimar = TestCase $ do
    let expected = encimar dibujo dibujo
        actual = dibujo ^^^ dibujo
    assertEqual "encimar" expected actual


{- testMapDib :: Test
testMapDib = TestCase $ do
    let expected = encimar (espejar dibujo) (espejar dibujo)
        actual   = mapDib (espejar) (encimar dibujo dibujo)
    assertEqual "mapDib" expected actual -}
 
testMapDib :: Test
testMapDib = TestCase (do
    let dibujo = apilar 1 1 (figura 1) (figura 2) -- Dibujo original
    let expected = apilar 1 1 (figura 2) (figura 4) -- Dibujo esperado después de aplicar mapDib
    assertEqual "mapDib (*2) dibujo" expected (mapDib (*2) dibujo))



tests :: Test
tests = TestList
    [ TestLabel "Test rotar" testRotar
    , TestLabel "Test rotar 180" testRotar180
    , TestLabel "Test rotar 270" testRotar270
    , TestLabel "Test espejar doble" testEspejarDoble
    , TestLabel "Test apilar" testApilar
    , TestLabel "Test juntar" testJuntar
    , TestLabel "Test encimar" testEncimar
    , TestLabel "Test mapDib" testMapDib
    ]

main :: IO ()
main = do
    counts <- runTestTT tests
    putStrLn $ show counts
