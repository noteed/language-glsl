module Main where

import Text.ParserCombinators.Parsec hiding (State, parse) -- TODO clean
import Text.PrettyPrint.HughesPJClass (prettyShow, Pretty)
import Test.HUnit
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

import Language.GLSL.Syntax
import Language.GLSL.Parser
import Language.GLSL.Pretty ()

main :: IO ()
main = defaultMain . hUnitTestToTests . TestList $ parsingTests

parsingTests :: [Test]
parsingTests =
  [ legalExpressionsTests
  , illegalExpressionsTests
  , legalDeclarationsTests
  , illegalDeclarationsTests
  , legalFunctionDefinitionsTests

  , legalCommentsTests
  , illegalCommentsTests

  , TestLabel "expressions id" $ TestList $
    map expressionsId testExpressionsTrue
  , TestLabel "declarations id" $ TestList $
    map declarationsId testDeclarationsTrue
  , TestLabel "function definitions id" $ TestList $
    map functionDefinitionsId testFunctionDefinitionsTrue

  , sampleFileTest
  ]

----------------------------------------------------------------------
-- For QuickCheck testing
----------------------------------------------------------------------

-- Given an AST ast, pretty-print it and parse it back. The result be
-- equal to the original ast, i.e. (parse . pretty) == id
-- TODO refactor the common parts
-- TODO Befor it is possible to use QC, the AST should enforce more
-- constraints, and some sanity check funtions should be provided
-- (to build only legal ASTs).

parsePrettyId :: TranslationUnit -> Bool
parsePrettyId e = case pass translationUnit (prettyShow e) of
  Left _ -> False
  Right e' -> e == e'

parsePrettyIdExpr :: Expr -> Bool
parsePrettyIdExpr e = case pass expression (prettyShow e) of
  Left _ -> False
  Right e' -> e == e'

parsePrettyIdDecl :: Declaration -> Bool
parsePrettyIdDecl e = case pass declaration (prettyShow e) of
  Left _ -> False
  Right e' -> e == e'

check :: Pretty a => P a -> String -> IO ()
check p str = case pass p str of
  Left err -> print err
  Right ast -> putStrLn $ prettyShow ast

parsePrettyIdFunc :: ExternalDeclaration -> Bool
parsePrettyIdFunc e = case pass functionDefinition (prettyShow e) of
  Left _ -> False
  Right e' -> e == e'

expressionsId :: String -> Test
expressionsId str = TestCase . assertBool ("expressionsId: " ++ str) . parsePrettyIdExpr $ ast
  where ast = case pass expression str of
          Left _ -> error "does not even parse the original string"
          Right a -> a

declarationsId :: String -> Test
declarationsId str = TestCase . assertBool ("declarationsId: " ++ str) . parsePrettyIdDecl $ ast
  where ast = case pass declaration str of
          Left _ -> error "does not even parse the original string"
          Right a -> a

functionDefinitionsId :: String -> Test
functionDefinitionsId str = TestCase . assertBool ("functionDefinitionsId: " ++ str) . parsePrettyIdFunc $ ast
  where ast = case pass functionDefinition str of
          Left _ -> error "does not even parse the original string"
          Right a -> a

----------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------

-- Just check if the parser passes of fails
pass :: P a -> String -> Either ParseError a
pass p = runParser (do {skipMany blank ; r <- p ; eof ; return r}) S "pass"

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

isLeft :: Either a b -> Bool
isLeft = not . isRight

doesParse :: P a -> String -> Test
doesParse p str =
  TestCase . assertBool ("doesParse: " ++ str) . isRight . pass p $ str

doesNotParse :: P a -> String -> Test
doesNotParse p str =
  TestCase . assertBool ("doesNotParse: " ++ str) . isLeft . pass p $ str

----------------------------------------------------------------------
-- expressions
----------------------------------------------------------------------

legalExpressionsTests :: Test
legalExpressionsTests = TestLabel "legal expressions" $
  TestList $ map (doesParse expression) testExpressionsTrue

illegalExpressionsTests :: Test
illegalExpressionsTests = TestLabel "illegal expressions" $
  TestList $ map (doesNotParse expression) testExpressionsFalse

testExpressionsTrue :: [String]
testExpressionsTrue =
  [ "a"
  , "avoid"
  , "filters"
  , "0"
  , "1"
  , ".1"
  , "0x01"
  , "0xF"
  , "07"
  , "a++"
  , "a--"
  , "a++--"
  , "a--++"
  , "a++++"
  , "a----"
  , "++a"
  , "--a"
  , "++--a"
  , "--++a"
  , "++++a"
  , "----a"
  , "a ++"
  , "+a"
  , "+ a"
  , "a + b"
  , "a + b++"
  , "a + ++b"
  , "a + + b"
  , "a ++ +b"
  , "a()"
  , "float()"
  , "a ()"
  , "a( )"
  , "a ( )"
  , "a.a"
  , "a.a()"
  , "a.length()"
  , "a[1]"
  , "a[1].x"
  , "a[1].x()"
  , "a().b"
  , "a().b[1]"
  , "a().b()"
  , "a.b.c"
  ]

testExpressionsFalse :: [String]
testExpressionsFalse =
  [ "void"
  , "filter"
  , ".A"
  , "08"
  , "0A"
  , "+"
  , "++"
  , "a+"
  , "a +"
  , "a . a" -- TODO should it be allowed ?
  , "a[]"
--  , "a[1][2]" -- TODO it is illegal to declare an array of arrays
  ]

----------------------------------------------------------------------
-- declarations
----------------------------------------------------------------------

legalDeclarationsTests :: Test
legalDeclarationsTests = TestLabel "legal declarations" $
  TestList $ map (doesParse declaration) testDeclarationsTrue

illegalDeclarationsTests :: Test
illegalDeclarationsTests = TestLabel "illegal declarations" $
  TestList $ map (doesNotParse declaration) testDeclarationsFalse

testDeclarationsTrue :: [String]
testDeclarationsTrue =
  [ "int a;"
  , "int a, b, c;"
  , "precision highp float;"
  , "int a = 1;"
  , "struct a { int b; };"
  , "struct a { int b; float c; };"
  , "layout (origin_upper_left) in;"
  , "layout (origin_upper_left) in vec4 gl_FragCoord;"
  , "layout (origin_upper_left, pixel_center_integer) in vec4 gl_FragCoord;"
  , "bool success;"
  , "bool done = false;"
  , "int a = 123;"
  , "int a = 123u;"
  , "int a = 123U;"
  , "int a = 0123;"
  , "int a = 0123u;"
  , "int a = 0123U;"
  , "int a = 0x123;"
  , "int a = 0x123u;"
  , "int a = 0x123U;"
  , "int a = 0x12ABu;"
  , "int a = 0x12ABU;"
  , "int a = 0X123;"
  , "int a = 0X123u;"
  , "int a = 0X123U;"
  , "int a = 0X12ABu;"
  , "int a = 0X12ABU;"
  , "float a = 1.0;"
  , "float a = 1.;"
  , "float a = .0;"
  , "float a = 1e1;"
  , "float a = 1.0e1;"
  , "float a = 1.e1;"
  , "float a = 1.0e-1;"
  , "float a = 1.0e+1;"
  , "float a = 1.0e+1f;"
  , "float a = 1.0e+1F;"
  , "vec2 texcoord1, texcoord2;"
  , "mat3x2 m;"
  , "struct light { float intensity; vec3 position; } lightVar;"
  , "const struct light { float intensity; vec3 position; } lightVar;"
  , "float frequencies[3];"
  , "uniform vec4 lightPosition[4];"
  , "light lights[];"
  , "const int numLights = 2;"
  , "light lights[numLights];"
  , "float[5] a;"
  , "float a[5];"
  , "float a[5] = float[](3.4, 4.2, 5.0, 5.2, 1.1);"
  , "float a[5] = float[5](3.4, 4.2, 5.0, 5.2, 1.1);"
  , "const int a;"
  , "in int a;"
  , "centroid in int a;"
  , "smooth in int a;"

  , "bool success;"
  , "bool done = false;"
  , "int i;"
  , "int i, j = 42;"
  , "int j = 1;"
  , "uint k = 3u;"
  , "float x = 1.0;"
  , "float a = 1.5, b;"
  , "vec2 texcoord1, texcoord2;"
  , "vec3 position;"
  , "vec4 myRGBA;"
  , "ivec2 textureLookup;"
  , "bvec3 less;"
  , "mat2 mat2D;"
  , "mat3 optMatrix;"
  , "mat4 view;"
  , "mat3x2 m;"
  , "struct light\
    \{\
    \float intensity;\
    \vec3 position;\
    \} lightVar;"
  , "light lightVar3;"
  , "float frequencies[3];"
  , "uniform vec4 lightPosition[4];"
  , "light lights[];"
  , "const int numLights = 2;"
  , "light lights[numLights];"
  , "int a[5];"
  , "const float coef = 2.75;"
  , "const vec3 zAxis = vec3 (0.0, 0.0, 1.0);"
  , "in vec4 position;"
  , "in vec3 normal;"
  , "in vec2 texCoord[4];"
  , "in float foo[];"
  , "centroid out vec2 TexCoord;"
  , "invariant centroid out vec4 Color;"
  , "invariant flat centroid out vec4 Color;"
  , "noperspective out float temperature;"
  , "flat out vec3 myColor;"
  , "noperspective centroid out vec2 myTexCoord;"
  , "out vec4 FragmentColor;"
  , "out uint Luminosity;"
  , "uniform Transform\
    \{\
    \  mat4 ModelViewMatrix;\
    \  mat4 ModelViewProjectionMatrix;\
    \  uniform mat3 NormalMatrix;\
    \  float Deformation;\
    \};"
  , "in Material\
    \{\
    \  smooth in vec4 Color1;\
    \  smooth vec4 Color2;\
    \  vec2 TexCoord;\
    \};"
  , "out Vertex\
    \{\
    \  vec4 Position;\
    \  vec2 Texture;\
    \} Coords;"
  , "uniform Transform {\
    \  mat4 ModelViewMatrix;\
    \  mat4 ModelViewProjectionMatrix;\
    \  float Deformation;\
    \} transforms[4];"
  , "layout (triangles) in;"
  , "layout (origin_upper_left) in vec4 gl_FragCoord;"
  , "layout (pixel_center_integer) in vec4 gl_FragCoord;"
  , "layout (origin_upper_left, pixel_center_integer) in vec4 gl_FragCoord;"
  , "layout (triangle_strip, max_vertices = 60) out;"
  , "layout (triangle_strip) out;"
  , "layout (max_vertices = 60) out;"
  , "layout (shared, column_major) uniform;"
  , "layout (std140) uniform Transform {\
    \  mat4 M1;\
    \  layout (column_major) mat4 M2;\
    \  mat3 N1;\
    \};"
  , "flat out vec4 gl_FrontColor;"
  , "lowp float color;"
  , "out mediump vec2 P;"
  , "highp mat4 m;"
  , "precision highp float;"
  , "precision highp int;"
  , "precision mediump int;"
  , "invariant gl_Position;"
  , "out vec3 Color;"
  , "invariant Color;"
  , "invariant centroid out vec3 Color;"
  , "vec4 color = vec4(0.0, 1.0, 0.0, 1.0);"
  , "int i = 1 - 5 * 4 + 3;"
  , "int i = 1 - (5 * 4) + 3;"
  , "int i = (1 - 5) * 4 + 3;"
  , "int i = (1 - 5) * (4 + 3);"
  , "bool b = 1 < 2;"

  ]

testDeclarationsFalse :: [String]
testDeclarationsFalse =
  [ "int a"
  , "int a, b c;"
  , "precision high float;"
  , "precision float f();"
  , "int float;"
  , "struct a { };"
  , "struct a { int b, float c; };"
  , "int a = 0128;"
  , "int a = 0xu;"
  , "float a = .e1;"
  , "float a = 1.0e+1G;"
{- TODO embeded structs are not possible.
  , "struct light {\
    \  struct { float intensity; };\
    \  vec3 position;\
    \} lightVar;"
-}
  , "int f();" -- function declaration should be at top level -- TODO put these in legalTranslationUnitsTests
  , "int f(void);"
  , "int f ( void ) ;"
  , "lowp ivec2 foo(lowp mat3);"
  , "float[5] foo();"
  , "void foo (float[5]);"
  , "void foo (float a[5]);"
  , "float[] foo();"
  , "float[5] foo();"
  , "float[5] foo(int[4]);"
  , "int f ();"
-- TODO qualifier only possible where declarators.
--  , "const struct light { float intensity; vec3 position; };"
  , "float a[5][3];"
-- interpolation qualifier may only preced [centroid]in/out.
--  , "smooth const int a;"
  ]

----------------------------------------------------------------------
-- function definitions
----------------------------------------------------------------------

legalFunctionDefinitionsTests :: Test
legalFunctionDefinitionsTests = TestLabel "legal function definition" $
  TestList $ map (doesParse functionDefinition) testFunctionDefinitionsTrue

testFunctionDefinitionsTrue :: [String]
testFunctionDefinitionsTrue =
  [ "void main ()\n\
    \{\n\
    \}"
  , "void main ()\n\
    \{\n\
    \  if (intensity < 0.0)\n\
    \    return;\n\
    \}"
  ]

----------------------------------------------------------------------
-- comments (inside simple declarations)
----------------------------------------------------------------------

legalCommentsTests :: Test
legalCommentsTests = TestLabel "legal comments" $
  TestList $ map (doesParse declaration) testCommentsTrue

illegalCommentsTests :: Test
illegalCommentsTests = TestLabel "illegal comments" $
  TestList $ map (doesNotParse declaration) testCommentsFalse

testCommentsTrue :: [String]
testCommentsTrue =
  [ "int a; // a comment"
  , "int a; /* another comment */"
  , "int a; // a comment\n"
  , "int a; /* another comment */\n"
  , "int a; /* another comment\non multiple\nlines.*/"
  , "int a; /* another comment\non multiple\nlines.*/\n"
  , "int a; /* another comment\non multiple\nlines.\n*/"
  , "/* before */ int a;"
  , "// before\nint a;"
  , "int /* middle */ a;"
  , "int/* middle */a;"
  , "int a/* middle */;"
  , "int a; /* not a // nested comment */"
  ]

testCommentsFalse :: [String]
testCommentsFalse =
  [ "int a; /* no /* nested */  comment */"
  ]

----------------------------------------------------------------------
-- translation unit
----------------------------------------------------------------------

legalTranslationUnitsTests :: Test
legalTranslationUnitsTests = TestLabel "legal translation unit" $
  TestList $ map (doesParse translationUnit) $
  testDeclarationsTrue ++ testFunctionDefinitionsTrue

----------------------------------------------------------------------
-- kitchen sink
----------------------------------------------------------------------

sampleFileTest :: Test
sampleFileTest = TestLabel "Parse/Pretty glsl/sample-01.glsl test" . TestCase . assert $ do
  content <- readFile $ "glsl/sample-01.glsl"
  case parse content of
    Left err -> do
      putStrLn $ "parse error: \n" ++ show err
      return False
    Right ast ->
      return $ parsePrettyId ast
  
