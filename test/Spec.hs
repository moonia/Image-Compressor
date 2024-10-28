import Test.HUnit
import GetOpts

testValidArgs :: Test
testValidArgs = TestCase (assertEqual "test valid args"
                    (Just (Conf (Just 2) (Just 0.8) (Just "in")))
                    (getOpts (Just defaultConf)
                    ["-n", "2", "-l", "0.8", "-f", "in"]))

testCheckNbColorsInt :: Test
testCheckNbColorsInt = TestCase (assertEqual "test is nb colors an int"
                    (Nothing)
                    (getOpts (Just defaultConf)
                    ["-n", "2.3", "-l", "0.8", "-f", "in"])) 

testCheckNbColorsPositive :: Test
testCheckNbColorsPositive = TestCase (assertEqual "test is nb colors positive"
                    (Nothing)
                    (getOpts (Just defaultConf)
                    ["-n", "-2", "-l", "0.8", "-f", "in"]))

testCheckLimitPositive :: Test
testCheckLimitPositive = TestCase (assertEqual "test is limit positive"
                    (Nothing)
                    (getOpts (Just defaultConf)
                    ["-n", "2", "-l", "-0.8", "-f", "in"])) 

testUnknownArg :: Test
testUnknownArg = TestCase (assertEqual "test unknown arg"
                    (Nothing)
                    (getOpts (Just defaultConf)
                    ["-none", "10", "-l", "8", "-f", "in"]))

testTooManyArgs :: Test
testTooManyArgs = TestCase (assertEqual "test too many args"
                    (Nothing)
                    (getOpts (Just defaultConf)
                    ["-n", "10", "-l", "8", "-f", "in", "none", "23"]))

testNotEnoughArgs :: Test
testNotEnoughArgs = TestCase (assertEqual "test not enough args"
                    (Nothing)
                    (checkOpts (getOpts (Just defaultConf)
                    ["-n", "10", "-f", "in"])))
                    
tests :: Test
tests = TestList [TestLabel "test valid args" testValidArgs,
                  TestLabel "test is nb colors an int" testCheckNbColorsInt,
                  TestLabel "test unknown arg" testUnknownArg,
                  TestLabel "test too many args" testTooManyArgs,
                  TestLabel "test not enough args" testNotEnoughArgs,
                  TestLabel "test is nb colors positive" testCheckNbColorsPositive,
                  TestLabel "test is limit positive" testCheckLimitPositive]

main :: IO Counts

main = runTestTT tests