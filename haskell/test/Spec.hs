import Lib
import Test.Tasty
import Test.Tasty.HUnit

testParseDirection :: TestTree
testParseDirection = testGroup "parseDirection tests"
                 [ testCase "North" $ parseDirection "NORTH" @?= Just North
                 , testCase "East"  $ parseDirection "EAST"  @?= Just East
                 , testCase "South" $ parseDirection "SOUTH" @?= Just South
                 , testCase "West"  $ parseDirection "WEST"  @?= Just West
                 ]

testParseLine :: TestTree
testParseLine = testGroup "parseLine tests"
                 [ testCase "Place" $ parseLine "PLACE 3,1,NORTH"
                      @?= (Just $ Place (3, 1, North))
                 , testCase "Move" $ parseLine "MOVE" @?= (Just Move)
                 , testCase "Left" $ parseLine "Left" @?= (Just TurnLeft)
                 , testCase "Right" $ parseLine "right" @?= (Just TurnRight)
                 , testCase "Report" $ parseLine "rEpORT" @?= (Just Report)
                 ]

testParse :: TestTree
testParse = testGroup "parse tests"
             [ testCase "sample 1" $ parse ["PLACE 0,0,NORTH", "MOVE", "REPORT"]
                  @?= [Place (0,0,North), Move, Report]
             , testCase "sample 2" $ parse ["PLACE 0,0,NORTH", "LEFT", "REPORT"]
                  @?= [Place (0,0,North), TurnLeft, Report]
             , testCase "sample 3" $ parse [ "PLACE 1,2,EAST"
                                           , "MOVE"
                                           , "MOVE"
                                           , "LEFT"
                                           , "MOVE"
                                           , "REPORT"]
                  @?= [Place (1,2,East), Move, Move, TurnLeft, Move, Report]
             ]

testReport :: TestTree
testReport = testGroup "test report"
              [ testCase "report (0,0,North)"
                    $ report (0,0,North) @?= "0,0,NORTH"
              , testCase "report (3,4,East)" $ report (3,4,East) @?= "3,4,EAST"
              ]

testIsValid :: TestTree
testIsValid = testGroup "test isValid"
              [ testCase "isValid (0,0,North)" $ isValid (0,0,North) @?= True
              , testCase "isValid (4,1,East)" $ isValid (4,1,East) @?= True
              , testCase "isValid (-1,0,South)" $ isValid (-1,0,South) @?= False
              , testCase "isValid (3,5,West)" $ isValid (3,5,West) @?= False
              ]

tests :: TestTree
tests = testGroup "All Tests"
                  [ testParseDirection
                  , testParseLine
                  , testParse
                  , testReport
                  , testIsValid
                  ]

main :: IO ()
main = defaultMain tests
