module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String exposing (..)

main : Program Never Model Msg
main = beginnerProgram { 
    model = init,
    view = display,
    update = refresh }

{- MODEL -}

type alias Calc = { 
    addition : Float -> Float -> Float,
    subtraction : Float -> Float -> Float,
    multiplication : Float -> Float -> Float,
    floatDivision : Float -> Float -> Float,
    power : Float -> Float -> Float,
    root : Float -> Float -> Float,  
    plusTax : Float -> Float -> Float, 
    minusTax : Float -> Float -> Float,
    totalTax : Float -> Float -> Float,
    combinatrix : Float -> Float -> Float }
    -- intDivision : Float -> Float -> Float }

operations : Calc
operations = { 
    addition = (\x y -> x + y), 
    subtraction = (\x y -> x - y),
    multiplication = (\x y -> x * y),
    floatDivision = (\x y -> x / y),
    power = (\x y -> x ^ y),
    root = (\x y -> x ^ (1/y)), 
    plusTax = (\x y -> x * (y/100)), 
    minusTax = (\x y -> x - (x * (y/100))),
    totalTax = (\x y -> x + (x * (y/100))),
    combinatrix = (\x y -> check x y) }
    -- intDivision = (\x y -> x // y) }

type alias Model = { 
    screen : String, 
    func : Float -> Float -> Float, 
    answer : Float,
    group : Bool }


init : Model
init = { 
    screen = "", 
    func = (\x y -> y), 
    answer = 0, 
    group = True }

parseAnswer : String -> Float
parseAnswer ans = Result.withDefault 0 (String.toFloat ans)

operation : Model -> (Float -> Float -> Float) -> Model
operation model func = { 
    model
      | func = func,
        answer = (parseAnswer model.screen),
        group = False }

{- UPDATE/REFRESH -}

type Msg
    = None
    | Divide
    -- | Integer
    | Combo
    | Root
    | Power
    | Multiply
    | Subtract
    | Plus
    | PlusTax
    | TotalTax
    | MinusTax
    | Equal
    | Decimal
    | Zero
    | Number Int
    | Clear

refresh : Msg -> Model -> Model
refresh msg model =
    case msg of
        None -> model

        Clear -> init

        Number number -> refreshAnswer model number

        Decimal -> decimal model

        Zero -> zero model

        Divide -> operation model operations.floatDivision

        --Integer -> operation model operations.intDivision

        Combo -> operation model operations.combinatrix

        Power -> operation model operations.power

        Root -> operation model operations.root

        PlusTax -> operation model operations.plusTax

        MinusTax -> operation model operations.minusTax

        TotalTax -> operation model operations.totalTax

        Multiply -> operation model operations.multiplication

        Subtract -> operation model operations.subtraction

        Plus -> operation model operations.addition

        Equal -> equal model

refreshAnswer : Model -> Int -> Model
refreshAnswer model number = if model.group then { 
    model | screen = model.screen ++ toString (number) }
  else {
    model | screen = toString (number), group = True }

equal : Model -> Model
equal model = 
    if model.group then { 
        model
          | screen = calc model, 
            answer = (parseAnswer model.screen), 
            group = False }
    else { 
        model
          | screen = calc model,
            group = False }

calc : Model -> String
calc model = model.func model.answer (parseAnswer model.screen) |> toString

zero : Model -> Model
zero model =
    if String.isEmpty model.screen || not model.group then { 
        model
          | screen = "0", 
          group = False }
    else { 
        model 
          | screen  = model.screen ++ "0" }

decimal : Model -> Model
decimal model =
    if not (String.isEmpty model.screen) && model.group then { 
        model 
          | screen = addDecimal model.screen }
    else { 
        model 
          | screen = "0.", group = True }

addDecimal : String -> String
addDecimal string =
    if String.contains "." string then
        string
    else
        string ++ "."

{- VIEW -}

action : Msg -> String -> Html Msg
action msg buttonText =
    button [ class "button", onClick msg ]
        [ span [] [ text (buttonText) ] ]

actionWide : Msg -> String -> Html Msg
actionWide msg buttonText =
    button [ class "button wide", onClick msg ]
        [ span [] [ text (buttonText) ] ]

actionHeight : Msg -> String -> Html Msg
actionHeight msg buttonText =
    button [ class "button height", onClick msg ]
        [ span [] [ text (buttonText) ] ]

stylesheet : Html Msg
stylesheet = let
    tag = "link"
    attrs = [ 
        attribute "Rel" "stylesheet", 
        attribute "property" "stylesheet",
        attribute "href" "styles.css" ]
    children = []
  in node tag attrs children

styleTitle : Attribute msg
styleTitle = style [ 
    ("backgroundColor", "black"),
    ("font-size","40px"),
    ("color", "white"),
    ("font-family", "Arial"),
    ("textAlign", "center"),
    --("height", "45px"),
    ("padding", "30px 0px 30px 0px"),
    ("margin", "0px 480px 0px 480px") ]

styleFort : Attribute msg
styleFort = style [ 
    ("backgroundColor", "black"),
    ("font-size","8px"),
    ("color", "white"),
    ("padding", "0px 0px 0px 0px"),
    ("display", "inline-block"),
    ("margin", "0px 0px 0px 0px") ]

display : Model -> Html Msg
display model =
    div []
        [
          h1 [styleTitle] [text "MY DOPE CALCULATOR"]
    , div [ class "calculator" ]
        [ stylesheet
        , div [ class "row" ]
            [ div [ class "col-xs-12" ]
                [ div [ class "display" ]
                    [ div [ class "display-text" ]
                        [ text (model.screen) ]
                    ]
                , div [ class "buttons" ]
                    [ action (Number 1) "1" 
                    , action (Number 2) "2"
                    , action (Number 3) "3"
                    , action (Number 4) "4"
                    , action (Number 5) "5"
                    , action (Number 6) "6"
                    , action (Number 7) "7"
                    , action (Number 8) "8"
                    , action (Number 9) "9"
                    , action Plus "+"
                    , action Subtract "-"
                    , action Zero "0"
                    , action Multiply "x"
                    , action Divide "÷"
                    , action Decimal "."
                    , action Power "x ^ y"
                    , action Root "x√y"
                    , action Combo "nCr"
                    , action PlusTax "Tax"
                    , action MinusTax "Minus Tax"
                    , action TotalTax "Total Tax"
                    --, action Add "Prime #?"
                    --, action Add "Even #?"
                    --, action Add "Odd #?"
                    , actionHeight Equal "="
                    , actionWide Clear "RESET"
                    ]
                ]
            ]
        ]
        , div []
            [
              h1 [styleTitle] [text "MY DOPE CALCULATOR"]
            , p [styleFort] [text "P.S. Fortnite Is Better Than PUBG"]
            ]
    ]

{- HELPER FUNCTIONS -}

mod : Int -> Int -> Float
mod x y = Basics.toFloat(x % y)

toFloa : Int -> Float
toFloa x = Basics.toFloat(x)

check x y = if x < y then 0 else combinatorics x y

combinatorics : Float -> Float -> Float
combinatorics x y = let
    factX = factorial x
    factY = factorial y
    factB = factorial (x - y)
  in (factX / (factY * factB))

factorial : Float -> Float
factorial n = if n == 0 then 1 else n * factorial (n - 1)
