module Piece exposing (..)

import Dict
import List exposing (..)
import Matrix exposing (Matrix(..))
import Maybe
import Set exposing (Set(..))


type alias Point =
    ( Int, Int )


type Piece
    = Piece (Set Point)


type alias Dice =
    Set Point


type DiceType
    = Dice1
    | Dice2
    | Dice3
    | Dice4
    | Dice5
    | Dice6
    | Dice7


getDice : DiceType -> Dice
getDice diceType =
    case diceType of
        Dice1 ->
            Set.fromList [ ( 4, 5 ), ( 5, 4 ), ( 5, 5 ), ( 5, 6 ), ( 6, 4 ), ( 6, 5 ) ]

        Dice2 ->
            Set.fromList [ ( 1, 6 ), ( 6, 1 ) ]

        Dice3 ->
            Set.fromList [ ( 1, 2 ), ( 1, 3 ), ( 2, 1 ), ( 2, 2 ), ( 2, 3 ), ( 3, 2 ) ]

        Dice4 ->
            Set.fromList [ ( 2, 4 ), ( 3, 3 ), ( 3, 4 ), ( 4, 3 ), ( 4, 4 ), ( 5, 3 ) ]

        Dice5 ->
            Set.fromList [ ( 1, 1 ), ( 3, 1 ), ( 4, 1 ), ( 4, 2 ), ( 5, 2 ), ( 6, 3 ) ]

        Dice6 ->
            Set.fromList [ ( 1, 5 ), ( 2, 6 ), ( 5, 1 ), ( 6, 2 ) ]

        Dice7 ->
            Set.fromList [ ( 1, 4 ), ( 2, 5 ), ( 3, 5 ), ( 3, 6 ), ( 4, 6 ), ( 6, 6 ) ]


fromSet : Set Point -> Piece
fromSet =
    Piece


fromList : List Point -> Piece
fromList =
    Set.fromList >> fromSet


type PieceType
    = Blocker
    | Blue
    | Brown
    | Orange
    | Purple
    | Yellow
    | Cyan
    | Green
    | Red
    | Grey


getPiece : PieceType -> Piece
getPiece pieceType =
    case pieceType of
        Blocker ->
            fromList [ ( 0, 0 ) ]

        Blue ->
            fromList [ ( 0, 0 ) ]

        Brown ->
            fromList [ ( 0, 0 ), ( 1, 0 ) ]

        Orange ->
            fromList [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]

        Purple ->
            fromList [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ) ]

        Yellow ->
            fromList [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 1, 1 ) ]

        Cyan ->
            fromList [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 1, 0 ) ]

        Green ->
            fromList [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ]

        Red ->
            fromList [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 2, 1 ) ]

        Grey ->
            fromList [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ) ]


matrixFromList : Int -> Int -> List Int -> Matrix Int
matrixFromList rows columns items =
    Matrix.fromList rows columns items |> Maybe.withDefault Matrix.empty


dotMatrix : Matrix Int -> Matrix Int -> Matrix Int
dotMatrix m1 m2 =
    Matrix.dot m1 m2 |> Maybe.withDefault Matrix.empty


identityMatrix : Matrix Int
identityMatrix =
    Matrix.identity 2


rotateNinetyMatrix : Matrix Int
rotateNinetyMatrix =
    matrixFromList 2 2 [ 0, 1, -1, 0 ]


flipMatrix : Matrix Int
flipMatrix =
    matrixFromList 2 2 [ -1, 0, 0, 1 ]


allRotationMatrices : Matrix Int -> List (Matrix Int)
allRotationMatrices start =
    let
        rotateNinety : Matrix Int -> Matrix Int
        rotateNinety =
            dotMatrix rotateNinetyMatrix
    in
    List.foldl (\_ ( last, rotations ) -> ( rotateNinety last, rotateNinety last :: rotations )) ( start, [ start ] ) [ 90, 180, 270 ]
        |> Tuple.second


allTransformationMatrices : List (Matrix Int)
allTransformationMatrices =
    allRotationMatrices identityMatrix ++ allRotationMatrices flipMatrix


transform : Matrix Int -> Piece -> Piece
transform matrix (Piece points) =
    let
        pointToMatrix : Point -> Matrix Int
        pointToMatrix ( a, b ) =
            matrixFromList 2 1 [ a, b ]

        matrixToPoint : Matrix Int -> Point
        matrixToPoint m =
            case Matrix.toList m of
                [ a, b ] ->
                    ( a, b )

                _ ->
                    Debug.todo "Invalid 2x1 matrix"

        transformPoint : Point -> Point
        transformPoint point =
            point |> pointToMatrix |> dotMatrix matrix |> matrixToPoint
    in
    Piece <|
        Set.map transformPoint points


normalise : Piece -> Piece
normalise (Piece points) =
    let
        listPoints =
            Set.toList points

        xs =
            List.map Tuple.first listPoints

        ys =
            List.map Tuple.second listPoints

        minX =
            List.minimum xs |> Maybe.withDefault 0

        minY =
            List.minimum ys |> Maybe.withDefault 0

        translatePoint : Point -> Point
        translatePoint ( x, y ) =
            ( x - minX, y - minY )
    in
    Piece <|
        Set.map translatePoint points


removeDuplicates : List Piece -> List Piece
removeDuplicates pieces =
    List.map (\(Piece points) -> ( Set.toList points, points )) pieces
        |> Dict.fromList
        |> Dict.values
        |> List.map Piece


transformations : Piece -> List Piece
transformations piece =
    List.map (\t -> transform t piece) allTransformationMatrices
        |> List.map normalise
        |> removeDuplicates
