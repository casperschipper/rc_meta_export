module Main exposing (..)

import Browser
import Html exposing (Html, pre, text)
import Http
import Imf.DateTime
import Parser
import Result.Extra exposing (andMap)
import Time exposing (Posix)
import XmlParser as XP exposing (Node(..), Xml)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure
    | Loading
    | Success String Xml


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { expect = Http.expectString GotText
        , url = "kcfeed2.xml"
        }
    )



-- UPDATE


type Msg
    = GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText res ->
            case res of
                Ok fullText ->
                    let
                        _ =
                            Debug.log "fulltext" fullText

                        parsed =
                            XP.parse fullText
                    in
                    case parsed of
                        Ok x ->
                            let
                                _ =
                                    Debug.log "xml" x
                            in
                            ( Success fullText x, Cmd.none )

                        Err err ->
                            let
                                _ =
                                    Debug.log "err" err
                            in
                            ( Failure, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load your book."

        Loading ->
            text "Loading..."

        Success fullText xml ->
            let
                _ =
                    Debug.log "xml" xml

                expos =
                    fromXml xml |> Result.map (\exp -> exp |> List.map toString |> String.join "\n\n")

                string =
                    case expos of
                        Ok str ->
                            str

                        Err prob ->
                            "error\n" ++ fromProblem prob
            in
            pre [] [ text string ]


type Status
    = Published
    | InProgress


status : String -> Status
status str =
    case str of
        "published" ->
            Published

        "in progress" ->
            InProgress

        _ ->
            InProgress


expositionMeta : String -> Int -> String -> String -> String -> Posix -> ExpositionMeta
expositionMeta title id link desc stat posix =
    ExpositionMeta
        { title = title
        , id = id
        , link = link
        , description = desc
        , status = status stat
        , pubDate = posix
        }


type ExpositionMeta
    = ExpositionMeta
        { title : String
        , id : Int
        , link : String
        , description : String
        , status : Status
        , pubDate : Posix
        }


fromStatus : Status -> String
fromStatus stat =
    case stat of
        InProgress ->
            "InProgress"

        Published ->
            "Published"


toString : ExpositionMeta -> String
toString (ExpositionMeta m) =
    [ m.title, String.fromInt m.id, m.link, m.description, fromStatus m.status ] |> String.join ", "


type Problem
    = CannotFindTag String
    | TooMany
    | Oops
    | NoText
    | DateParserError (List Parser.DeadEnd)
    | CouldNotRetrieveID String


fromProblem : Problem -> String
fromProblem prob =
    case prob of
        CannotFindTag str ->
            "Cannot find tag :" ++ str

        TooMany ->
            "Too many children"

        Oops ->
            "Oops "

        NoText ->
            "Element has no text"

        DateParserError deadEnd ->
            "Couldn't parse date: " ++ Parser.deadEndsToString deadEnd

        CouldNotRetrieveID str ->
            "Could not find id in : " ++ str


findNodeWithName : String -> List Node -> Result Problem Node
findNodeWithName name nodes =
    let
        hasName node =
            case node of
                Element elemName _ _ ->
                    elemName == name

                _ ->
                    let
                        _ =
                            Debug.log "test" node
                    in
                    False
    in
    nodes
        |> List.filter hasName
        |> List.head
        |> Result.fromMaybe (CannotFindTag name)


children : Node -> List Node
children node =
    case node of
        Element _ _ nodes ->
            nodes

        Text _ ->
            []


getText : Node -> Result Problem String
getText node =
    case node of
        Element _ _ [ Text text ] ->
            Ok text

        _ ->
            Err NoText


findText : String -> List Node -> Result Problem String
findText str nodes =
    nodes |> findNodeWithName str |> Result.andThen getText


textElement : String -> Node -> Result Problem String
textElement str node =
    case node of
        Element name _ [ Text txt ] ->
            if name == str then
                Ok txt

            else
                Err (CannotFindTag str)

        Element name _ _ ->
            Err TooMany

        _ ->
            Err (CannotFindTag str)


filterName : String -> Node -> Bool
filterName match node =
    case node of
        Element name _ _ ->
            name == match

        _ ->
            False


publicationDate : String -> Result Problem Posix
publicationDate str =
    let
        res =
            Imf.DateTime.toPosix str
    in
    case res of
        Ok psx ->
            Ok psx

        Err deadEnds ->
            Err (DateParserError deadEnds)


nth idx lst =
    lst
        |> List.drop idx
        |> List.head


getIdFromLink link =
    let try = String.split "/" link |> nth 3 in
    case try of 
        Just str -> 
            str |> String.toInt |> Result.fromMaybe (CouldNotRetrieveID link) 

        Nothing -> Err <| CouldNotRetrieveID link


fromItem : Node -> Result Problem ExpositionMeta
fromItem node =
    node
        |> children
        |> (\nodes ->
                Ok expositionMeta
                    |> andMap (findText "title" nodes)
                    |> andMap (Ok "123456" |> Result.map (String.toInt >> Maybe.withDefault 0))
                    |> andMap (findText "link" nodes)
                    |> andMap (findText "description" nodes)
                    |> andMap (findText "status" nodes)
                    |> andMap (findText "pubDate" nodes |> Result.andThen publicationDate)
           )


list : List Node -> (Node -> a) -> List a
list nodes fromNode =
    List.map fromNode nodes


fromXml : Xml -> Result Problem (List ExpositionMeta)
fromXml xml =
    xml.root
        |> children
        |> findNodeWithName "channel"
        |> Result.map (children >> List.filter (filterName "item"))
        |> Result.andThen (List.map fromItem >> Result.Extra.combine)
