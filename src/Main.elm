module Main exposing (..)

import Browser
import File.Download as Download
import Html exposing (Html, button, div, pre, text)
import Html.Events exposing (onClick)
import Http
import Imf.DateTime
import Json.Encode as E
import Parser
import RCJson exposing (PublicationStatus, Research, decodeResearch)
import Result.Extra exposing (andMap)
import Time exposing (Posix)
import XmlParser2 as XP exposing (Node(..), Xml)



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
    | Finished String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { expect = Http.expectString GotText
        , url = "rss.xml"
        }
    )



-- UPDATE


type Msg
    = GotText (Result Http.Error String)
    | GotJson (Result Http.Error (List Research))
    | Download


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
                            ( Success fullText x
                            , Http.get
                                { expect = Http.expectJson GotJson RCJson.decodeResearch
                                , url = "internal_research.json"
                                }
                            )

                        Err err ->
                            let
                                _ =
                                    Debug.log "err" err
                            in
                            ( Failure, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        GotJson result ->
            case result of
                Err err ->
                    let
                        _ =
                            Debug.log "error decoding json" err
                    in
                    ( Failure, Cmd.none )

                Ok research ->
                    case model of
                        Success _ xml ->
                            let
                                attempt =
                                    xml
                                        |> fromXml
                                        |> Result.map
                                            (\metas ->
                                                metas |> List.map (merge research)
                                            )
                            in
                            case attempt of
                                Ok mergedSuccess ->
                                    let
                                        onlyTheJust =
                                            List.foldr
                                                (\x acc ->
                                                    case x of
                                                        Just some ->
                                                            some :: acc

                                                        Nothing ->
                                                            acc
                                                )
                                                []
                                                mergedSuccess
                                    in
                                    onlyTheJust
                                        |> encoder
                                        |> E.encode 4
                                        |> (\str -> ( Finished str, Cmd.none ))

                                Err e ->
                                    let
                                        _ =
                                            Debug.log "error" e
                                    in
                                    ( Failure, Cmd.none )

                        wrongModel ->
                            let
                                _ =
                                    Debug.log "there is a problem" wrongModel
                            in
                            ( Failure, Cmd.none )

        Download ->
            case model of
                Finished json ->
                    ( model, Download.string "export.json" "text/json" json )

                _ ->
                    ( model, Cmd.none )



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

        Finished mergedJson ->
            div []
                [ button [ onClick Download ] [ text "download" ]
                , pre [] [ text mergedJson ]
                ]


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


expositionMeta : String -> Int -> String -> String -> String -> Posix -> Enclosure -> ExpositionMeta
expositionMeta title id link desc stat posix encl =
    ExpositionMeta
        { title = title
        , id = id
        , link = link
        , description = desc
        , status = status stat
        , pubDate = posix
        , enclosure = encl
        }


type ExpositionMeta
    = ExpositionMeta
        { title : String
        , id : Int
        , link : String
        , description : String
        , status : Status
        , pubDate : Posix
        , enclosure : Enclosure
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
    | IncorrectEnclosure


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

        IncorrectEnclosure ->
            "IncorrectEnclosure"


findNodeWithName : String -> List Node -> Result Problem Node
findNodeWithName name nodes =
    let
        hasName node =
            case node of
                Element elemName _ _ ->
                    elemName == name

                _ ->
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
    let
        try =
            String.split "/" link |> nth 4
    in
    case try of
        Just str ->
            str |> String.toInt |> Result.fromMaybe (CouldNotRetrieveID link)

        Nothing ->
            Err <| CouldNotRetrieveID link


type alias Enclosure =
    { url : String
    , fileType : String
    }


attrWithName : String -> List XP.Attribute -> Maybe String
attrWithName name attrs =
    attrs |> List.filter (\a -> a.name == name) |> List.head |> Maybe.map (\a -> a.value)


enclosure : Node -> Result Problem Enclosure
enclosure node =
    case node of
        Element "enclosure" attrs _ ->
            let
                _ =
                    Debug.log "attrs" attrs
            in
            Maybe.map2 Enclosure (attrWithName "url" attrs) (attrWithName "type" attrs) |> Result.fromMaybe IncorrectEnclosure

        _ ->
            Ok defaultThumb


fallback : a -> Result Problem a -> Result Problem a
fallback replacement result =
    case result of
        Ok r ->
            Ok r

        Err _ ->
            Ok replacement


defaultThumb =
    Enclosure "null" "null"


fromItem : Node -> Result Problem ExpositionMeta
fromItem node =
    node
        |> children
        |> (\nodes ->
                Ok expositionMeta
                    |> andMap (findText "title" nodes)
                    |> andMap (findText "link" nodes |> Result.andThen getIdFromLink)
                    |> andMap (findText "link" nodes)
                    |> andMap (findText "description" nodes)
                    |> andMap (findText "status" nodes)
                    |> andMap (findText "pubDate" nodes |> Result.andThen publicationDate)
                    |> andMap (findNodeWithName "enclosure" nodes |> Result.andThen enclosure |> fallback defaultThumb)
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
        |> Result.map (List.map fromItem)
        |> Result.map Result.Extra.partition
        |> Result.map Tuple.first


type alias MergedExposition =
    { id : Int
    , title : String
    , keywords : List String
    , description : String
    , metaPage : String
    , issue : String
    , pubDate : Posix
    , doi : String
    , status : PublicationStatus
    , author : String
    , authorProfile : String
    , authorId : Int
    , copyright : String
    , license : String
    , thumb : String
    }


findResearchWithId : Int -> List Research -> Maybe Research
findResearchWithId id lst =
    let
        test : Research -> Bool
        test r =
            r.id == id
    in
    lst |> List.filter test |> List.head


metaPageFromId : Int -> String
metaPageFromId id =
    "https://www.researchcatalogue.net/profile/show-exposition?exposition=" ++ String.fromInt id


authorProfile : Int -> String
authorProfile id =
    "https://www.researchcatalogue.net/profile/?person=" ++ String.fromInt id


merge : List Research -> ExpositionMeta -> Maybe MergedExposition
merge lst (ExpositionMeta meta) =
    findResearchWithId meta.id lst
        |> Maybe.map
            (\research ->
                { id = meta.id
                , title = meta.title
                , keywords = research.keywords
                , description = meta.description
                , metaPage = metaPageFromId meta.id
                , issue = "null"
                , pubDate = meta.pubDate
                , doi = research.doi |> Maybe.withDefault "null"
                , status = research.publicationStatus
                , author = research.author
                , authorProfile = research.authorId |> authorProfile
                , authorId = research.authorId
                , copyright = research.copyright
                , license = research.license
                , thumb = meta.enclosure.url
                }
            )

clean : String -> String
clean str =
    let 
        f c =
            case (c |> Char.toCode) of
                0xE2 -> '\n' 
                
                0x80 -> '\n'
                
                0xA9 -> '\n'

                0x2028 -> '\n'

                0xA0 -> '\n'

                other -> Char.fromCode other
    in
    str |> String.map f  |> String.replace "\r\n" "\\n" |> String.replace "\n" "\\n"

encodeMerged : MergedExposition -> E.Value
encodeMerged mexp =
    E.object
        [ ( "exposition"
          , E.object
                [ ( "id", E.int mexp.id )
                , ( "title", E.string mexp.title )
                , ( "keywords", E.list E.string mexp.keywords )
                , ( "description", E.string (mexp.description |> clean) )
                , ( "metaPage", E.string mexp.metaPage )
                , ( "issue", E.string mexp.issue )
                , ( "pubDate", E.string (mexp.pubDate |> Imf.DateTime.fromPosix Time.utc) )
                , ( "doi", E.string mexp.doi )
                , ( "status", E.string (mexp.status |> RCJson.statusToString) )
                , ( "author", E.string mexp.author )
                , ( "authorProfile", E.string mexp.authorProfile )
                , ( "authorId", E.int mexp.authorId )
                , ( "copyright", E.string mexp.copyright )
                , ( "license", E.string mexp.license )
                , ( "thumb", E.string mexp.thumb )
                ]
          )
        ]


encoder : List MergedExposition -> E.Value
encoder lst =
    E.list encodeMerged lst
