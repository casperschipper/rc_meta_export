module RCJson exposing (PublicationStatus, Research, decodeResearch, statusToString)

import Json.Decode exposing (Decoder, field, int, maybe, string)
import Json.Decode.Extra as JDE


type alias Research =
    { id : Int
    , title : String
    , keywords : List String
    , created : String
    , author : String
    , issueId : Maybe Int
    , publicationStatus : PublicationStatus -- should be string?
    , publication : Maybe String
    , doi : Maybe String
    , authorId : Int
    , license : String
    , copyright : String
    }


type PublicationStatus
    = InProgress
    | Published
    | LocalPublication
    | Undecided


statusToString : PublicationStatus -> String
statusToString status =
    case status of
        InProgress ->
            "in progress"

        Published ->
            "published"

        LocalPublication ->
            "local publication"

        Undecided ->
            "unkown"


decodeResearch : Decoder (List Research)
decodeResearch =
    Json.Decode.list entry


calcStatus : Research -> PublicationStatus
calcStatus research =
    case research.publicationStatus of
        InProgress ->
            InProgress

        _ ->
            case research.issueId of
                Just _ ->
                    Published

                Nothing ->
                    Published


entry : Decoder Research
entry =
    let
        researchPublicationStatus : Research -> Research
        researchPublicationStatus research =
            { research | publicationStatus = calcStatus research }

        statusFromString : String -> PublicationStatus
        statusFromString statusString =
            case statusString of
                "published" ->
                    Published

                "in progress" ->
                    InProgress

                _ ->
                    Undecided
    in
    Json.Decode.map researchPublicationStatus <|
        (Json.Decode.succeed
            Research
            |> JDE.andMap (field "id" int)
            |> JDE.andMap (field "title" string)
            |> JDE.andMap (field "keywords" (Json.Decode.list string))
            |> JDE.andMap (field "created" string)
            |> JDE.andMap (field "author" <| field "name" string)
            |> JDE.andMap (maybe (field "issue" <| field "id" int))
            |> JDE.andMap (Json.Decode.map statusFromString (field "status" string))
            |> JDE.andMap (maybe (field "published" string))
            |> JDE.andMap (field "doi" <| maybe (field "url" string))
            |> JDE.andMap (field "author" <| field "id" int)
            |> JDE.andMap (Json.Decode.succeed "All rights reserved")
            |> JDE.andMap (field "author" (field "name" string))
        )