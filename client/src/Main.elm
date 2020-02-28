module Main exposing (main)

--| TODO: Adicionar Cors no backend e testar novamente

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D

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
  | Success String
  | SuccessUser (List User)


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getUsers)


type Msg
  = MorePlease
  | GotGif (Result Http.Error String)
  | GotUsers (Result Http.Error (List User))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getRandomCatGif)

    GotGif result ->
      case result of
        Ok url ->
          (Success url, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

    GotUsers result ->
      case result of
        Ok users ->
          (SuccessUser users , Cmd.none)

        Err _ ->
          (Failure, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Random Cats" ]
    , viewGif model
    ]


viewGif : Model -> Html Msg
viewGif model =
  case model of
    Failure ->
      div []
        [ text "I could not load a random cat for some reason. "
        , button [ onClick MorePlease ] [ text "Try Again!" ]
        ]

    Loading ->
      text "Loading..."

    Success url ->
      div []
        [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
        , img [ src url ] []
        ]

    SuccessUser users ->
      div [] (List.map mkUserView users)


-- HTTP


getRandomCatGif : Cmd Msg
getRandomCatGif =
  Http.get
    { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
    , expect = Http.expectJson GotGif gifDecoder
    }

getUsers : Cmd Msg
getUsers =
    Http.get
        { url = "http://localhost:8080/api/v1/usuarios"
        , expect = Http.expectJson GotUsers usersDecoder }


gifDecoder : D.Decoder String
gifDecoder =
  D.field "data" (D.field "image_url" D.string)

myJson = """[ { "id": 1, "username" : "thales", "email": "teste@teste" } ]"""

type alias User =
    { id : Int
    , username : String
    , email: String }

usersDecoder : D.Decoder (List User)
usersDecoder = D.list userDecoder

userDecoder : D.Decoder User
userDecoder =
    D.map3 User
        (D.field "id" D.int)
        (D.field "username" D.string)
        (D.field "email" D.string)

-- mkUserView : List User 
mkUserView u =
    div []
        [ p [] [ text <| String.fromInt u.id ]
        , p [] [ text u.username ]
        , p [] [ text u.email ]
        ]
-- main =
--     div [] [ case decodeString (list userDecoder) myJson of
--                 (Ok users) -> div [] (List.map mkUserView users)
--                 _ -> div [] [ text "Erro no json"]
--                 ]
