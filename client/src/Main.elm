module Main exposing (main)

--| TODO: Adicionar Cors no backend e testar novamente

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type alias LoginForm =
    { username : String
    , password : String
    }

type FormState
    = ShowForm
    | Failure
    | Loading
    | Success

type alias Model
  = { loginForm: LoginForm
    , formState: FormState
    }

init : () -> (Model, Cmd Msg)
init _ =
  (Model {username = "", password =  ""} ShowForm, Cmd.none)


type Msg
  = Logar
  | SetUsername String
  | SetPassword String
  | GotValidation (Result Http.Error User)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Logar -> ({ model | formState = Loading } |> Debug.log "chamou o logar", validateUser model)

    SetUsername newUsernameValue ->
        let
            oldForm = model.loginForm
            newForm = { oldForm | username = newUsernameValue }
        in
            ({ model | loginForm = newForm} |> Debug.log "atualizou usuario", Cmd.none)
    
    SetPassword newPasswordValue ->
        let
            oldForm = model.loginForm
            newForm = { oldForm | password = newPasswordValue }
        in 
            ({ model | loginForm = newForm} |> Debug.log "atualizou senha", Cmd.none)

    GotValidation result ->
        case result of
            Ok _ -> ({ model | formState = Success}, Cmd.none)
            _    -> ({ model | formState = Failure }, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div [ class "column col-2 col-mx-auto flex-centered" ]
    [ viewGif model ]


viewGif : Model -> Html Msg
viewGif model =
  case model.formState of
    Failure ->
      div [ class "toast toast-error" ]
        [ text "Erro: Não foi possível logar com o usuário informado. " ]


    Loading ->
      div [ class "loading loading-lg"] []

    Success ->
      div [ class "toast toast-success" ]
        [ text "Logado com sucesso" ]

    ShowForm -> viewForm

viewForm =
    div [ style "padding-top" "100px" ]
        [ div [ class "form-group" ]
              [ label [ class "form-label"
                      , for "password" ] [ text "Password"]
              , input [ onInput SetUsername
                      , class "form-input"
                      , type_ "text"
                      , id "username" ] []
              ]
        , div [ class "form-group" ]
              [ label [ class "form-label"
                      , for "password" ] [ text "Senha"]
              , input [ onInput SetPassword
                      , class "form-input"
                      , type_ "password"
                      , id "password"] []
              ]
        , div [ class "form-group" ]
              [ button [ class "btn"
                       , style "margin-top" "12px"
                       , onClick Logar ] [ text "Logar" ]
              ]
        ]

-- HTTP

userEncoder : Model -> Encode.Value
userEncoder model =
    Encode.object
        [ ("loginUsername", Encode.string model.loginForm.username)
        , ("loginPassword", Encode.string model.loginForm.password)
        ]    

validateUser : Model -> Cmd Msg
validateUser model =
    let
        body = model |> userEncoder |> Http.jsonBody
    in
        Http.post { url = "http://localhost:8080/api/v1/loginUser"
                  , body = body
                  , expect = Http.expectJson GotValidation userDecoder}
-- getRandomCatGif : Cmd Msg
-- getRandomCatGif =
--   Http.get
--     { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
--     , expect = Http.expectJson GotGif gifDecoder
--     }


type alias User =
    { id : Int
    , username : String
    , email: String }

usersDecoder : Decoder (List User)
usersDecoder = Decode.list userDecoder

userDecoder : Decoder User
userDecoder =
    map3 User
        (field "id" Decode.int)
        (field "username" Decode.string)
        (field "email" Decode.string)

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
