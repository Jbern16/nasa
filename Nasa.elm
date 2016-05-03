import Http exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp
import Json.Decode as Json exposing (..)
import Task exposing (..)
import Effects exposing (..)


type Action
  = NoOp
  | FetchData
  | ErrorOccured String
  | DataFetched Data


type alias Data =
  { explanation : String
  , url : String
  }

type alias Model =
  { message : String
  , data : Data
  }

init =
  let
    model =
      { message = "Hello, Elm!"
      , data = { explanation = "", url = "" }
      }
  in
    (model, Effects.none)

update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    FetchData ->
      ( { model | message = "Initating data fetch!" }, fetchData)
    ErrorOccured errorMessage ->
      ( { model | message = "Oops! An Error has Occured: " ++ errorMessage }, Effects.none )
    DataFetched data ->
      ( { model | data = data, message = "The data has been fetched" }, Effects.none )


httpResultToAction result =
  case result of
    Ok data ->
      DataFetched data
    Err err ->
      ErrorOccured (toString err)

view : Signal.Address Action -> Model -> Html
view address model =
  let
    data = model.data
  in
    div [ ]
      [ div [ ] [ text model.message ]
      , button [ onClick address FetchData ] [ text "Click to load Apod" ]
      , div [ ] [ text data.explanation ]
      , img [ src data.url ] [ ]
      ]

apodDecoder =
  Json.object2
    Data
    ( "explanation" := Json.string )
    ( "hdurl" := Json.string )


fetchData =
  Http.get apodDecoder ("https://api.nasa.gov/planetary/apod?api_key=")
  |> Task.toResult
  |> Task.map httpResultToAction
  |> Effects.task


----------------

app = StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = [ ]
  }

main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks
