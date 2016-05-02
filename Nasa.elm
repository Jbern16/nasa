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
  | DataFetched String


type alias Model =
  { message : String
  , explanation : String
  }

init =
  let
    model =
      { message = "Hello, Elm!"
      , explanation = ""
      }
  in
    (model, Effects.none)

update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    FetchData ->
      ( {model | message = "Initating data fetch!" }, fetchDataAsEffects)
    ErrorOccured errorMessage ->
      ( { model | message = "Oops! An Error has Occured: " ++ errorMessage }, Effects.none )
    DataFetched explanation ->
      ({ model | explanation = explanation, message = "The data has been fetched" }, Effects.none )


httpResultToAction result =
  case result of
    Ok explanation ->
      DataFetched explanation
    Err err ->
      ErrorOccured (toString err)

view : Signal.Address Action -> Model -> Html
view address model =
  let
    showRepo repo =
      li [ ]
      [ text ("Explanation:" ++ (toString repo.explanation) )
      ]
  in
    div []
      [ div [] [ text model.message ]
      , button [ onClick address FetchData ] [ text "Click to load Apod" ]
      , div [ ] [ text model.explanation ]
      ]

apodDecoder =
  Json.at ["explanation"] Json.string

fetchData =
  Http.get apodDecoder ("https://api.nasa.gov/planetary/apod?api_key=JtYyGRj0iPEje9cmVMNg8O93I6yl8RT4xqCxTJ0D")
  |> Task.toResult

fetchDataAsEffects =
  fetchData
    |> Task.map httpResultToAction
    |> Effects.task


app = StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = [ ]
  }

main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks
