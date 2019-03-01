module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)


displayField : Model -> Field -> Html Msg
displayField model field =
    case field of
        IndividualField ->
            div []
                [ label [] [ text "Yes" ]
                , input [ type_ "radio", name "individual", value "1", onClick SetIndividual ] []
                , label [] [ text "No" ]
                , input [ type_ "radio", name "individual", value "0", onClick SetCompany ] []
                ]

        TinField ->
            div []
                [ label [] [ text "EIN" ]
                , input [ type_ "radio", name "tin", value "1", checked <| currentTinType model == EIN, onClick <| SetTinType EIN ] []
                , label [] [ text "SSN" ]
                , input [ type_ "radio", name "tin", value "0", checked <| currentTinType model == SSN, onClick <| SetTinType SSN ] []
                ]



-- Are you an individual?
-- * If yes, are you using an EIN or SSN?
-- * If no, it'll always be EIN.


view : Model -> Html Msg
view model =
    div []
        [ text "Hello world"
        , br [] []
        , text "hey there!!!!"
        , displayField model TinField
        , displayField model IndividualField
        , input [ type_ "text", onInput SetTinValue ] []
        ]
