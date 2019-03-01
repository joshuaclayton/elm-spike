module Update exposing
    ( init
    , subscriptions
    , update
    )

import Model exposing (..)


init : Model.Flags -> ( Model, Cmd Msg )
init =
    always
        ( Model.initial
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetIndividual ->
            ( { model | taxClassification = setTaxClassificationToIndividual model.taxClassification }, Cmd.none )

        SetCompany ->
            ( { model | taxClassification = setTaxClassificationToCompany model.taxClassification }, Cmd.none )

        SetTinValue input ->
            ( { model | taxClassification = setTinOnTaxClassification input model.taxClassification }
            , Cmd.none
            )

        SetTinType tinType ->
            ( { model | taxClassification = setTinTypeOnTaxClassification tinType model.taxClassification }
            , Cmd.none
            )

        SelectMultipleChoiceField field input ->
            ( { model | fieldValues = upsertFieldResponse field input model.fieldValues }, Cmd.none )
