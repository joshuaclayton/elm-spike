module Model exposing
    ( Field(..)
    , FieldResponse(..)
    , Flags
    , Model
    , Msg(..)
    , TinType(..)
    , currentTinType
    , initial
    , setTaxClassificationToCompany
    , setTaxClassificationToIndividual
    , setTinOnTaxClassification
    , setTinTypeOnTaxClassification
    , upsertFieldResponse
    )

import FormModels exposing (..)
import List.Extra as List


type alias Model =
    { taxClassification : TaxClassification
    , customFields : List Field
    , fieldValues : List FieldResponse
    }


type Field
    = TinField
    | IndividualField
    | MultipleChoiceField MultipleChoice


type FieldResponse
    = FieldResponse Field String


type TinType
    = SSN
    | EIN


type EIN_
    = EIN_ String


type SSN_
    = SSN_ String


type TaxClassification
    = Business EIN_
    | Individual SSN_
    | IndividualBusiness EIN_


setTinOnTaxClassification : String -> TaxClassification -> TaxClassification
setTinOnTaxClassification input taxClassification =
    case taxClassification of
        Business (EIN_ _) ->
            Business <| EIN_ input

        Individual (SSN_ _) ->
            Individual <| SSN_ input

        IndividualBusiness (EIN_ _) ->
            IndividualBusiness <| EIN_ input


setTaxClassificationToIndividual : TaxClassification -> TaxClassification
setTaxClassificationToIndividual taxClassification =
    case taxClassification of
        Business (EIN_ tin) ->
            Individual <| SSN_ tin

        IndividualBusiness (EIN_ tin) ->
            Individual <| SSN_ tin

        Individual (SSN_ tin) ->
            Individual <| SSN_ tin


currentTinType : Model -> TinType
currentTinType { taxClassification } =
    case taxClassification of
        Business _ ->
            EIN

        IndividualBusiness _ ->
            EIN

        Individual _ ->
            SSN


setTinTypeOnTaxClassification : TinType -> TaxClassification -> TaxClassification
setTinTypeOnTaxClassification tinType taxClassification =
    case ( taxClassification, tinType ) of
        ( IndividualBusiness (EIN_ tin), EIN ) ->
            IndividualBusiness <| EIN_ tin

        ( IndividualBusiness (EIN_ tin), SSN ) ->
            Individual <| SSN_ tin

        ( Individual (SSN_ tin), EIN ) ->
            IndividualBusiness <| EIN_ tin

        _ ->
            taxClassification


setTaxClassificationToCompany : TaxClassification -> TaxClassification
setTaxClassificationToCompany taxClassification =
    case taxClassification of
        Business (EIN_ tin) ->
            Business <| EIN_ tin

        IndividualBusiness (EIN_ tin) ->
            Business <| EIN_ tin

        Individual (SSN_ tin) ->
            Business <| EIN_ tin


type Msg
    = SetIndividual
    | SetCompany
    | SetTinValue String
    | SetTinType TinType
    | SelectMultipleChoiceField Field String


type alias Flags =
    ()


initial : Model
initial =
    { taxClassification = Business (EIN_ "")
    , customFields =
        [ MultipleChoiceField <| customField (FieldName "payment")
        , MultipleChoiceField <| buildMultipleChoice (FieldName "different") [ "One", "Two", "Three" ]
        ]
    , fieldValues = []
    }


customField : FieldName -> MultipleChoice
customField fieldName =
    buildMultipleChoice fieldName [ "Check", "Wire transfer", "Other" ]


upsertFieldResponse : Field -> String -> List FieldResponse -> List FieldResponse
upsertFieldResponse field string responses =
    case List.find (responseByField field) responses of
        Nothing ->
            responses ++ [ FieldResponse field string ]

        Just _ ->
            List.updateIf (responseByField field) (always <| FieldResponse field string) responses


responseByField : Field -> FieldResponse -> Bool
responseByField field (FieldResponse existingField _) =
    field == existingField
