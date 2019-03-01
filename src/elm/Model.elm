module Model exposing
    ( Field(..)
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
    )


type alias Model =
    { taxClassification : TaxClassification }


type Field
    = TinField
    | IndividualField


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


type alias Flags =
    ()


initial : Model
initial =
    { taxClassification = Business (EIN_ "") }
