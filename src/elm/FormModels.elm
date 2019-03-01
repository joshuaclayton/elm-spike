module FormModels exposing
    ( FieldName(..)
    , MultipleChoice(..)
    , Option(..)
    , buildMultipleChoice
    , multipleChoiceOptions
    , optionValue
    )


type Option
    = Option String


type MultipleChoice
    = MultipleChoice FieldName (List Option)


multipleChoiceOptions : MultipleChoice -> List Option
multipleChoiceOptions (MultipleChoice _ options) =
    options


optionValue : Option -> String
optionValue (Option value) =
    value


type FieldName
    = FieldName String


buildMultipleChoice : FieldName -> List String -> MultipleChoice
buildMultipleChoice fieldName =
    MultipleChoice fieldName << List.map Option
