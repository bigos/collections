module Collections exposing (..)

import Browser
import Date exposing (Date, Unit(..), fromCalendarDate, weekday)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Html exposing (Html, br, button, div, img, input, option, p, select, span, text, textarea)
import Html.Attributes exposing (src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import Task
import Time exposing (Month(..), Weekday(..))



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


settings : DatePicker.Settings
settings =
    let
        isDisabled date =
            --  here we can disable weekend dates [Sat, Sun]
            [ Sun ]
                |> List.member (weekday date)
    in
    { defaultSettings | isDisabled = isDisabled }



-- MODEL


type alias Flags =
    { user_full_name : String
    , user_email : String
    , contact_id : Int
    , branch_id : Int
    , branches : List Branch
    , status : String
    }


type alias Branch =
    { branch_id : Int
    , name : String
    , address : String
    , postcode : String
    , phone : String
    }


type alias Entered =
    { boxes : Int
    , date : String
    , address : String
    , postcode : String
    , name : String
    , contact : String
    , phone : String
    , notes : String
    }


emptyEntered : Flags -> Entered
emptyEntered flags =
    { boxes = 1
    , date = ""
    , address = ""
    , postcode = ""
    , name = ""
    , contact = flags.user_full_name
    , phone = ""
    , notes = ""
    }


type SubmissionStatus
    = Editing
    | Submission
    | SubmissionSuccess
    | SubmissionFailure Http.Error


type alias Model =
    { flags : Flags
    , entered : Entered
    , today : Maybe Date
    , date : Maybe Date
    , datePicker : DatePicker.DatePicker
    , submissionStatus : SubmissionStatus
    }


init : D.Value -> ( Model, Cmd Msg )
init flags_json =
    let
        parsed_flags =
            case D.decodeValue flagsDecoder flags_json of
                Err err ->
                    Debug.log (Debug.toString err)
                        { user_full_name = "Flags parse error"
                        , user_email = "Flags parse error"
                        , contact_id = 0
                        , branch_id = 0
                        , branches = []
                        , status = "Flags parse error"
                        }

                Ok flags ->
                    flags

        ( datePicker, datePickerCmd ) =
            DatePicker.init
    in
    ( { flags = parsed_flags
      , entered = emptyEntered parsed_flags
      , today = Nothing
      , date = Nothing
      , datePicker = datePicker
      , submissionStatus = Editing
      }
    , Cmd.map InitAll datePickerCmd
    )


dateFormat : String
dateFormat =
    "E, d/MMM/yyyy"



-- UPDATE


type EnteredBox
    = Name
    | Address
    | Postcode
    | Notes
    | Contact
    | Phone


resetEnteredBranch : Model -> Model
resetEnteredBranch model =
    { model
        | entered =
            { boxes = model.entered.boxes
            , date = model.entered.date
            , address = ""
            , postcode = ""
            , name = ""
            , contact = model.entered.contact
            , phone = ""
            , notes = model.entered.notes
            }
    }


updateEnteredBranch : Model -> Branch -> Model
updateEnteredBranch model branch =
    { model
        | entered =
            { boxes = model.entered.boxes
            , date = model.entered.date
            , address = branch.address
            , postcode = branch.postcode
            , name = branch.name
            , contact = model.entered.contact
            , phone = branch.phone
            , notes = model.entered.notes
            }
    }


updateTheRest : Model -> Model
updateTheRest model =
    case model.flags.branches of
        [] ->
            model

        -- list of 2 or more elements
        _ :: _ :: _ ->
            model

        [ branch ] ->
            updateEnteredBranch model branch


updateDatePicker : DatePicker.Msg -> Model -> Model
updateDatePicker datePickerCmd model =
    let
        ( newDatePicker, dateEvent ) =
            DatePicker.update settings datePickerCmd model.datePicker

        newDate =
            case dateEvent of
                Picked changedDate ->
                    Just changedDate

                _ ->
                    model.date

        newEntered =
            model.entered
    in
    { model
        | date = newDate
        , datePicker = newDatePicker
        , entered =
            { newEntered
                | date =
                    Date.format dateFormat
                        (Maybe.withDefault
                            (fromCalendarDate 1970 Jan 1)
                            newDate
                        )
            }
    }


type Msg
    = TodaysDateReceived Date
    | InitAll DatePicker.Msg
    | ToDatePicker DatePicker.Msg
    | SetBoxes String
    | SetEntered EnteredBox String
    | PressedSubmit
    | SubmittedForm (Result Http.Error String)
    | SelectBranchOption String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitAll datePickerCmd ->
            ( -- update model at multiple stages
              model |> updateDatePicker datePickerCmd |> updateTheRest
            , Date.today |> Task.perform TodaysDateReceived
            )

        TodaysDateReceived date ->
            ( { model | today = Just date }
            , Cmd.none
            )

        SetBoxes str ->
            let
                newEntered =
                    model.entered
            in
            ( { model
                | entered =
                    { newEntered
                        | boxes = Maybe.withDefault 1 (String.toInt str)
                    }
              }
            , Cmd.none
            )

        SetEntered box str ->
            let
                newEntered =
                    model.entered
            in
            ( { model
                | entered =
                    case box of
                        Name ->
                            { newEntered | name = str }

                        Address ->
                            { newEntered | address = str }

                        Postcode ->
                            { newEntered
                                | postcode = str
                            }

                        Contact ->
                            { newEntered
                                | contact = str
                            }

                        Phone ->
                            { newEntered
                                | phone = str
                            }

                        Notes ->
                            { newEntered
                                | notes = str
                            }
              }
            , Cmd.none
            )

        ToDatePicker subMsg ->
            ( updateDatePicker subMsg model, Cmd.none )

        PressedSubmit ->
            ( { model | submissionStatus = Submission }, submitForm model )

        SubmittedForm result ->
            Debug.log ("submitted " ++ Debug.toString result)
                (case result of
                    Ok r ->
                        Debug.log ("submited " ++ Debug.toString r)
                            ( { model | submissionStatus = SubmissionSuccess }, Cmd.none )

                    Err string ->
                        Debug.log ("submited " ++ Debug.toString string)
                            ( { model | submissionStatus = SubmissionFailure string }, Cmd.none )
                )

        SelectBranchOption branch_id ->
            Debug.log (Debug.toString ( "selecting branch", branch_id ))
                (case
                    List.head
                        (List.filter (\branch -> branch_id == String.fromInt branch.branch_id)
                            model.flags.branches
                        )
                 of
                    Just branch ->
                        Debug.log (Debug.toString ( "selected branch", branch_id, branch ))
                            ( updateEnteredBranch model branch, Cmd.none )

                    Nothing ->
                        ( resetEnteredBranch model, Cmd.none )
                )



-- end of update


submitForm : Model -> Cmd Msg
submitForm model =
    Http.post
        { url = "http://localhost:3000/collections/submit"
        , body = Http.jsonBody (formDataEncoder model)
        , expect = Http.expectString SubmittedForm
        }


formDataEncoder : Model -> E.Value
formDataEncoder model =
    let
        e =
            model.entered
    in
    E.object
        [ ( "contact_id", E.int model.flags.contact_id )
        , ( "branch_id", E.int model.flags.branch_id )
        , ( "boxes", E.int e.boxes )
        , ( "date", E.string e.date )
        , ( "address", E.string e.address )
        , ( "postcode", E.string e.postcode )
        , ( "name", E.string e.name )
        , ( "contact", E.string e.contact )
        , ( "phone", E.string e.phone )
        , ( "notes", E.string e.notes )
        , ( "submitter_name", E.string model.flags.user_full_name )
        , ( "submitter_email", E.string model.flags.user_email )
        ]



-- DECODERS


flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.map6 Flags
        (D.field "user_full_name" D.string)
        (D.field "user_email" D.string)
        (D.field "contact_id" D.int)
        (D.field "branch_id" D.int)
        (D.field "branches" branchesDecoder)
        (D.field "status" D.string)


branchesDecoder : D.Decoder (List Branch)
branchesDecoder =
    D.list branchDecoder


branchDecoder : D.Decoder Branch
branchDecoder =
    D.map5 Branch
        (D.field "branch_id" D.int)
        (D.field "name" D.string)
        (D.field "address" D.string)
        (D.field "postcode" (D.oneOf [ D.string, D.null "" ]))
        (D.field "phone" D.string)



-- VIEW


boxCollectionLimit : Int
boxCollectionLimit =
    9


largeOrderWarning : Model -> Html Msg
largeOrderWarning model =
    if model.entered.boxes > 5 then
        span
            [ style "background" "yellow"
            , style "padding" "0.25em"
            , style "margin-left" "2em"
            ]
            [ text
                ("For large orders, "
                    ++ String.fromInt (boxCollectionLimit + 1)
                    ++ " or more, please contact our Customer Service"
                )
            ]

    else
        span [] []


modelDebug : Model -> Html Msg
modelDebug model =
    let
        debugMe =
            False
    in
    if debugMe then
        p [ style "background" "yellow" ]
            [ text
                ("model debug will go here"
                    ++ Debug.toString model
                )
            ]

    else
        span [] []


badFlagsWarning : Model -> Html Msg
badFlagsWarning model =
    if model.flags.status /= "Ok" then
        p
            [ style "background" "red"
            , style "padding" "0.5em"
            , style "color" "white"
            ]
            [ text "Invalid data detected. Please contact our support with your login information." ]

    else
        span [] []


branchOptionString : Branch -> String
branchOptionString branch =
    String.concat
        [ branch.name
        , " - "
        , branch.address
        , ", "
        , branch.postcode
        , " - "
        , branch.phone
        ]


compareBranchesForSort : Branch -> Branch -> Order
compareBranchesForSort a b =
    compare (branchOptionString a) (branchOptionString b)


viewBranchSelector : { a | flags : { b | branches : List Branch } } -> Html Msg
viewBranchSelector model =
    case model.flags.branches of
        [] ->
            p [] [ text "Error, we expect to see a branch here." ]

        [ _ ] ->
            p []
                [-- text "will hide branch selector"
                ]

        _ ->
            div []
                [ select [ onInput SelectBranchOption ]
                    (option [ value "0" ] [ text "Please select the collection address..." ]
                        :: List.map
                            (\branch ->
                                option
                                    [ value <| String.fromInt branch.branch_id
                                    ]
                                    [ text <| branchOptionString branch
                                    ]
                            )
                            (List.sortWith compareBranchesForSort model.flags.branches)
                    )
                ]



--isDateValid : Model -> Bool


isDateValid model =
    case model.today of
        Nothing ->
            False

        Just todaysDate ->
            let
                startDate =
                    Date.add Days 1 todaysDate

                endDate =
                    -- in two weeks
                    Date.add Days (1 + 14) todaysDate
            in
            case model.date of
                Nothing ->
                    False

                Just pickedDate ->
                    Date.isBetween startDate endDate pickedDate


validDateRange : Model -> String
validDateRange model =
    case model.today of
        Nothing ->
            ""

        Just todaysDate ->
            let
                startDate =
                    Date.add Days 1 todaysDate

                endDate =
                    -- in two weeks
                    Date.add Days (1 + 14) todaysDate
            in
            case model.date of
                Nothing ->
                    ""

                Just pickedDate ->
                    if Date.isBetween startDate endDate pickedDate then
                        ""

                    else
                        "Please pick a date between "
                            ++ Date.format dateFormat startDate
                            ++ " and "
                            ++ Date.format dateFormat endDate


redAsterix : Html Msg
redAsterix =
    span [ style "color" "red" ] [ text " *" ]


datePickerWidget : Model -> Html Msg
datePickerWidget model =
    div []
        [ if isDateValid model then
            p [] []

          else
            case model.date of
                Nothing ->
                    p [] []

                Just _ ->
                    p []
                        [ span [ style "background" "yellow", style "padding" "0.25em" ]
                            [ text
                                (validDateRange model)
                            ]
                        ]
        , case model.date of
            Nothing ->
                span [] [ text "Collection Date", redAsterix ]

            Just date ->
                span [] [ text <| Date.format dateFormat date ]
        , DatePicker.view model.date settings model.datePicker
            |> Html.map ToDatePicker
        ]


view : Model -> Html Msg
view model =
    div []
        [ modelDebug model
        , if model.flags.status == "Ok" then
            viewValid model

          else
            p
                [ style "background" "red"
                , style "padding" "0.5em"
                , style "color" "white"
                ]
                [ text "Invalid data detected. Please contact IT with your login information." ]
        ]


viewValid : Model -> Html Msg
viewValid model =
    let
        brSpanBr : String -> Bool -> Html Msg
        brSpanBr wording required =
            span []
                [ br [] []
                , span []
                    [ text wording
                    , if required then
                        redAsterix

                      else
                        text ""
                    ]
                , br [] []
                ]
    in
    div []
        [ div
            [ style "display"
                (case model.submissionStatus of
                    Editing ->
                        "block"

                    _ ->
                        "none"
                )
            ]
            [ span [] [ text "Please collect " ]
            , br [] []
            , input
                [ type_ "number"
                , value (String.fromInt model.entered.boxes)
                , Html.Attributes.min
                    "1"
                , Html.Attributes.max
                    (String.fromInt boxCollectionLimit)
                , onInput SetBoxes
                ]
                []
            , span []
                [ text
                    (" "
                        ++ (if model.entered.boxes == 1 then
                                "box"

                            else
                                "boxes"
                           )
                    )
                ]
            , largeOrderWarning model
            , br [] []
            , span [] [ text "on" ]
            , br [] []
            , datePickerWidget model
            , br [] []
            , viewBranchSelector model
            , span [] [ text "Company Name" ]
            , br [] []
            , input
                [ style "min-width" "25em"
                , value
                    model.entered.name
                , onInput (SetEntered Name)
                ]
                []
            , br [] []
            , span [] [ text "Collection Address", redAsterix ]
            , br [] []
            , textarea
                [ style "min-width" "35em"
                , style "min-height" "4em"
                , value
                    model.entered.address
                , onInput (SetEntered Address)
                ]
                []
            , brSpanBr "Postcode" True
            , input [ value model.entered.postcode, onInput (SetEntered Postcode) ] []
            , brSpanBr "Contact" True
            , input [ value model.entered.contact, onInput (SetEntered Contact) ] []
            , brSpanBr "Phone" True
            , input [ value model.entered.phone, onInput (SetEntered Phone) ] []
            , br [] []
            , brSpanBr "Additional Notes" False
            , textarea
                [ style "min-width" "35em"
                , style "min-height" "4em"
                , onInput (SetEntered Notes)
                ]
                []
            , br [] []
            ]
        , buttonAndSubmission model
        ]



-- mandatory:  collection date, address, postcode, contact, phone


missingReqiredCheck : Model -> List String
missingReqiredCheck model =
    let
        needed =
            [ ( model.date == Nothing, "Collection Date" )
            , ( model.entered.address == "", "Collection Address" )
            , ( model.entered.postcode == "", "Postcode" )
            , ( model.entered.contact == "", "Contact" )
            , ( model.entered.phone == "", "Phone" )
            ]
    in
    List.foldr
        (\t a ->
            if Tuple.first t then
                Tuple.second t :: a

            else
                a
        )
        []
        needed


buttonAndSubmission : Model -> Html Msg
buttonAndSubmission model =
    let
        missing =
            missingReqiredCheck model

        dateValid =
            isDateValid model
    in
    case model.submissionStatus of
        Editing ->
            div []
                [ if List.isEmpty missing && dateValid then
                    button [ onClick PressedSubmit ] [ text "Submit Collection Request" ]

                  else
                    p [ style "background" "red", style "color" "white", style "padding" "1em" ]
                        [ text
                            ((if List.isEmpty missing then
                                ""

                              else
                                "The following required fields "
                                    ++ (if List.length missing == 1 then
                                            "is"

                                        else
                                            "are"
                                       )
                                    ++ " missing "
                                    ++ List.foldl (\e a -> a ++ e ++ ", ") "" missing
                             )
                                ++ (case model.date of
                                        Nothing ->
                                            ""

                                        Just _ ->
                                            if dateValid then
                                                ""

                                            else
                                                "The date must be in valid range "
                                   )
                            )
                        ]
                ]

        Submission ->
            div []
                [ p [] [ text "Submission in progress..." ]
                , img [ src "/images/ajax-loader.gif" ] []
                ]

        SubmissionSuccess ->
            div [] [ text "Order has been submitted" ]

        SubmissionFailure err ->
            div [] [ text ("Submission failure" ++ " " ++ Debug.toString err) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
