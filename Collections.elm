module Collections exposing (..)

import Array
import Browser
import Date exposing (Date, Unit(..), diff, fromCalendarDate, month, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Html exposing (Html, a, br, button, div, form, h1, h3, hr, img, input, li, option, p, select, span, table, td, text, textarea, th, tr, ul)
import Html.Attributes exposing (action, attribute, checked, href, id, max, method, min, name, placeholder, readonly, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Http
import Json.Decode as D
import Json.Encode as E
import Task exposing (perform)
import Time exposing (Month(..), Weekday(..))



-- MAIN
--main : Program Int Model Msg


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


emptyEntered flags =
    { boxes = 0
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


updateTheRest model =
    case model.flags.branches of
        [] ->
            model

        -- list of 2 or more elements
        _ :: _ :: _ ->
            model

        [ branch ] ->
            updateEnteredBranch model branch


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
    = NoOp
    | TodaysDateReceived Date
    | InitAll DatePicker.Msg
    | PrefillBranch Branch
    | ToDatePicker DatePicker.Msg
    | SetBoxes String
    | SetEntered EnteredBox String
    | SubmitForm
    | SubmittedForm (Result Http.Error String)
    | SelectBranchOption String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
                        | boxes = Maybe.withDefault 0 (String.toInt str)
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

        PrefillBranch branch ->
            ( updateEnteredBranch model branch
            , Cmd.none
            )

        ToDatePicker subMsg ->
            ( updateDatePicker subMsg model, Cmd.none )

        SubmitForm ->
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


modelDebug : Model -> Html Msg
modelDebug model =
    if True then
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


branchOptionString branch =
    branch.name
        ++ " - "
        ++ branch.address
        ++ ", "
        ++ branch.postcode
        ++ " - "
        ++ branch.phone


compareBranchesForSort : Branch -> Branch -> Order
compareBranchesForSort a b =
    compare (branchOptionString a) (branchOptionString b)


viewBranchSelector model =
    case model.flags.branches of
        [] ->
            p [] [ text "Error, we expect to see a branch here." ]

        [ branch ] ->
            p []
                [-- text "will hide branch selector"
                ]

        _ ->
            div []
                [ select [ onInput SelectBranchOption ]
                    ([ option [ value "0" ] [ text "Please select the branch address..." ] ]
                        ++ List.map
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


dateValidation : Model -> String
dateValidation model =
    case model.today of
        Nothing ->
            "We did not expect lack of today's date"

        Just todaysDate ->
            let
                startDate =
                    Date.add Days 1 todaysDate

                endDate =
                    Date.add Days 31 todaysDate
            in
            case model.date of
                Nothing ->
                    "please select date"

                Just pickedDate ->
                    if Date.isBetween startDate endDate pickedDate then
                        "we have valid date"

                    else
                        "Please pick valid date between 1 and 31 days from today"


datePickerWidget model =
    div []
        [ p [] [ text (dateValidation model) ]
        , case model.date of
            Nothing ->
                span [] [ text "Pick collection date" ]

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
        brSpanBr wording =
            span []
                [ br [] []
                , span [] [ text wording ]
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
                , Html.Attributes.min
                    "1"
                , Html.Attributes.max
                    "100"
                , onInput SetBoxes
                ]
                []
            , span [] [ text " boxes " ]
            , br [] []
            , span [] [ text "on" ]
            , br [] []
            , datePickerWidget model
            , br [] []
            , viewBranchSelector model
            , span [] [ text "company name" ]
            , br [] []
            , input
                [ style "min-width" "25em"
                , value
                    model.entered.name
                , onInput (SetEntered Name)
                ]
                []
            , br [] []
            , span [] [ text "collection address" ]
            , br [] []
            , textarea
                [ style "min-width" "35em"
                , style "min-height" "4em"
                , value
                    model.entered.address
                , onInput (SetEntered Address)
                ]
                []
            , brSpanBr "postcode"
            , input [ value model.entered.postcode, onInput (SetEntered Postcode) ] []
            , brSpanBr "contact"
            , input [ value model.entered.contact, onInput (SetEntered Contact) ] []
            , brSpanBr "phone"
            , input [ value model.entered.phone, onInput (SetEntered Phone) ] []
            , br [] []
            , brSpanBr "additional notes"
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


buttonAndSubmission : Model -> Html Msg
buttonAndSubmission model =
    case model.submissionStatus of
        Editing ->
            div []
                [ button [ onClick SubmitForm ] [ text "Submit Collection Request" ]
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
