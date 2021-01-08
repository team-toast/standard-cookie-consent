port module CookieConsent exposing (..)

import Common.Types exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Font
import Helpers.Element as EH exposing (DisplayProfile, responsiveVal)
import Maybe.Extra


type Msg userMsg
    = CookieConsentGranted (Maybe (Cmd userMsg))


type alias Theme userMsg =
    { backgroundColor : Element.Color
    , fontColor : Element.Color
    , linkFontColor : Element.Color
    , button : EH.DisplayProfile -> List (Attribute (Msg userMsg)) -> List String -> Msg userMsg -> Element (Msg userMsg)
    }


defaultTheme : Theme userMsg
defaultTheme =
    { backgroundColor = Element.rgb255 7 27 92
    , fontColor = EH.white
    , linkFontColor = Element.rgb 0 0 1
    , button = blueButton
    }


viewModal : DisplayProfile -> Maybe (Theme userMsg) -> Maybe (Cmd userMsg) -> Element (Msg userMsg)
viewModal dProfile maybeTheme maybeCmdOnAccept =
    let
        theme : Theme userMsg
        theme =
            maybeTheme |> Maybe.withDefault defaultTheme
    in
    Element.row
        [ Element.alignBottom
        , responsiveVal dProfile Element.centerX (Element.width Element.fill)
        , Element.Border.roundEach
            { topLeft = 5
            , topRight = 5
            , bottomLeft = 0
            , bottomRight = 0
            }
        , Element.padding 15
        , Element.spacing 15
        , Element.Background.color theme.backgroundColor
        , Element.Font.color theme.fontColor
        , Element.Border.glow
            (Element.rgba 0 0 0 0.2)
            10
        ]
        [ Element.paragraph
            [ Element.width <| responsiveVal dProfile (Element.px 800) Element.fill
            , Element.Font.size <| responsiveVal dProfile 20 12
            ]
            [ Element.text "Foundry products use cookies and analytics to track behavior patterns, to help zero in on effective marketing strategies. To avoid being tracked in this way, we recommend using the "
            , Element.newTabLink
                [ Element.Font.color theme.linkFontColor ]
                { url = "https://brave.com/"
                , label = Element.text "Brave browser"
                }
            , Element.text " or installing the "
            , Element.newTabLink
                [ Element.Font.color theme.linkFontColor ]
                { url = "https://tools.google.com/dlpage/gaoptout"
                , label = Element.text "Google Analytics Opt-Out browser addon"
                }
            , Element.text "."
            ]
        , theme.button dProfile [ Element.alignTop ] [ "Understood" ] (CookieConsentGranted maybeCmdOnAccept)
        ]


blueButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> msg -> Element msg
blueButton dProfile attributes text msg =
    EH.button dProfile
        attributes
        ( Element.rgba 0 0 1 1
        , Element.rgba 0 0 1 0.8
        , Element.rgba 0 0 1 0.6
        )
        EH.white
        text
        msg


handleMsg : Msg userMsg -> Cmd userMsg
handleMsg msg =
    case msg of
        CookieConsentGranted maybeUserCmd ->
            Cmd.batch <|
                Maybe.Extra.values
                    [ Just <| consentToCookies ()
                    , maybeUserCmd
                    ]


port consentToCookies : () -> Cmd msg
