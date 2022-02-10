port module Main exposing (Model)

import Array exposing (Array)
import Browser
import Browser.Dom
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Html.Keyed
import Json.Decode as Decode
import Task
import Time exposing (Posix, millisToPosix)


type Scene
    = Home
    | Practice
    | About
    | Game
    | GameMiss
    | GameOver


type alias ViewPort =
    { width : Float
    , height : Float
    }


type alias Point =
    ( Int, Int )


type alias Grid =
    Array (Array Tile)


type Tile
    = None
    | Horizonal
    | Vertical
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight
    | EndTop
    | EndLeft
    | EndRight
    | EndBottom
    | StartTop
    | StartLeft
    | StartRight
    | StartBottom


port requestGrid : Int -> Cmd msg


port gridReceiver : (GridAndMatch -> msg) -> Sub msg


type alias Model =
    { scene : Scene
    , touch : Bool
    , level : Int
    , viewPort : ViewPort
    , startTime : Posix
    , endTime : Posix
    , practiceMiss : Bool
    , grid : Array (Array Tile)
    , match : Point
    , loading : Bool
    }


init : Int -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { scene = Home
            , touch = False
            , level = 0
            , viewPort = { width = 0.0, height = 0.0 }
            , startTime = millisToPosix 0
            , endTime = millisToPosix 0
            , practiceMiss = False
            , grid = Array.fromList [ Array.fromList [ TopRight ] ]
            , match = ( 0, 0 )
            , loading = False
            }
    in
    ( model, Task.perform InitializeViewPort Browser.Dom.getViewport )


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        defaultSubs =
            Sub.batch
                [ Browser.Events.onResize UpdateViewPort
                , gridReceiver ReceiveGrid
                ]
    in
    defaultSubs


parseGrid : Array (Array String) -> Array (Array Tile)
parseGrid grid =
    Array.map (\row -> row |> Array.map parseTile) grid


parseTile : String -> Tile
parseTile str =
    case str of
        "right-right" ->
            Horizonal

        "left-left" ->
            Horizonal

        "left-right" ->
            Horizonal

        "right-left" ->
            Horizonal

        "top-top" ->
            Vertical

        "bottom-bottom" ->
            Vertical

        "top-bottom" ->
            Vertical

        "bottom-top" ->
            Vertical

        "top-right" ->
            TopRight

        "right-top" ->
            TopRight

        "top-left" ->
            TopLeft

        "left-top" ->
            TopLeft

        "bottom-left" ->
            BottomLeft

        "left-bottom" ->
            BottomLeft

        "bottom-right" ->
            BottomRight

        "right-bottom" ->
            BottomRight

        "start-left" ->
            StartLeft

        "start-right" ->
            StartRight

        "start-top" ->
            StartTop

        "start-bottom" ->
            StartBottom

        "end-top" ->
            EndTop

        "end-bottom" ->
            EndBottom

        "end-left" ->
            EndLeft

        "end-right" ->
            EndRight

        "none" ->
            None

        _ ->
            None


type alias GridAndMatch =
    { grid : Array (Array String)
    , match : Point
    }


type Msg
    = ShowHome
    | ShowAbout
    | ShowPractice Bool
    | HandlePractice Bool
    | StartTimer Posix
    | HandleTilePick ( Int, Int )
    | EndGame Posix
    | TransitionToGameOver
    | ReceiveGrid GridAndMatch
    | InitializeViewPort Browser.Dom.Viewport
    | UpdateViewPort Int Int


startPathLength : number
startPathLength =
    5


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowHome ->
            ( { model | scene = Home }, Cmd.none )

        ShowAbout ->
            ( { model | scene = About }, Cmd.none )

        ShowPractice touch ->
            ( { model | scene = Practice, practiceMiss = False, touch = model.touch || touch }, Cmd.none )

        HandlePractice success ->
            if success then
                ( { model | scene = Game, level = 0, loading = True }, Cmd.batch [ requestGrid startPathLength, Task.perform StartTimer Time.now ] )

            else if model.practiceMiss then
                ( { model | scene = Home }, Cmd.none )

            else
                ( { model | practiceMiss = True }, Cmd.none )

        StartTimer start ->
            ( { model | startTime = start }, Cmd.none )

        HandleTilePick ( colIndex, rowIndex ) ->
            let
                ( col, row ) =
                    model.match

                correct =
                    col == colIndex && row == rowIndex
            in
            if correct then
                ( { model | level = model.level + 1, loading = True }, requestGrid (model.level + startPathLength) )

            else
                ( model, Task.perform EndGame Time.now )

        EndGame end ->
            ( { model | scene = GameMiss, endTime = end }, Cmd.none )

        TransitionToGameOver ->
            ( { model | scene = GameOver }, Cmd.none )

        ReceiveGrid gen ->
            ( { model | grid = parseGrid gen.grid, match = gen.match, loading = False }, Cmd.none )

        InitializeViewPort viewPort ->
            ( { model
                | viewPort =
                    { width = viewPort.viewport.width
                    , height = viewPort.viewport.height
                    }
              }
            , Cmd.none
            )

        UpdateViewPort width height ->
            ( { model | viewPort = { width = toFloat width, height = toFloat height } }, Cmd.none )


px : Float -> String
px num =
    String.fromFloat num ++ "px"


timeDifference : Posix -> Posix -> Int
timeDifference start finish =
    let
        startMs =
            Time.posixToMillis start

        finishMs =
            Time.posixToMillis finish
    in
    finishMs - startMs


timeInSeconds : Posix -> Posix -> String
timeInSeconds start finish =
    let
        duration =
            timeDifference start finish

        seconds =
            duration // 1000
    in
    String.fromInt seconds


tileClass : Tile -> String
tileClass tile =
    case tile of
        None ->
            "tile--none"

        Horizonal ->
            "tile--horizontal"

        Vertical ->
            "tile--vertical"

        TopLeft ->
            "tile--top-left"

        TopRight ->
            "tile--top-right"

        BottomLeft ->
            "tile--bottom-left"

        BottomRight ->
            "tile--bottom-right"

        EndTop ->
            "tile--end-top"

        EndLeft ->
            "tile--end-left"

        EndRight ->
            "tile--end-right"

        EndBottom ->
            "tile--end-bottom"

        StartTop ->
            "tile--start-top"

        StartLeft ->
            "tile--start-left"

        StartRight ->
            "tile--start-right"

        StartBottom ->
            "tile--start-bottom"


tileSize : Int -> Int -> ViewPort -> Float
tileSize width height viewPort =
    let
        tileWidth =
            viewPort.width / toFloat width

        tileHeight =
            viewPort.height / toFloat height

        maxSize =
            min tileWidth tileHeight

        damppening =
            0.8
    in
    maxSize * damppening


renderTileGrid : Grid -> Int -> ViewPort -> (Tile -> ( Int, Int ) -> String -> Html Msg) -> Html Msg
renderTileGrid grid level viewPort fn =
    let
        gridWidth =
            grid |> Array.get 0 |> Maybe.withDefault Array.empty |> Array.length

        gridHeight =
            grid |> Array.length

        size =
            tileSize gridWidth gridHeight viewPort |> px
    in
    Html.Keyed.node "div"
        []
        [ ( level |> String.fromInt
          , div [ class "grid" ]
                (grid
                    |> Array.indexedMap
                        (\rowIndex row ->
                            div [ class "row" ]
                                (row
                                    |> Array.indexedMap
                                        (\colIndex v ->
                                            if v == None then
                                                div
                                                    [ class "tile tile--none"
                                                    , style "width" size
                                                    , style "height" size
                                                    ]
                                                    []

                                            else
                                                fn v ( colIndex, rowIndex ) size
                                        )
                                    |> Array.toList
                                )
                        )
                    |> Array.toList
                )
          )
        ]


view : Model -> Html Msg
view model =
    case model.scene of
        Home ->
            introLevel model

        Practice ->
            practiceLevel model

        About ->
            aboutView model

        Game ->
            div [ class "game" ]
                [ div [ class "arena" ]
                    [ div [ class "watermark" ] [ text "3/10" ]
                    , if model.loading then
                        text "Loading..."

                      else
                        let
                            gridWidth =
                                model.grid |> Array.get 0 |> Maybe.withDefault Array.empty |> Array.length

                            gridHeight =
                                model.grid |> Array.length

                            size =
                                tileSize gridWidth gridHeight model.viewPort |> px
                        in
                        Html.Keyed.node "div"
                            []
                            [ ( model.level |> String.fromInt
                              , div [ class "grid" ]
                                    (model.grid
                                        |> Array.indexedMap
                                            (\rowIndex row ->
                                                div [ class "row" ]
                                                    (row
                                                        |> Array.indexedMap
                                                            (\colIndex v ->
                                                                if v == None then
                                                                    div
                                                                        [ class "tile tile--none"
                                                                        , style "width" size
                                                                        , style "height" size
                                                                        ]
                                                                        []

                                                                else
                                                                    div
                                                                        [ class "tile"
                                                                        , class (tileClass v)
                                                                        , style "width" size
                                                                        , style "height" size
                                                                        , if model.touch then
                                                                            onTouch (HandleTilePick ( colIndex, rowIndex ))

                                                                          else
                                                                            onClick (HandleTilePick ( colIndex, rowIndex ))
                                                                        ]
                                                                        [ div [ class "tile-outer" ] []
                                                                        , div [ class "tile-inner" ] []
                                                                        ]
                                                            )
                                                        |> Array.toList
                                                    )
                                            )
                                        |> Array.toList
                                    )
                              )
                            ]
                    ]
                ]

        GameMiss ->
            div
                [ class "game game-miss"
                , if model.touch then
                    onTouch TransitionToGameOver

                  else
                    onClick TransitionToGameOver
                ]
                [ div [ class "arena" ]
                    [ div [ class "watermark" ] [ text "3/10" ]
                    , renderTileGrid model.grid
                        model.level
                        model.viewPort
                        (\tile point size ->
                            div
                                (if point == model.match then
                                    [ class "tile"
                                    , class (tileClass tile)
                                    , class "tile--match"
                                    , style "width" size
                                    , style "height" size
                                    ]

                                 else
                                    [ class "tile"
                                    , class (tileClass tile)
                                    , style "width" size
                                    , style "height" size
                                    ]
                                )
                                [ div [ class "tile-outer" ] []
                                , div [ class "tile-inner" ] []
                                ]
                        )
                    ]
                ]

        GameOver ->
            let
                seconds =
                    timeInSeconds model.startTime model.endTime

                secondsUnit =
                    if seconds == "1" then
                        "second"

                    else
                        "seconds"

                rounds =
                    String.fromInt model.level

                integerUnit =
                    if model.level == 1 then
                        "level"

                    else
                        "levels"
            in
            div [ class "game-over" ]
                [ div [ class "overlay" ]
                    [ div
                        [ class "overlay-content home" ]
                        [ h1 [ class "score" ]
                            [ text ("You made it through " ++ rounds ++ " " ++ integerUnit)
                            , br [] []
                            , text ("in " ++ seconds ++ " " ++ secondsUnit ++ ".")
                            ]
                        , p [ class "button-set" ]
                            [ button
                                [ class "button button-secondary"
                                , if model.touch then
                                    onTouch ShowHome

                                  else
                                    onClick ShowHome
                                ]
                                [ text "Return home" ]
                            ]
                        ]
                    ]
                ]


introLevel : Model -> Html Msg
introLevel _ =
    div [ class "home" ]
        [ h1 [ class "hero" ] [ text "Three Tenths" ]
        , div [ class "ruler" ]
            [ div [ class "ruler-tip-left" ] []
            , div
                [ class "ruler-28"
                ]
                []
            , div
                [ class "ruler-4 hit-point-visible"
                , onClick (ShowPractice False)
                , onTouch (ShowPractice True)
                ]
                []
            , div [ class "ruler-68" ]
                [ div [ class "ruler-hint" ] [ text "← Let's go!" ]
                ]
            ]
        , div [ class "footer" ]
            [ p [ class "button-set" ]
                [ button [ class "button button-secondary", onClick ShowAbout ] [ text "What is this?" ]
                ]
            ]
        ]


practiceLevel : Model -> Html Msg
practiceLevel model =
    let
        handleHit =
            if model.touch then
                onTouch (HandlePractice True)

            else
                onClick (HandlePractice True)

        handleMiss =
            if model.touch then
                onTouch (HandlePractice False)

            else
                onClick (HandlePractice False)
    in
    div [ class "home practice" ]
        [ h1 [ class "practice-title" ]
            [ text
                (if model.practiceMiss then
                    "Well, you tried..."

                 else
                    "One quick warm up..."
                )
            ]
        , div [ class "ruler" ]
            [ div
                [ class "ruler-64", handleMiss ]
                []
            , div
                (if model.practiceMiss then
                    [ class "ruler-12", class "hit-point-hint", handleHit ]

                 else
                    [ class "ruler-12", handleHit ]
                )
                []
            , div
                [ class "ruler-24", handleMiss ]
                []
            , div [ class "ruler-tip-right" ] []
            ]
        , div [ class "footer" ]
            [ p [ class "practice-hint" ]
                [ text "This game isn't called seven tenths! The path starts from these marks ↗" ]
            ]
        ]


onTouch : msg -> Attribute msg
onTouch message =
    Html.Events.on "touchstart" (Decode.succeed message)


aboutView : Model -> Html Msg
aboutView _ =
    div [ class "info overlay" ]
        [ header [ onClick ShowHome ] [ text "← Back to game" ]
        , article []
            [ h1 [] [ text "Three Tenths" ]
            , h2 []
                [ text
                    "A game of precision where you must strike at exactly three tenths (3/10, 0.3, 3:7) along a given path. As you progress the path gets longer, twistier, and requires more precision to land a perfect strike. (Inspired by "
                , a [ href "https://jujutsu-kaisen.fandom.com/wiki/Kento_Nanami" ] [ text "Jujutsu Kaisen" ]
                , text ")"
                ]
            , p []
                [ text "Three Tenths is written by "
                , a [ href "https://jew.ski/" ] [ text "Chris Andrejewski" ]
                , text ". The source code is "
                , a [ href "https://github.com/andrejewski/three-tenths" ] [ text "open source" ]
                , text "."
                ]
            ]
        ]


main : Program Int Model Msg
main =
    Browser.element
        { init = init, subscriptions = subscriptions, update = update, view = view }
