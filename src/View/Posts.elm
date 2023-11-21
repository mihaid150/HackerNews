module View.Posts exposing (..)

import Html exposing (Html, div, input, option, p, select, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, href, id, placeholder, selected, type_, value)
import Html.Events exposing (onCheck, onInput)
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time exposing (durationBetween)


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable postsConfig currentTime posts =
     div [] [
       table [] [
         thead [] [
           tr [] [
             th [] [text "Score"],
             th [] [text "Title"],
             th [] [text "Type"],
             th [] [text "Posted Date"],
             th [] [text "Link"]
           ]
         ],
         tbody [] (createPostsRows posts currentTime)
       ]
     ]

createPostsRows : List Post -> Time.Posix -> List (Html Msg)
createPostsRows posts currentTime =
    List.map (postRow currentTime) posts

postRow : Time.Posix -> Post -> Html Msg
postRow currentTime post =
    let
        relativeDuration =
            Util.Time.durationBetween post.time currentTime
                |> Maybe.map Util.Time.formatDuration
                |> Maybe.withDefault "just now"
    in
        tr [] [
            td [class "post-score"] [text (String.fromInt post.score)],
            td [class "post-title"] [text post.title],
            td [class "post-type"] [text post.type_],
            td [class "post-time"] [text (Util.Time.formatTime Time.utc post.time ++ " (" ++ relativeDuration ++ ")")],
            td [class "post-url"] [maybeText post.url]
        ]

maybeText: Maybe String -> Html Msg
maybeText maybeValue =
    case maybeValue of
        Just value -> text value
        Nothing -> text ""

{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}
postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    let
        postsToShowOptions = [10, 25, 50]
        sortByOptions = ["Score", "Title", "Posted", "None"]
        postsToShowOptionView number =
            let
                isSelected = number == config.postsToShow
            in
             option [ value (String.fromInt number), selected isSelected ] [text (String.fromInt number)]

        sortByOptionView sortByString =
                    let
                        isSelected = sortToString config.sortBy == sortByString
                    in
                        option [ value sortByString, selected isSelected ] [text sortByString]
    in
        div [] [
                    p [] [text "Choose how many posts to show"],
                    select [ id "select-posts-per-page"
                           , onInput (String.toInt >> Maybe.withDefault 0 >> UpdatePostsToShow >> ConfigChanged) ]
                        (List.map postsToShowOptionView postsToShowOptions),

                    p [] [text "Choose sorting method"],
                    select [ id "select-sort-by"
                           , onInput (sortFromString >> Maybe.withDefault None >> UpdateSortBy >> ConfigChanged) ]
                        (List.map sortByOptionView sortByOptions),

                    p [] [text "Show Job Posts"],
                    input [ type_ "checkbox"
                          , checked config.showJobs
                          , onCheck (UpdateShowJobs >> ConfigChanged)
                          , id "checkbox-show-job-posts"
                          ] [],

                    p [] [text "Show Text Only Posts"],
                    input [ type_ "checkbox"
                          , checked config.showTextOnly
                          , onCheck (UpdateShowTextOnly >> ConfigChanged)
                          , id "checkbox-show-text-only-posts"
                          ] []
                ]