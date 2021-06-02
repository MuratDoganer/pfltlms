open CoursesReview__Types

let decodeProps = json => {
  open Json.Decode
  (
    field("levels", array(Level.decode), json),
    field("courseId", string, json),
    field("teamCoaches", array(Coach.decode), json),
    field("currentCoach", Coach.decode, json),
    field("teamTags", array(string), json) |> Belt.Set.String.fromArray,
    field("userTags", array(string), json) |> Belt.Set.String.fromArray,
  )
}

let (levels, courseId, teamCoaches, currentCoach, teamTags, userTags) =
  DomUtils.parseJSONAttribute() |> decodeProps

switch ReactDOM.querySelector("#react-root") {
| Some(root) =>
  ReactDOM.render(
    <CoursesReview__Root levels courseId teamCoaches currentCoach teamTags userTags />,
    root,
  )
| None => ()
}
