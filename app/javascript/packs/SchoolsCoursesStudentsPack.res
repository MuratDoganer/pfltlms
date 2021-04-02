open StudentsEditor__Types

let decodeProps = json => {
  open Json.Decode
  (
    json |> field("courseId", string),
    json |> field("courseCoachIds", array(string)),
    json |> field("schoolCoaches", array(Coach.decode)),
    json |> field("levels", array(Level.decode)),
    json |> field("studentTags", array(string)),
    json |> field("certificates", array(Certificate.decode)),
    json |> field("currentUserName", string),
  )
}

let (courseId, courseCoachIds, schoolCoaches, levels, studentTags, certificates, currentUserName) =
  DomUtils.parseJSONTag(~id="sa-students-panel-data", ()) |> decodeProps

switch ReactDOM.querySelector("#sa-students-panel") {
| Some(element) =>
  ReactDOM.render(
    <StudentsEditor__Root
      courseId courseCoachIds schoolCoaches levels studentTags certificates currentUserName
    />,
    element,
  )
| None => ()
}
