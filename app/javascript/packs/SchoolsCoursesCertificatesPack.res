open CourseCertificates__Types

let decodeProps = json => {
  open Json.Decode
  (
    field("course", Course.decode, json),
    field("certificates", array(Certificate.decode), json),
    field("verifyImageUrl", string, json),
    field("canBeAutoIssued", bool, json),
  )
}

let (course, certificates, verifyImageUrl, canBeAutoIssued) =
  DomUtils.parseJSONTag(~id="schools-courses-certificates__props", ()) |> decodeProps

switch ReactDOM.querySelector("#schools-courses-certificates__root") {
| Some(root) =>
  ReactDOM.render(
    <CourseCertificates__Root course certificates verifyImageUrl canBeAutoIssued />,
    root,
  )
| None => ()
}
