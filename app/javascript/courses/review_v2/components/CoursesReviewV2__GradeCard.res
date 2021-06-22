%bs.raw(`require("./CoursesReview__GradeCard.css")`)

open CoursesReview__Types
let str = React.string

type status =
  | Graded(bool)
  | Grading
  | Ungraded

type editor =
  | GradesEditor
  | ChecklistEditor
  | NotesEditor

type state = {
  grades: array<Grade.t>,
  newFeedback: string,
  saving: bool,
  checklist: array<SubmissionChecklistItem.t>,
  note: option<string>,
  editor: editor,
}

type action =
  | BeginSaving
  | FinishSaving
  | UpdateFeedback(string)
  | GenerateFeeback(string)
  | UpdateGrades(array<Grade.t>)
  | UpdateChecklist(array<SubmissionChecklistItem.t>)
  | UpdateNote(string)
  | ShowGradesEditor
  | ShowChecklistEditor
  | ShowNotesEditor

let reducer = (state, action) =>
  switch action {
  | BeginSaving => {...state, saving: true}
  | FinishSaving => {...state, saving: false}
  | UpdateFeedback(newFeedback) => {...state, newFeedback: newFeedback}
  | GenerateFeeback(newFeedback) => {...state, newFeedback: newFeedback, editor: GradesEditor}
  | UpdateGrades(grades) => {...state, grades: grades}
  | UpdateChecklist(checklist) => {...state, checklist: checklist}
  | UpdateNote(note) => {...state, note: Some(note)}
  | ShowGradesEditor => {...state, editor: GradesEditor}
  | ShowChecklistEditor => {...state, editor: ChecklistEditor}
  | ShowNotesEditor => {...state, editor: NotesEditor}
  }

let passed = (grades, evaluationCriteria) => grades |> Js.Array.filter(g => {
    let passGrade =
      evaluationCriteria
      |> ArrayUtils.unsafeFind(
        ec => EvaluationCriterion.id(ec) == (g |> Grade.evaluationCriterionId),
        "CoursesReview__GradeCard: Unable to find evaluation criterion with id - " ++
        (g |> Grade.evaluationCriterionId),
      )
      |> EvaluationCriterion.passGrade

    g |> Grade.value < passGrade
  }) |> ArrayUtils.isEmpty

module CreateGradingMutation = %graphql(
  `
    mutation CreateGradingMutation($submissionId: ID!, $feedback: String, $grades: [GradeInput!]!, $note: String,  $checklist: JSON!) {
      createGrading(submissionId: $submissionId, feedback: $feedback, grades: $grades, note: $note, checklist: $checklist){
        success
      }
    }
  `
)

module UndoGradingMutation = %graphql(
  `
    mutation UndoGradingMutation($submissionId: ID!) {
      undoGrading(submissionId: $submissionId){
        success
      }
    }
  `
)

let undoGrading = (submissionId, send) => {
  send(BeginSaving)

  UndoGradingMutation.make(~submissionId, ())
  |> GraphqlQuery.sendQuery
  |> Js.Promise.then_(response => {
    response["undoGrading"]["success"] ? DomUtils.reload() |> ignore : send(FinishSaving)
    Js.Promise.resolve()
  })
  |> ignore
}

let trimToOption = s =>
  switch s |> String.trim {
  | "" => None
  | s => Some(s)
  }

let gradeSubmissionQuery = (submissionId, state, send, evaluationCriteria, addGradingCB) => {
  let jsGradesArray = state.grades |> Array.map(g => g |> Grade.asJsType)

  let checklist = state.checklist |> SubmissionChecklistItem.encodeArray
  send(BeginSaving)

  let feedback = state.newFeedback |> trimToOption
  let note = state.note |> OptionUtils.flatMap(trimToOption)

  CreateGradingMutation.make(
    ~submissionId,
    ~feedback?,
    ~note?,
    ~grades=jsGradesArray,
    ~checklist,
    (),
  )
  |> GraphqlQuery.sendQuery
  |> Js.Promise.then_(response => {
    response["createGrading"]["success"] ? () : ()

    // addGradingCB(
    //     ~newFeedback=state.newFeedback,
    //     ~passed=passed(state.grades, evaluationCriteria),
    //     ~grades=state.grades,
    //     ~checklist=state.checklist,
    //   )
    send(FinishSaving)
    Js.Promise.resolve()
  })
  |> ignore
}

let updateGrading = (grade, state, send) => {
  let newGrades =
    state.grades
    |> Js.Array.filter(g =>
      g |> Grade.evaluationCriterionId != (grade |> Grade.evaluationCriterionId)
    )
    |> Array.append([grade])

  send(UpdateGrades(newGrades))
}

let handleGradePillClick = (evaluationCriterionId, value, state, send, event) => {
  event |> ReactEvent.Mouse.preventDefault
  switch send {
  | Some(send) => updateGrading(Grade.make(~evaluationCriterionId, ~value), state, send)
  | None => ()
  }
}

let findEvaluationCriterion = (evaluationCriteria, evaluationCriterionId) =>
  switch evaluationCriteria |> Js.Array.find(ec =>
    ec |> EvaluationCriterion.id == evaluationCriterionId
  ) {
  | Some(ec) => ec
  | None =>
    Rollbar.error(
      "Unable to find evaluation Criterion with id: " ++
      (evaluationCriterionId ++
      "in CoursesRevew__GradeCard"),
    )
    evaluationCriteria[0]
  }

let gradePillHeader = (evaluationCriteriaName, selectedGrade, gradeLabels) =>
  <div className="flex justify-between">
    <p className="text-xs font-semibold"> {evaluationCriteriaName |> str} </p>
    <p className="text-xs font-semibold">
      {(selectedGrade |> string_of_int) ++
        ("/" ++
        (GradeLabel.maxGrade(gradeLabels |> Array.to_list) |> string_of_int)) |> str}
    </p>
  </div>

let gradePillClasses = (selectedGrade, currentGrade, passgrade, send) => {
  let defaultClasses =
    "course-review-grade-card__grade-pill border-gray-400 py-1 px-2 text-sm flex-1 font-semibold " ++
    switch send {
    | Some(_) =>
      "cursor-pointer hover:shadow-lg focus:outline-none " ++ (
        currentGrade >= passgrade
          ? "hover:bg-green-500 hover:text-white "
          : "hover:bg-red-500 hover:text-white "
      )

    | None => ""
    }

  defaultClasses ++ (
    currentGrade <= selectedGrade
      ? switch selectedGrade >= passgrade {
        | true => "bg-green-500 text-white shadow-lg"
        | false => "bg-red-500 text-white shadow-lg"
        }
      : "bg-white text-gray-900"
  )
}

let showGradePill = (key, evaluationCriterion, gradeValue, passGrade, state, send) =>
  <div
    ariaLabel={"evaluation-criterion-" ++ (evaluationCriterion |> EvaluationCriterion.id)}
    key={key |> string_of_int}
    className="md:pr-8 mt-4">
    {gradePillHeader(
      evaluationCriterion |> EvaluationCriterion.name,
      gradeValue,
      evaluationCriterion |> EvaluationCriterion.gradesAndLabels,
    )}
    <div className="course-review-grade-card__grade-bar inline-flex w-full text-center mt-1">
      {evaluationCriterion |> EvaluationCriterion.gradesAndLabels |> Array.map(gradeLabel => {
        let gradeLabelGrade = gradeLabel |> GradeLabel.grade

        <button
          key={string_of_int(gradeLabelGrade)}
          onClick={handleGradePillClick(
            evaluationCriterion |> EvaluationCriterion.id,
            gradeLabelGrade,
            state,
            send,
          )}
          title={GradeLabel.label(gradeLabel)}
          className={gradePillClasses(gradeValue, gradeLabelGrade, passGrade, send)}>
          {switch send {
          | Some(_) => string_of_int(gradeLabelGrade)->str
          | None => React.null
          }}
        </button>
      }) |> React.array}
    </div>
  </div>

let showGrades = (grades, evaluationCriteria, state) =>
  <div> {grades |> Grade.sort(evaluationCriteria) |> Array.mapi((key, grade) => {
      let gradeEcId = Grade.evaluationCriterionId(grade)
      let ec =
        evaluationCriteria |> ArrayUtils.unsafeFind(
          ec => ec |> EvaluationCriterion.id == gradeEcId,
          "Unable to find evaluation Criterion with id: " ++
          (gradeEcId ++
          "in CoursesRevew__GradeCard"),
        )

      showGradePill(key, ec, grade |> Grade.value, ec |> EvaluationCriterion.passGrade, state, None)
    }) |> React.array} </div>
let renderGradePills = (evaluationCriteria, targetEvaluationCriteriaIds, state, send) =>
  targetEvaluationCriteriaIds |> Array.mapi((key, evaluationCriterionId) => {
    let ec =
      evaluationCriteria |> ArrayUtils.unsafeFind(
        e => e |> EvaluationCriterion.id == evaluationCriterionId,
        "CoursesReview__GradeCard: Unable to find evaluation criterion with id - " ++
        evaluationCriterionId,
      )
    let grade =
      state.grades |> Js.Array.find(g =>
        g |> Grade.evaluationCriterionId == (ec |> EvaluationCriterion.id)
      )
    let gradeValue = switch grade {
    | Some(g) => g |> Grade.value
    | None => 0
    }

    let passGrade = ec |> EvaluationCriterion.passGrade

    showGradePill(key, ec, gradeValue, passGrade, state, Some(send))
  }) |> React.array
let gradeStatusClasses = (color, status) =>
  "w-12 h-10 p-1 mr-2 md:mr-0 md:w-26 md:h-22 rounded md:rounded-lg border flex justify-center items-center bg-" ++
  (color ++
  ("-100 " ++
  ("border-" ++
  (color ++
  ("-400 " ++
  switch status {
  | Grading => "course-review-grade-card__status-pulse"
  | Graded(_)
  | Ungraded => ""
  })))))

let submissionStatusIcon = (status, overlaySubmission, send) => {
  let (text, color) = switch status {
  | Graded(passed) => passed ? ("Completed", "green") : ("Rejected", "red")
  | Grading => ("Reviewing", "orange")
  | Ungraded => ("Pending Review", "gray")
  }

  <div
    ariaLabel="submission-status"
    className="flex w-full md:w-3/6 flex-col items-center justify-center md:border-l mt-4 md:mt-0">
    <div
      className="flex flex-col-reverse md:flex-row items-start md:items-stretch justify-center w-full md:pl-6">
      {switch (overlaySubmission |> OverlaySubmission.evaluatedAt, status) {
      | (Some(date), Graded(_)) =>
        <div
          className="bg-gray-200 block md:flex flex-col w-full justify-between rounded-lg pt-3 mr-2 mt-4 md:mt-0">
          <div>
            <p className="text-xs px-3"> {"Evaluated By" |> str} </p>
            <p className="text-sm font-semibold px-3 pb-3">
              {switch overlaySubmission |> OverlaySubmission.evaluatorName {
              | Some(name) => name |> str
              | None => <em> {"Deleted Coach" |> str} </em>
              }}
            </p>
          </div>
          <div
            className="text-xs bg-gray-300 flex items-center rounded-b-lg px-3 py-2 md:px-3 md:py-1">
            {"on " ++ date->DateFns.format("MMMM d, yyyy") |> str}
          </div>
        </div>
      | (None, Graded(_))
      | (_, Grading)
      | (_, Ungraded) => React.null
      }}
      <div className="w-full md:w-26 flex flex-row md:flex-col md:items-center justify-center">
        <div className={gradeStatusClasses(color, status)}>
          {switch status {
          | Graded(passed) =>
            passed
              ? <Icon className="if i-badge-check-solid text-xl md:text-5xl text-green-500" />
              : <FaIcon classes="fas fa-exclamation-triangle text-xl md:text-4xl text-red-500" />
          | Grading =>
            <Icon className="if i-writing-pad-solid text-xl md:text-5xl text-orange-300" />
          | Ungraded => <Icon className="if i-eye-solid text-xl md:text-4xl text-gray-400" />
          }}
        </div>
        <p
          className={"text-xs flex items-center justify-center md:block text-center w-full border rounded px-1 py-px font-semibold md:mt-1 " ++
          ("border-" ++
          (color ++
          ("-400 " ++
          ("bg-" ++ (color ++ ("-100 " ++ ("text-" ++ (color ++ "-800 "))))))))}>
          {text |> str}
        </p>
      </div>
    </div>
    {switch (overlaySubmission |> OverlaySubmission.evaluatedAt, status) {
    | (Some(_), Graded(_)) =>
      <div className="mt-4 md:pl-6 w-full">
        <button
          onClick={_ =>
            WindowUtils.confirm(
              "Are you sure you want to remove these grades? This will return the submission to a 'Pending Review' state.",
              () => OverlaySubmission.id(overlaySubmission)->undoGrading(send),
            )}
          className="btn btn-danger btn-small">
          <i className="fas fa-undo" /> <span className="ml-2"> {"Undo Grading" |> str} </span>
        </button>
      </div>
    | (None, Graded(_))
    | (_, Grading)
    | (_, Ungraded) => React.null
    }}
  </div>
}

let gradeSubmission = (
  submissionId,
  state,
  send,
  evaluationCriteria,
  addGradingCB,
  status,
  event,
) => {
  event |> ReactEvent.Mouse.preventDefault
  switch status {
  | Graded(_) => gradeSubmissionQuery(submissionId, state, send, evaluationCriteria, addGradingCB)
  | Grading
  | Ungraded => ()
  }
}

let reviewButtonDisabled = status =>
  switch status {
  | Graded(_) => false
  | Grading
  | Ungraded => true
  }

let computeStatus = (
  overlaySubmission,
  selectedGrades,
  evaluationCriteria,
  targetEvaluationCriteriaIds,
) => {
  let currentGradingCriteria =
    evaluationCriteria |> Js.Array.filter(criterion =>
      targetEvaluationCriteriaIds |> Array.mem(EvaluationCriterion.id(criterion))
    )
  switch (
    overlaySubmission |> OverlaySubmission.passedAt,
    overlaySubmission |> OverlaySubmission.grades |> ArrayUtils.isNotEmpty,
  ) {
  | (Some(_), _) => Graded(true)
  | (None, true) => Graded(false)
  | (_, _) =>
    if selectedGrades == [] {
      Ungraded
    } else if selectedGrades |> Array.length != (currentGradingCriteria |> Array.length) {
      Grading
    } else {
      Graded(passed(selectedGrades, currentGradingCriteria))
    }
  }
}

let submitButtonText = (feedback, grades) =>
  switch (feedback != "", grades |> ArrayUtils.isNotEmpty) {
  | (false, false)
  | (false, true) => "Save grades"
  | (true, false)
  | (true, true) => "Save grades & send feedback"
  }

let noteForm = (overlaySubmission, teamSubmission, note, send) =>
  switch overlaySubmission |> OverlaySubmission.grades {
  | [] =>
    let (noteAbout, additionalHelp) = teamSubmission
      ? (
          "team",
          " This submission is from a team, so a note added here will be posted to the report of all students in the team.",
        )
      : ("student", "")

    let help =
      <HelpIcon className="ml-1">
        {"Notes can be used to keep track of a " ++
        (noteAbout ++
        ("'s progress. These notes are shown only to coaches in a student's report." ++
        additionalHelp)) |> str}
      </HelpIcon>

    let textareaId = "note-for-submission-" ++ (overlaySubmission |> OverlaySubmission.id)

    <div className="text-sm">
      <h5 className="font-semibold text-sm flex items-center">
        <i className="far fa-sticky-note text-gray-800 text-base" />
        {switch note {
        | Some(_) =>
          <span className="ml-2 md:ml-3 tracking-wide">
            <label htmlFor=textareaId> {"Write a Note" |> str} </label> help
          </span>
        | None =>
          <div className="ml-2 md:ml-3 tracking-wide flex justify-between items-center w-full">
            <span>
              <span>
                {"Would you like to write a note about this " ++ (noteAbout ++ "?") |> str}
              </span>
              help
            </span>
            <button
              className="btn btn-small btn-primary-ghost ml-1" onClick={_ => send(UpdateNote(""))}>
              {"Write a Note" |> str}
            </button>
          </div>
        }}
      </h5>
      {switch note {
      | Some(note) =>
        <div className="ml-6 md:ml-7 mt-2">
          <MarkdownEditor
            maxLength=10000
            textareaId
            value=note
            onChange={value => send(UpdateNote(value))}
            profile=Markdown.Permissive
            placeholder="Did you notice something while reviewing this submission?"
          />
        </div>
      | None => React.null
      }}
    </div>
  | _someGrades => React.null
  }

@react.component
let make = (
  ~overlaySubmission,
  ~teamSubmission,
  ~evaluationCriteria,
  ~reviewChecklist,
  ~addGradingCB,
  ~updateReviewChecklistCB,
  ~targetId,
  ~targetEvaluationCriteriaIds,
) => {
  let (state, send) = React.useReducer(
    reducer,
    {
      grades: [],
      newFeedback: "",
      saving: false,
      note: None,
      checklist: overlaySubmission |> OverlaySubmission.checklist,
      editor: GradesEditor,
    },
  )

  let status = computeStatus(
    overlaySubmission,
    state.grades,
    evaluationCriteria,
    targetEvaluationCriteriaIds,
  )

  let updateChecklistCB = switch overlaySubmission |> OverlaySubmission.grades {
  | [] => Some(checklist => send(UpdateChecklist(checklist)))
  | _ => None
  }
  let pending = overlaySubmission |> OverlaySubmission.grades |> ArrayUtils.isEmpty

  <DisablingCover disabled=state.saving>
    <div className="flex md:flex-row flex-col w-full">
      <div className="md:w-1/2 w-full p-4">
        <div className="bg-white rounded-lg shadow px-6 py-4">
          <SubmissionChecklistShow checklist=state.checklist updateChecklistCB pending />
        </div>
      </div>
      <div className="md:w-1/2 w-full p-4">
        <div className="bg-white rounded-lg shadow p-4">
          {switch state.editor {
          | GradesEditor =>
            <div>
              <div className="px-4 pt-4 md:px-6">
                <div className="flex h-7 items-end">
                  <h5 className="font-semibold text-sm flex items-center">
                    <PfIcon
                      className="if i-check-square-alt-regular text-gray-800 text-base md:text-lg inline-block"
                    />
                    <span className="ml-2 md:ml-3 tracking-wide">
                      {"Review Checklist" |> str}
                    </span>
                  </h5>
                </div>
                <button
                  className="mt-2 bg-gray-100 p-4 rounded-lg w-full text-left text-gray-900 font-semibold hover:bg-gray-200 hover:border-gray-300 border-dashed focus:outline-none"
                  onClick={_ => send(ShowChecklistEditor)}>
                  <span className="ml-3"> {"Show Review Checklist" |> str} </span>
                </button>
              </div>
              <div className="px-4 pt-4 md:px-6 md:pt-6 course-review__feedback-editor text-sm">
                <h5 className="font-semibold text-sm flex items-center">
                  <PfIcon
                    className="if i-comment-alt-regular text-gray-800 text-base md:text-lg inline-block"
                  />
                  <span className="ml-2 md:ml-3 tracking-wide"> {"Add Your Feedback" |> str} </span>
                </h5>
                <div className="mt-2 md:ml-7" ariaLabel="feedback">
                  <MarkdownEditor
                    onChange={feedback => send(UpdateFeedback(feedback))}
                    value=state.newFeedback
                    profile=Markdown.Permissive
                    maxLength=10000
                    placeholder="This feedback will be emailed to students when you finish grading."
                  />
                </div>
              </div>
              <div className="w-full px-4 pt-4 md:px-6 md:pt-6">
                {noteForm(overlaySubmission, teamSubmission, state.note, send)}
                <h5 className="font-semibold text-sm flex items-center mt-4 md:mt-6">
                  <Icon className="if i-tachometer-regular text-gray-800 text-base" />
                  <span className="ml-2 md:ml-3 tracking-wide"> {"Grade Card" |> str} </span>
                </h5>
                <div
                  className="flex md:flex-row flex-col border md:ml-7 bg-gray-100 p-2 md:p-4 rounded-lg mt-2">
                  <div className="w-full md:w-3/6">
                    {switch overlaySubmission |> OverlaySubmission.grades {
                    | [] =>
                      renderGradePills(evaluationCriteria, targetEvaluationCriteriaIds, state, send)

                    | grades => showGrades(grades, evaluationCriteria, state)
                    }}
                  </div>
                  {submissionStatusIcon(status, overlaySubmission, send)}
                </div>
              </div>
              {switch overlaySubmission |> OverlaySubmission.grades {
              | [] =>
                <div className="bg-white pt-4 mr-4 ml-4 md:mr-6 md:ml-13">
                  <button
                    disabled={reviewButtonDisabled(status)}
                    className="btn btn-success btn-large w-full border border-green-600"
                    onClick={gradeSubmission(
                      overlaySubmission |> OverlaySubmission.id,
                      state,
                      send,
                      evaluationCriteria,
                      addGradingCB,
                      status,
                    )}>
                    {submitButtonText(state.newFeedback, state.grades) |> str}
                  </button>
                </div>

              | _ => React.null
              }}
            </div>

          | ChecklistEditor =>
            <div>
              <div className="btn btn-default" onClick={_ => send(ShowGradesEditor)}>
                {str("back")}
              </div>
              <CoursesReview__Checklist
                reviewChecklist
                updateFeedbackCB={feedback => send(GenerateFeeback(feedback))}
                feedback=state.newFeedback
                updateReviewChecklistCB
                targetId
              />
            </div>
          | NotesEditor => <div> {str("back")} </div>
          }}
        </div>
      </div>
    </div>
  </DisablingCover>
}
