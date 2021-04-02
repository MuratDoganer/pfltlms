open CourseEditor__Types

let t = I18n.t(~scope="components.CourseEditor__Form")
let ts = I18n.t(~scope="shared")

let str = React.string

type tabs =
  | DetailsTab
  | ImagesTab
  | ActionsTab

let selectedTabClasses = selected =>
  "flex items-center focus:outline-none justify-center w-1/3 p-3 font-semibold rounded-t-lg leading-relaxed border border-gray-400 text-gray-600 cursor-pointer " ++ (
    selected ? "text-primary-500 bg-white border-b-0" : "bg-gray-100"
  )

let tabItemsClasses = selected => selected ? "" : "hidden"

type progressionBehavior = [#Limited | #Unlimited | #Strict]

type state = {
  name: string,
  description: string,
  endsAt: option<Js.Date.t>,
  hasNameError: bool,
  hasDescriptionError: bool,
  hasDateError: bool,
  about: string,
  publicSignup: bool,
  dirty: bool,
  saving: bool,
  featured: bool,
  progressionBehavior: progressionBehavior,
  progressionLimit: int,
}

type action =
  | UpdateName(string, bool)
  | UpdateDescription(string, bool)
  | UpdateEndsAt(option<Js.Date.t>)
  | StartSaving
  | FailSaving
  | UpdateAbout(string)
  | UpdatePublicSignup(bool)
  | UpdateFeatured(bool)
  | UpdateProgressionBehavior(progressionBehavior)
  | UpdateProgressionLimit(int)

let reducer = (state, action) =>
  switch action {
  | StartSaving => {...state, saving: true}
  | FailSaving => {...state, saving: false}
  | UpdateName(name, hasNameError) => {
      ...state,
      name: name,
      hasNameError: hasNameError,
      dirty: true,
    }
  | UpdateDescription(description, hasDescriptionError) => {
      ...state,
      description: description,
      hasDescriptionError: hasDescriptionError,
      dirty: true,
    }
  | UpdateEndsAt(date) => {...state, endsAt: date, dirty: true}
  | UpdatePublicSignup(publicSignup) => {...state, publicSignup: publicSignup, dirty: true}
  | UpdateAbout(about) => {...state, about: about, dirty: true}
  | UpdateFeatured(featured) => {...state, featured: featured, dirty: true}
  | UpdateProgressionBehavior(progressionBehavior) => {
      ...state,
      progressionBehavior: progressionBehavior,
      dirty: true,
    }
  | UpdateProgressionLimit(progressionLimit) => {
      ...state,
      progressionBehavior: #Limited,
      progressionLimit: progressionLimit,
      dirty: true,
    }
  }

module CreateCourseQuery = %graphql(
  `
    mutation CreateCourseMutation($name: String!, $description: String!, $endsAt: ISO8601DateTime, $about: String!, $publicSignup: Boolean!, $featured: Boolean!, $progressionBehavior: ProgressionBehavior!, $progressionLimit: Int) {
      createCourse(name: $name, description: $description, endsAt: $endsAt, about: $about, publicSignup: $publicSignup, featured: $featured, progressionBehavior: $progressionBehavior, progressionLimit: $progressionLimit) {
        course {
          ...Course.Fragments.AllFields
        }
      }
    }
  `
)

module UpdateCourseQuery = %graphql(
  `
    mutation UpdateCourseMutation($id: ID!, $name: String!, $description: String!, $endsAt: ISO8601DateTime, $about: String!, $publicSignup: Boolean!, $featured: Boolean!, $progressionBehavior: ProgressionBehavior!, $progressionLimit: Int) {
      updateCourse(id: $id, name: $name, description: $description, endsAt: $endsAt, about: $about, publicSignup: $publicSignup, featured: $featured, progressionBehavior: $progressionBehavior, progressionLimit: $progressionLimit) {
        course {
          ...Course.Fragments.AllFields
        }
      }
    }
  `
)

module ArciveCourseQuery = %graphql(
  `
  mutation ArchiveCourseMutation($id: ID!) {
    archiveCourse(id: $id)  {
      success
    }
  }
`
)

module UnarchiveCourseQuery = %graphql(
  `
  mutation UnarchiveCourseMutation($id: ID!) {
    unarchiveCourse(id: $id)  {
      success
    }
  }
`
)

let updateName = (send, name) => {
  let hasError = name->String.trim->String.length < 2
  send(UpdateName(name, hasError))
}

let updateDescription = (send, description) => {
  let lengthOfDescription = description->String.trim->String.length
  let hasError = lengthOfDescription < 2 || lengthOfDescription >= 150
  send(UpdateDescription(description, hasError))
}

let saveDisabled = state =>
  state.hasDateError ||
  (state.hasDescriptionError ||
  (state.description == "" ||
    (state.hasNameError ||
    (state.name == "" || (!state.dirty || state.saving)))))

let progressionLimitForQuery = state =>
  switch state.progressionBehavior {
  | #Unlimited
  | #Strict =>
    None
  | #Limited => Some(state.progressionLimit)
  }

let createCourse = (state, send, reloadCoursesCB) => {
  send(StartSaving)

  let createCourseQuery = CreateCourseQuery.make(
    ~name=state.name,
    ~description=state.description,
    ~endsAt=?state.endsAt->Belt.Option.map(DateFns.encodeISO),
    ~about=state.about,
    ~publicSignup=state.publicSignup,
    ~featured=state.featured,
    ~progressionBehavior=state.progressionBehavior,
    ~progressionLimit=?progressionLimitForQuery(state),
    (),
  )

  createCourseQuery |> GraphqlQuery.sendQuery |> Js.Promise.then_(result => {
    switch result["createCourse"]["course"] {
    | Some(_course) => reloadCoursesCB()
    | None => send(FailSaving)
    }

    Js.Promise.resolve()
  }) |> Js.Promise.catch(error => {
    Js.log(error)
    send(FailSaving)
    Js.Promise.resolve()
  }) |> ignore
}

let updateCourse = (state, send, updateCourseCB, course) => {
  send(StartSaving)

  let updateCourseQuery = UpdateCourseQuery.make(
    ~id=Course.id(course),
    ~name=state.name,
    ~description=state.description,
    ~endsAt=?state.endsAt->Belt.Option.map(DateFns.encodeISO),
    ~about=state.about,
    ~publicSignup=state.publicSignup,
    ~featured=state.featured,
    ~progressionBehavior=state.progressionBehavior,
    ~progressionLimit=?progressionLimitForQuery(state),
    (),
  )

  updateCourseQuery |> GraphqlQuery.sendQuery |> Js.Promise.then_(result => {
    switch result["updateCourse"]["course"] {
    | Some(course) => updateCourseCB(Course.makeFromJs(course))
    | None => send(FailSaving)
    }

    Js.Promise.resolve()
  }) |> Js.Promise.catch(error => {
    Js.log(error)
    send(FailSaving)
    Js.Promise.resolve()
  }) |> ignore
}

let archiveCourse = (send, reloadCoursesCB, course) => {
  send(StartSaving)

  ArciveCourseQuery.make(~id=course |> Course.id, ())
  |> GraphqlQuery.sendQuery
  |> Js.Promise.then_(result => {
    result["archiveCourse"]["success"] ? reloadCoursesCB() : send(FailSaving)
    Js.Promise.resolve()
  })
  |> Js.Promise.catch(error => {
    Js.log(error)
    send(FailSaving)
    Js.Promise.resolve()
  })
  |> ignore
}

let unarchiveCourse = (send, reloadCoursesCB, course) => {
  send(StartSaving)

  UnarchiveCourseQuery.make(~id=course |> Course.id, ())
  |> GraphqlQuery.sendQuery
  |> Js.Promise.then_(result => {
    result["unarchiveCourse"]["success"] ? reloadCoursesCB() : send(FailSaving)
    Js.Promise.resolve()
  })
  |> Js.Promise.catch(error => {
    Js.log(error)
    send(FailSaving)
    Js.Promise.resolve()
  })
  |> ignore
}

let booleanButtonClasses = bool => {
  let classes = "toggle-button__button"
  classes ++ (bool ? " toggle-button__button--active" : "")
}

let enablePublicSignupButton = (publicSignup, send) =>
  <div className="flex items-center mt-5">
    <label className="block tracking-wide text-xs font-semibold mr-6" htmlFor="public-signup">
      {t("enable_public_signup_label")->str}
    </label>
    <div id="public-signup" className="flex toggle-button__group flex-shrink-0 rounded-lg">
      <button
        className={booleanButtonClasses(publicSignup)}
        onClick={_ => send(UpdatePublicSignup(true))}>
        {ts("_yes")->str}
      </button>
      <button
        className={booleanButtonClasses(!publicSignup)}
        onClick={_ => send(UpdatePublicSignup(false))}>
        {ts("_no")->str}
      </button>
    </div>
  </div>

let featuredButton = (featured, send) =>
  <div className="flex items-center mt-5">
    <label className="block tracking-wide text-xs font-semibold mr-6" htmlFor="featured">
      {t("feature_course_in_homepage_label")->str}
    </label>
    <div id="featured" className="flex toggle-button__group flex-shrink-0 rounded-lg">
      <button className={booleanButtonClasses(featured)} onClick={_ => send(UpdateFeatured(true))}>
        {ts("_yes")->str}
      </button>
      <button
        className={booleanButtonClasses(!featured)} onClick={_ => send(UpdateFeatured(false))}>
        {ts("_no")->str}
      </button>
    </div>
  </div>

let about = course => Belt.Option.getWithDefault(Course.about(course), "")

let updateAboutCB = (send, about) => send(UpdateAbout(about))

let computeInitialState = course =>
  switch course {
  | Some(course) => {
      name: Course.name(course),
      description: Course.description(course),
      endsAt: Course.endsAt(course),
      hasNameError: false,
      hasDateError: false,
      hasDescriptionError: false,
      dirty: false,
      saving: false,
      about: about(course),
      publicSignup: Course.publicSignup(course),
      featured: Course.featured(course),
      progressionBehavior: Course.progressionBehavior(course),
      progressionLimit: Course.progressionLimit(course)->Belt.Option.getWithDefault(1),
    }
  | None => {
      name: "",
      description: "",
      endsAt: None,
      hasNameError: false,
      hasDateError: false,
      hasDescriptionError: false,
      dirty: false,
      saving: false,
      about: "",
      publicSignup: false,
      featured: true,
      progressionBehavior: #Limited,
      progressionLimit: 1,
    }
  }

let handleSelectProgressionLimit = (send, event) => {
  let target = ReactEvent.Form.target(event)

  switch target["value"] {
  | "1"
  | "2"
  | "3" =>
    send(UpdateProgressionLimit(int_of_string(target["value"])))
  | otherValue => Rollbar.error("Unexpected progression limit was selected: " ++ otherValue)
  }
}

let progressionBehaviorButtonClasses = (state, progressionBehavior, additionalClasses) => {
  let selected = state.progressionBehavior == progressionBehavior
  let defaultClasses =
    additionalClasses ++ " w-1/3 relative border font-semibold focus:outline-none rounded px-5 py-4 md:px-8 md:py-5 items-center cursor-pointer text-center bg-gray-200 hover:bg-gray-300"
  defaultClasses ++ (selected ? " text-primary-500 border-primary-500" : "")
}

let detailsTab = (state, send, course, updateCourseCB, reloadCoursesCB) => {
  <div>
    <div className="mt-5">
      <label className="inline-block tracking-wide text-xs font-semibold " htmlFor="name">
        {t("course_name.label")->str}
      </label>
      <input
        className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
        id="name"
        type_="text"
        placeholder={t("course_name.placeholder")}
        maxLength=50
        value=state.name
        onChange={event => updateName(send, ReactEvent.Form.target(event)["value"])}
      />
      <School__InputGroupError message={t("course_name.error_message")} active=state.hasNameError />
    </div>
    <div className="mt-5">
      <label className="inline-block tracking-wide text-xs font-semibold" htmlFor="description">
        {t("course_description.label")->str}
      </label>
      <input
        className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
        id="description"
        type_="text"
        placeholder={t("course_description.placeholder")}
        value=state.description
        maxLength=150
        onChange={event => updateDescription(send, ReactEvent.Form.target(event)["value"])}
      />
    </div>
    <School__InputGroupError
      message={t("course_description.error_message")} active=state.hasDescriptionError
    />
    <div className="mt-5">
      <label className="tracking-wide text-xs font-semibold" htmlFor="course-ends-at-input">
        {t("course_end_date.label")->str}
      </label>
      <span className="ml-1 text-xs"> {("(" ++ ts("optional") ++ ")")->str} </span>
      <HelpIcon className="ml-2" link="https://docs.pupilfirst.com/#/courses">
        {t("course_end_date.help")->str}
      </HelpIcon>
      <DatePicker
        onChange={date => send(UpdateEndsAt(date))} selected=?state.endsAt id="course-ends-at-input"
      />
    </div>
    <School__InputGroupError message="Enter a valid date" active=state.hasDateError />
    <div className="mt-5">
      <label className="tracking-wide text-xs font-semibold" htmlFor="course-about">
        {t("course_about.label")->str}
      </label>
      <div className="mt-2">
        <MarkdownEditor
          textareaId="course-about"
          onChange={updateAboutCB(send)}
          value=state.about
          placeholder={t("course_about.placeholder")}
          profile=Markdown.Permissive
          maxLength=10000
        />
      </div>
    </div>
    <div className="mt-5">
      <label className="tracking-wide text-xs font-semibold">
        {t("progression_behavior.label")->str}
      </label>
      <HelpIcon
        className="ml-2" link="https://docs.pupilfirst.com/#/courses?id=progression-behaviour">
        {t("progression_behavior.help")->str}
      </HelpIcon>
      <div className="flex mt-2">
        <button
          onClick={_ => send(UpdateProgressionBehavior(#Limited))}
          className={progressionBehaviorButtonClasses(state, #Limited, "mr-1")}>
          <div className="font-bold text-xl"> {t("progression_behavior.limited.title")->str} </div>
          <div className="text-xs mt-2">
            <div> {t("progression_behavior.limited.description_start")->str} </div>
            <select
              id="progression-limit"
              onChange={handleSelectProgressionLimit(send)}
              className="my-1 cursor-pointer inline-block appearance-none bg-white border-b-2 text-xl font-semibold border-blue-500 hover:border-gray-500 p-1 leading-tight rounded-none focus:outline-none"
              style={ReactDOM.Style.make(~textAlignLast="center", ())}
              value={string_of_int(state.progressionLimit)}>
              <option value="1"> {t("progression_behavior.limited.once")->str} </option>
              <option value="2"> {t("progression_behavior.limited.twice")->str} </option>
              <option value="3"> {t("progression_behavior.limited.thrice")->str} </option>
            </select>
            <div> {t("progression_behavior.limited.description_end")->str} </div>
          </div>
        </button>
        <button
          onClick={_ => send(UpdateProgressionBehavior(#Unlimited))}
          className={progressionBehaviorButtonClasses(state, #Unlimited, "mx-1")}>
          <div className="font-bold text-xl">
            {t("progression_behavior.unlimited.title")->str}
          </div>
          <span className="text-xs"> {t("progression_behavior.unlimited.description")->str} </span>
        </button>
        <button
          onClick={_ => send(UpdateProgressionBehavior(#Strict))}
          className={progressionBehaviorButtonClasses(state, #Strict, "ml-1")}>
          <div className="font-bold text-xl"> {t("progression_behavior.strict.title")->str} </div>
          <span className="text-xs"> {t("progression_behavior.strict.description")->str} </span>
        </button>
      </div>
    </div>
    {featuredButton(state.featured, send)}
    {enablePublicSignupButton(state.publicSignup, send)}
    <div className="max-w-2xl p-6 mx-auto">
      <div className="flex">
        {switch course {
        | Some(course) =>
          <button
            disabled={saveDisabled(state)}
            onClick={_ => updateCourse(state, send, updateCourseCB, course)}
            className="w-full btn btn-large btn-primary mt-3">
            {t("update_course")->str}
          </button>

        | None =>
          <button
            disabled={saveDisabled(state)}
            onClick={_ => createCourse(state, send, reloadCoursesCB)}
            className="w-full btn btn-large btn-primary mt-3">
            {t("create_course")->str}
          </button>
        }}
      </div>
    </div>
  </div>
}

let submitButtonIcons = saving => saving ? "fas fa-spinner fa-spin" : "fa fa-exclamation-triangle"

let actionsTab = (state, send, reloadCoursesCB, course) => {
  <div>
    {Belt.Option.isSome(Course.archivedAt(course))
      ? <div className="mt-2">
          <label className="tracking-wide text-xs font-semibold">
            {str(t("actions.unarchive_course.label"))}
          </label>
          <div>
            <button
              disabled=state.saving
              className="btn btn-success btn-large mt-2"
              onClick={_ =>
                WindowUtils.confirm(t("alert.unarchive_message"), () =>
                  unarchiveCourse(send, reloadCoursesCB, course)
                )}>
              <FaIcon classes={submitButtonIcons(state.saving)} />
              <span className="ml-2"> {t("actions.unarchive_course.button_text")->str} </span>
            </button>
          </div>
        </div>
      : <div className="mt-2">
          <label className="tracking-wide text-xs font-semibold">
            {str(t("actions.archive_course.label"))}
          </label>
          <div>
            <button
              disabled=state.saving
              className="btn btn-danger btn-large mt-2"
              onClick={_ =>
                WindowUtils.confirm(t("alert.archive_message"), () =>
                  archiveCourse(send, reloadCoursesCB, course)
                )}>
              <FaIcon classes={submitButtonIcons(state.saving)} />
              <span className="ml-2"> {t("actions.archive_course.button_text")->str} </span>
            </button>
          </div>
        </div>}
  </div>
}

@react.component
let make = (~course, ~updateCourseCB, ~reloadCoursesCB, ~selectedTab) => {
  let (state, send) = React.useReducerWithMapState(reducer, course, computeInitialState)
  <DisablingCover disabled={state.saving}>
    <div className="mx-auto bg-white">
      <div className="pt-6 border-b border-gray-400 bg-gray-100">
        <div className="max-w-2xl mx-auto">
          <h5 className="uppercase text-center">
            {(
              course == None
                ? t("button_text.add_new_course")
                : t("button_text.edit_course_details")
            )->str}
          </h5>
          {ReactUtils.nullUnless(
            <div className="w-full pt-6">
              <div className="flex flex-wrap w-full max-w-3xl mx-auto text-sm px-3 -mb-px">
                <button
                  className={selectedTabClasses(selectedTab == DetailsTab)}
                  onClick={_ => RescriptReactRouter.push("./details")}>
                  <i className="fa fa-edit" />
                  <span className="ml-2"> {t("tabs.details")->str} </span>
                </button>
                <button
                  className={selectedTabClasses(selectedTab == ImagesTab)}
                  onClick={_ => RescriptReactRouter.push("./images")}>
                  <i className="fa fa-camera" />
                  <span className="ml-2"> {t("tabs.images")->str} </span>
                </button>
                <button
                  className={"-ml-px " ++ selectedTabClasses(selectedTab == ActionsTab)}
                  onClick={_ => RescriptReactRouter.push("./actions")}>
                  <i className="fa fa-cog" />
                  <span className="ml-2"> {t("tabs.actions")->str} </span>
                </button>
              </div>
            </div>,
            Belt.Option.isSome(course),
          )}
        </div>
      </div>
      <div className="max-w-2xl mx-auto">
        <div className={tabItemsClasses(selectedTab == DetailsTab)}>
          {detailsTab(state, send, course, updateCourseCB, reloadCoursesCB)}
        </div>
        {switch course {
        | Some(c) =>
          [
            <div key="actions-tab" className={tabItemsClasses(selectedTab == ActionsTab)}>
              {actionsTab(state, send, reloadCoursesCB, c)}
            </div>,
            <div key="images-tab" className={tabItemsClasses(selectedTab == ImagesTab)}>
              <CourseEditor__ImagesForm course=c updateCourseCB />
            </div>,
          ]->React.array
        | None => React.null
        }}
      </div>
    </div>
  </DisablingCover>
}
