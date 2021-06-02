%bs.raw(`require("./CoursesReview__Root.css")`)

open CoursesReview__Types

type reviewedTabSortCriterion = [#EvaluatedAt | #SubmittedAt]

let str = React.string
let tc = I18n.t(~scope="components.CoursesReview__Root")

type selectedTab = [#Reviewed | #Pending]

type filter = {
  nameOrEmail: option<string>,
  selectedLevel: option<Level.t>,
  selectedCoach: option<Coach.t>,
  tags: Belt.Set.String.t,
  sortDirection: [#Ascending | #Descending],
  reviewedTabSortCriterion: reviewedTabSortCriterion,
}

type state = {
  pendingSubmissions: Submissions.t,
  reviewedSubmissions: Submissions.t,
  selectedTab: selectedTab,
  filterString: string,
  reloadAt: Js.Date.t,
  filter: filter,
}

type action =
  | SelectLevel(Level.t)
  | DeselectLevel
  | ReloadSubmissions
  | SetSubmissions(array<IndexSubmission.t>, selectedTab, bool, option<string>, int)
  | UpdateReviewedSubmission(IndexSubmission.t)
  | SelectPendingTab
  | SelectReviewedTab
  | SetNameOrEmail(string)
  | UnsetNameOrEmail
  | SelectTag(string)
  | DeselectTag(string)
  | SelectCoach(Coach.t)
  | DeselectCoach
  | UpdateFilterString(string)
  | UpdateSortDirection([#Ascending | #Descending])
  | UpdateSortCriterion(selectedTab, [#EvaluatedAt | #SubmittedAt])
  | SyncSubmissionStatus(OverlaySubmission.t)

let makeSortBy = state =>
  switch state.selectedTab {
  | #Pending =>
    SubmissionsSorting.make(~sortCriterion=#SubmittedAt, ~sortDirection=state.filter.sortDirection)
  | #Reviewed =>
    SubmissionsSorting.make(
      ~sortCriterion=state.filter.reviewedTabSortCriterion,
      ~sortDirection=state.filter.sortDirection,
    )
  }

let reducer = (state, action) =>
  switch action {
  | SelectLevel(level) => {
      ...state,
      filter: {...state.filter, selectedLevel: Some(level)},
      filterString: "",
    }
  | DeselectLevel => {...state, filter: {...state.filter, selectedLevel: None}}
  | ReloadSubmissions => {
      ...state,
      pendingSubmissions: Unloaded,
      reviewedSubmissions: Unloaded,
      reloadAt: Js.Date.make(),
    }
  | SetSubmissions(submissions, selectedTab, hasNextPage, endCursor, totalCount) =>
    let filter = Submissions.makeFilter(state.filter.selectedLevel, state.filter.selectedCoach)

    let updatedSubmissions = switch (hasNextPage, endCursor) {
    | (_, None)
    | (false, Some(_)) =>
      Submissions.fullyLoaded(~submissions, ~filter, ~sortBy=makeSortBy(state), ~totalCount)
    | (true, Some(cursor)) =>
      Submissions.partiallyLoaded(
        ~submissions,
        ~filter,
        ~sortBy=makeSortBy(state),
        ~totalCount,
        ~cursor,
      )
    }

    switch selectedTab {
    | #Pending => {...state, pendingSubmissions: updatedSubmissions}
    | #Reviewed => {...state, reviewedSubmissions: updatedSubmissions}
    }
  | UpdateReviewedSubmission(submission) =>
    let filter = Submissions.makeFilter(state.filter.selectedLevel, state.filter.selectedCoach)

    {
      ...state,
      reviewedSubmissions: switch state.reviewedSubmissions {
      | Unloaded => Unloaded
      | PartiallyLoaded({submissions, totalCount}, cursor) =>
        Submissions.partiallyLoaded(
          ~submissions=submissions |> IndexSubmission.replace(submission),
          ~filter,
          ~sortBy=makeSortBy(state),
          ~totalCount,
          ~cursor,
        )
      | FullyLoaded({submissions, totalCount}) =>
        Submissions.fullyLoaded(
          ~submissions=submissions |> IndexSubmission.replace(submission),
          ~filter,
          ~totalCount,
          ~sortBy=makeSortBy(state),
        )
      },
    }
  | SelectPendingTab => {
      ...state,
      selectedTab: #Pending,
      filter: {...state.filter, sortDirection: #Ascending},
    }
  | SelectReviewedTab => {
      ...state,
      selectedTab: #Reviewed,
      filter: {...state.filter, sortDirection: #Descending},
    }
  | SelectCoach(coach) => {
      ...state,
      filter: {
        ...state.filter,
        selectedCoach: Some(coach),
      },
      filterString: "",
    }
  | DeselectCoach => {...state, filter: {...state.filter, selectedCoach: None}}
  | UpdateFilterString(filterString) => {...state, filterString: filterString}
  | UpdateSortDirection(sortDirection) => {
      ...state,
      filter: {...state.filter, sortDirection: sortDirection},
    }
  | SetNameOrEmail(search) => {
      ...state,
      filter: {
        ...state.filter,
        nameOrEmail: Some(search),
      },
      filterString: "",
    }
  | UnsetNameOrEmail => {
      ...state,
      filter: {
        ...state.filter,
        nameOrEmail: None,
      },
    }
  | SelectTag(tag) => {
      ...state,
      filter: {
        ...state.filter,
        tags: state.filter.tags->Belt.Set.String.add(tag),
      },
      filterString: "",
    }
  | DeselectTag(tag) => {
      ...state,
      filter: {
        ...state.filter,
        tags: state.filter.tags->Belt.Set.String.remove(tag),
      },
    }
  | UpdateSortCriterion(selectedTab, criterion) =>
    switch selectedTab {
    | #Pending => state
    | #Reviewed => {...state, filter: {...state.filter, reviewedTabSortCriterion: criterion}}
    }
  | SyncSubmissionStatus(overlaySubmission) =>
    let skipReload =
      state.pendingSubmissions
      |> Submissions.toArray
      |> Array.append(state.reviewedSubmissions |> Submissions.toArray)
      |> Js.Array.find(indexSubmission =>
        indexSubmission |> IndexSubmission.id == OverlaySubmission.id(overlaySubmission)
      )
      |> OptionUtils.mapWithDefault(
        indexSubmission => indexSubmission |> IndexSubmission.statusEq(overlaySubmission),
        true,
      )

    skipReload
      ? state
      : {
          ...state,
          pendingSubmissions: Unloaded,
          reviewedSubmissions: Unloaded,
          reloadAt: Js.Date.make(),
        }
  }

let computeInitialState = currentTeamCoach => {
  filter: {
    nameOrEmail: None,
    selectedLevel: None,
    selectedCoach: currentTeamCoach,
    tags: Belt.Set.String.empty,
    sortDirection: #Ascending,
    reviewedTabSortCriterion: #SubmittedAt,
  },
  pendingSubmissions: Unloaded,
  reviewedSubmissions: Unloaded,
  selectedTab: #Pending,
  filterString: "",
  reloadAt: Js.Date.make(),
}

let openOverlay = (submissionId, event) => {
  event |> ReactEvent.Mouse.preventDefault
  RescriptReactRouter.push("/submissions/" ++ submissionId)
}

let dropdownElementClasses = (level, selectedLevel) =>
  "p-3 w-full text-left font-semibold focus:outline-none " ++
  switch (selectedLevel, level) {
  | (Some(sl), Some(l)) when l |> Level.id == (sl |> Level.id) => "bg-gray-200 text-primary-500"
  | (None, None) => "bg-gray-200 text-primary-500"
  | _ => ""
  }

let buttonClasses = selected =>
  "w-1/2 md:w-auto py-2 px-3 md:px-6 font-semibold text-sm focus:outline-none " ++ (
    selected
      ? "bg-primary-100 shadow-inner text-primary-500"
      : "bg-white shadow-md hover:shadow hover:text-primary-500 hover:bg-gray-100"
  )

module Selectable = {
  type t =
    | Level(Level.t)
    | AssignedToCoach(Coach.t, string)
    | NameOrEmail(string)
    | Tag(string)

  let label = t =>
    switch t {
    | Level(level) => Some(LevelLabel.format(level |> Level.number |> string_of_int))
    | AssignedToCoach(_) => Some(tc("assigned_to"))
    | NameOrEmail(_) => Some("Name or Email")
    | Tag(_) => Some("Tagged with")
    }

  let value = t =>
    switch t {
    | Level(level) => level |> Level.name
    | AssignedToCoach(coach, currentCoachId) =>
      coach |> Coach.id == currentCoachId ? tc("me") : coach |> Coach.name
    | NameOrEmail(search) => search
    | Tag(tag) => tag
    }

  let searchString = t =>
    switch t {
    | Level(level) =>
      LevelLabel.searchString(level |> Level.number |> string_of_int, level |> Level.name)
    | AssignedToCoach(coach, currentCoachId) =>
      if coach |> Coach.id == currentCoachId {
        (coach |> Coach.name) ++ tc("assigned_to_me")
      } else {
        tc("assigned_to_coach") ++ (coach |> Coach.name)
      }
    | NameOrEmail(search) => search
    | Tag(tag) => "tag " ++ tag
    }

  let color = _t => "gray"
  let level = level => Level(level)
  let nameOrEmail = search => NameOrEmail(search)
  let tag = tagString => Tag(tagString)
  let assignedToCoach = (coach, currentCoachId) => AssignedToCoach(coach, currentCoachId)
}

module Multiselect = MultiselectDropdown.Make(Selectable)

let unselected = (levels, coaches, tags, currentCoachId, state) => {
  let unselectedLevels =
    levels
    |> Js.Array.filter(level =>
      state.filter.selectedLevel |> OptionUtils.mapWithDefault(
        selectedLevel => level |> Level.id != (selectedLevel |> Level.id),
        true,
      )
    )
    |> Array.map(Selectable.level)

  let unselectedCoaches =
    coaches
    |> Js.Array.filter(coach =>
      state.filter.selectedCoach |> OptionUtils.mapWithDefault(
        selectedCoach => coach |> Coach.id != Coach.id(selectedCoach),
        true,
      )
    )
    |> Array.map(coach => Selectable.assignedToCoach(coach, currentCoachId))

  let trimmedFilterString = state.filterString |> String.trim
  let nameOrEmail = trimmedFilterString == "" ? [] : [Selectable.nameOrEmail(trimmedFilterString)]

  let unselectedTags =
    Belt.Set.String.diff(tags, state.filter.tags)->Belt.Set.String.toArray
      |> Js.Array.map(Selectable.tag)

  unselectedLevels
  |> Array.append(nameOrEmail)
  |> Array.append(unselectedCoaches)
  |> Array.append(unselectedTags)
}

let selected = (state, currentCoachId) => {
  let selectedLevel =
    state.filter.selectedLevel |> OptionUtils.mapWithDefault(
      selectedLevel => [Selectable.level(selectedLevel)],
      [],
    )

  let selectedCoach =
    state.filter.selectedCoach |> OptionUtils.mapWithDefault(
      selectedCoach => [Selectable.assignedToCoach(selectedCoach, currentCoachId)],
      [],
    )

  let selectedSearchString =
    state.filter.nameOrEmail |> OptionUtils.mapWithDefault(
      nameOrEmail => [Selectable.nameOrEmail(nameOrEmail)],
      [],
    )

  let selectedTags = state.filter.tags |> Belt.Set.String.toArray |> Js.Array.map(Selectable.tag)

  selectedLevel
  |> Array.append(selectedCoach)
  |> Array.append(selectedSearchString)
  |> Array.append(selectedTags)
}

let onSelectFilter = (send, selectable) =>
  switch selectable {
  | Selectable.AssignedToCoach(coach, _currentCoachId) => send(SelectCoach(coach))
  | Level(level) => send(SelectLevel(level))
  | NameOrEmail(nameOrEmail) => send(SetNameOrEmail(nameOrEmail))
  | Tag(tag) => send(SelectTag(tag))
  }

let onDeselectFilter = (send, selectable) =>
  switch selectable {
  | Selectable.AssignedToCoach(_) => send(DeselectCoach)
  | Level(_) => send(DeselectLevel)
  | NameOrEmail(_) => send(UnsetNameOrEmail)
  | Tag(tag) => send(DeselectTag(tag))
  }

let filterPlaceholder = state =>
  switch (state.filter.selectedLevel, state.filter.selectedCoach) {
  | (None, Some(_)) => tc("filter_by_level")
  | (None, None) => tc("filter_by_level_or_submissions_assigned")
  | (Some(_), Some(_)) => tc("filter_by_another_level")
  | (Some(_), None) => tc("filter_by_another_level_or_submissions_assigned")
  }

let restoreFilterNotice = (send, currentCoach, message) =>
  <div
    className="mb-4 text-sm italic flex flex-col md:flex-row items-center justify-between p-3 border border-gray-300 bg-white rounded-lg">
    <span> {message |> str} </span>
    <button
      className="px-2 py-1 rounded text-xs overflow-hidden border text-gray-800 border-gray-300 bg-gray-200 hover:bg-gray-300 mt-1 md:mt-0"
      onClick={_ => send(SelectCoach(currentCoach))}>
      {tc("assigned_to") ++ ": " ++ tc("me") |> str} <i className="fas fa-level-up-alt ml-2" />
    </button>
  </div>

let restoreAssignedToMeFilter = (state, send, currentTeamCoach) =>
  currentTeamCoach |> OptionUtils.mapWithDefault(currentCoach =>
    switch state.filter.selectedCoach {
    | None =>
      restoreFilterNotice(send, currentCoach, tc("now_showing_submissions_from_all_students"))
    | Some(selectedCoach) when selectedCoach |> Coach.id == Coach.id(currentCoach) => React.null
    | Some(selectedCoach) =>
      restoreFilterNotice(
        send,
        currentCoach,
        tc(
          ~variables=[("name", selectedCoach |> Coach.name)],
          "now_showing_submissions_assigned_to",
        ),
      )
    }
  , React.null)

let filterSubmissions = (selectedLevel, selectedCoach, submissions) => {
  let levelFiltered =
    selectedLevel |> OptionUtils.mapWithDefault(
      level =>
        submissions |> Js.Array.filter(l => l |> IndexSubmission.levelId == (level |> Level.id)),
      submissions,
    )

  selectedCoach |> OptionUtils.mapWithDefault(
    coach =>
      levelFiltered |> Js.Array.filter(l =>
        l |> IndexSubmission.coachIds |> Array.mem(coach |> Coach.id)
      ),
    levelFiltered,
  )
}

module Sortable = {
  type t = [#EvaluatedAt | #SubmittedAt]

  let criterion = t =>
    switch t {
    | #SubmittedAt => tc("submitted_at")
    | #EvaluatedAt => tc("reviewed_at")
    }
  let criterionType = _t => #Number
}

module SubmissionsSorter = Sorter.Make(Sortable)

let submissionsSorter = (state, send) => {
  let criteria = switch state.selectedTab {
  | #Pending => [#SubmittedAt]
  | #Reviewed => [#SubmittedAt, #EvaluatedAt]
  }

  let selectedCriterion = switch state.selectedTab {
  | #Pending => #SubmittedAt
  | #Reviewed => state.filter.reviewedTabSortCriterion
  }
  <div ariaLabel="Change submissions sorting" className="flex-shrink-0 mt-3 md:mt-0 md:ml-2">
    <label className="block text-tiny font-semibold uppercase"> {tc("sort_by") |> str} </label>
    <SubmissionsSorter
      criteria
      selectedCriterion
      direction=state.filter.sortDirection
      onDirectionChange={sortDirection => send(UpdateSortDirection(sortDirection))}
      onCriterionChange={sortCriterion =>
        send(UpdateSortCriterion(state.selectedTab, sortCriterion))}
    />
  </div>
}

let displayedSubmissions = state =>
  switch state.selectedTab {
  | #Pending => state.pendingSubmissions
  | #Reviewed => state.reviewedSubmissions
  }

let submissionsCount = submissions =>
  submissions
  |> Submissions.totalCount
  |> OptionUtils.mapWithDefault(
    count =>
      <span
        className="course-review__status-tab-badge ml-2 text-white text-xs bg-red-500 w-auto h-5 px-1 inline-flex items-center justify-center rounded-full">
        {count |> string_of_int |> str}
      </span>,
    React.null,
  )

@react.component
let make = (~levels, ~courseId, ~teamCoaches, ~currentCoach, ~teamTags, ~userTags) => {
  let allTags = Belt.Set.String.union(teamTags, userTags)

  let (currentTeamCoach, _) = React.useState(() =>
    teamCoaches->Belt.Array.some(coach => coach |> Coach.id == (currentCoach |> Coach.id))
      ? Some(currentCoach)
      : None
  )

  let (state, send) = React.useReducerWithMapState(reducer, currentTeamCoach, computeInitialState)
  Js.log(state.filter)
  let url = RescriptReactRouter.useUrl()

  <div>
    {switch url.path {
    | list{"submissions", submissionId, "review"} =>
      <CoursesReview__SubmissionOverlay
        courseId
        submissionId
        currentCoach
        teamCoaches
        syncSubmissionCB={submission => send(SyncSubmissionStatus(submission))}
        removePendingSubmissionCB={() => send(ReloadSubmissions)}
        updateReviewedSubmissionCB={submission => send(UpdateReviewedSubmission(submission))}
      />
    | _ => React.null
    }}
    <div className="bg-gray-100 pt-9 pb-8 px-3 -mt-7">
      <div className="bg-gray-100 static md:sticky md:top-0">
        <div className="max-w-3xl mx-auto">
          <div className="flex flex-col md:flex-row items-end lg:items-center py-4">
            <div
              ariaLabel="status-tab"
              className="course-review__status-tab w-full md:w-auto flex rounded-lg border border-gray-400">
              <button
                className={buttonClasses(state.selectedTab == #Pending)}
                onClick={_ => send(SelectPendingTab)}>
                {tc("pending") |> str} {submissionsCount(state.pendingSubmissions)}
              </button>
              <button
                className={buttonClasses(state.selectedTab == #Reviewed)}
                onClick={_ => send(SelectReviewedTab)}>
                {tc("reviewed") |> str}
              </button>
            </div>
          </div>
          <div className="md:flex w-full items-start pb-4">
            <div className="flex-1">
              <label className="block text-tiny font-semibold uppercase">
                {tc("filter_by") |> str}
              </label>
              <Multiselect
                id="filter"
                unselected={unselected(levels, teamCoaches, allTags, Coach.id(currentCoach), state)}
                selected={selected(state, Coach.id(currentCoach))}
                onSelect={onSelectFilter(send)}
                onDeselect={onDeselectFilter(send)}
                value=state.filterString
                onChange={filterString => send(UpdateFilterString(filterString))}
                placeholder={filterPlaceholder(state)}
              />
            </div>
            {submissionsSorter(state, send)}
          </div>
        </div>
      </div>
      <div className="max-w-3xl mx-auto">
        {restoreAssignedToMeFilter(state, send, currentTeamCoach)}
      </div>
      <div className="max-w-3xl mx-auto">
        <CoursesReview__SubmissionsTab
          courseId
          selectedTab=state.selectedTab
          selectedLevel=state.filter.selectedLevel
          selectedCoach=state.filter.selectedCoach
          sortBy={makeSortBy(state)}
          levels
          search=state.filter.nameOrEmail
          tags=state.filter.tags
          submissions={displayedSubmissions(state)}
          reloadAt=state.reloadAt
          updateSubmissionsCB={(
            ~submissions,
            ~selectedTab,
            ~hasNextPage,
            ~totalCount,
            ~endCursor,
          ) => send(SetSubmissions(submissions, selectedTab, hasNextPage, endCursor, totalCount))}
        />
      </div>
    </div>
  </div>
}
