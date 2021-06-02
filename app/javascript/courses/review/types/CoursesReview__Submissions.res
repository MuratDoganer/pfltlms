module Level = CoursesReview__Level

type filter = {
  level: option<Level.t>,
  coach: option<UserProxy.t>,
  search: option<string>,
  tags: Belt.Set.String.t,
}

let makeFilter = (level, coach, search, tags) => {
  level: level,
  coach: coach,
  search: search,
  tags: tags,
}

let filterLevelId = level => level |> OptionUtils.mapWithDefault(Level.id, "none")
let filterCoachId = coach => coach |> OptionUtils.mapWithDefault(UserProxy.id, "none")

let filterEq = (level, coach, filter) =>
  filter.level |> filterLevelId == filterLevelId(level) &&
    filter.coach |> filterCoachId == filterCoachId(coach)

type data = {
  submissions: array<CoursesReview__IndexSubmission.t>,
  filter: filter,
  sortBy: CoursesReview__SubmissionsSorting.t,
  totalCount: int,
}

type cursor = string

type t =
  | Unloaded
  | PartiallyLoaded(data, cursor)
  | FullyLoaded(data)

let totalCount = t =>
  switch t {
  | Unloaded => None
  | PartiallyLoaded({totalCount}, _)
  | FullyLoaded({totalCount}) =>
    Some(totalCount)
  }

let unloaded = Unloaded

let partiallyLoaded = (
  ~submissions,
  ~filter,
  ~sortBy,
  ~search,
  ~tags,
  ~totalCount,
  ~cursor,
) => PartiallyLoaded(
  {
    submissions: submissions,
    filter: filter,
    sortBy: sortBy,
    totalCount: totalCount,
    search: search,
    tags: tags,
  },
  cursor,
)

let fullyLoaded = (~submissions, ~filter, ~sortBy, ~search, ~tags, ~totalCount) => FullyLoaded({
  submissions: submissions,
  filter: filter,
  sortBy: sortBy,
  search: search,
  tags: tags,
  totalCount: totalCount,
})

let needsReloading = (selectedLevel, selectedCoach, sortBy, search, tags, t) =>
  switch t {
  | Unloaded => true
  | FullyLoaded(data)
  | PartiallyLoaded(data, _) =>
    !(
      data.filter |> filterEq(selectedLevel, selectedCoach) &&
      data.sortBy == sortBy &&
      data.search == search &&
      data.tags == tags
    )
  }

let toArray = t =>
  switch t {
  | Unloaded => []
  | PartiallyLoaded(data, _cursor) => data.submissions
  | FullyLoaded(data) => data.submissions
  }
