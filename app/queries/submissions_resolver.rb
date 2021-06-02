class SubmissionsResolver < ApplicationQuery
  property :course_id
  property :status
  property :sort_direction
  property :sort_criterion
  property :level_id
  property :coach_id
  property :search
  property :tags

  def submissions
    applicable_submissions
      .includes(
        :startup_feedback,
        founders: %i[user startup],
        target: :target_group
      )
      .distinct
      .order("#{sort_criterion_string} #{sort_direction_string}")
  end

  def authorized?
    return false if current_user.faculty.blank?

    current_user.faculty.courses.exists?(id: course)
  end

  def sort_direction_string
    case sort_direction
    when 'Ascending'
      'ASC'
    when 'Descending'
      'DESC'
    else
      raise "#{sort_direction} is not a valid sort direction"
    end
  end

  def sort_criterion_string
    case sort_criterion
    when 'SubmittedAt'
      'created_at'
    when 'EvaluatedAt'
      'evaluated_at'
    else
      raise "#{sort_criterion} is not a valid sort criterion"
    end
  end

  def course
    @course ||= Course.find_by(id: course_id)
  end

  def teams_by_tag
    teams = course.startups.active.joins(founders: :user)

    return teams if tags.blank?

    user_tags =
      tags.intersection(
        course
          .users
          .joins(taggings: :tag)
          .distinct('tags.name')
          .pluck('tags.name')
      )

    team_tags =
      tags.intersection(
        course
          .startups
          .joins(taggings: :tag)
          .distinct('tags.name')
          .pluck('tags.name')
      )

    intersect_teams = user_tags.present? && team_tags.present?

    teams_with_user_tags =
      teams
        .where(
          users: {
            id: resource_school.users.tagged_with(user_tags).select(:id)
          }
        )
        .pluck(:id)

    teams_with_tags = teams.tagged_with(team_tags).pluck(:id)

    if intersect_teams
      teams.where(id: teams_with_user_tags.intersection(teams_with_tags))
    else
      teams.where(id: teams_with_user_tags + teams_with_tags)
    end
  end

  def applicable_submissions
    by_level =
      if level_id.present?
        course.levels.where(id: level_id).first.timeline_events
      else
        course.timeline_events
      end

    by_level_and_status =
      case status
      when 'Pending'
        by_level.pending_review
      when 'Reviewed'
        by_level.evaluated_by_faculty
      else
        raise "Unexpected status '#{status}' encountered when resolving submissions"
      end

    by_level_status_and_coach =
      if coach_id.present?
        by_level_and_status
          .joins(founders: { startup: :faculty_startup_enrollments })
          .where(faculty_startup_enrollments: { faculty_id: coach_id })
      else
        by_level_and_status
      end

    by_level_status_and_coach.from_founders(students)
  end

  def course_teams
    if search.present?
      teams_by_tag
        .where('users.name ILIKE ?', "%#{search}%")
        .or(teams_by_tag.where('startups.name ILIKE ?', "%#{search}%"))
        .or(teams_by_tag.where('users.email ILIKE ?', "%#{search}%"))
    else
      teams_by_tag
    end
  end

  def students
    @students ||= Founder.where(startup_id: course_teams)
  end

  def allow_token_auth?
    true
  end
end
