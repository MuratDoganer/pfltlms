module CourseExports
  class PrepareService
    def initialize(course_export)
      @course_export = course_export
    end

    def execute
      spreadsheet = RODF::Spreadsheet.new
      add_custom_styles(spreadsheet)

      spreadsheet.table 'Targets' do |table|
        populate_targets_table(table)
      end

      spreadsheet.table 'Students' do |table|
        populate_students_table(table)
      end

      spreadsheet.table 'Submissions' do |table|
        populate_submissions_table(table)
      end

      io = StringIO.new(spreadsheet.bytes)

      @course_export.file.attach(io: io, filename: filename, content_type: 'application/vnd.oasis.opendocument.spreadsheet')
    end

    private

    def course
      @course_export.course
    end

    def tags
      @course_export.tags.pluck(:name)
    end

    def add_custom_styles(spreadsheet)
      spreadsheet.office_style 'passing-grade', family: :cell do
        property :cell, 'background-color' => "#9AE6B4"
      end

      spreadsheet.office_style 'failing-grade', family: :cell do
        property :cell, 'background-color' => "#FEB2B2"
      end

      spreadsheet.office_style 'pending-grade', family: :cell do
        property :cell, 'background-color' => "#FAF089"
      end
    end

    def filename
      name = "#{course.name}-#{Time.zone.now.iso8601}".parameterize
      "#{name}.ods"
    end

    def populate_targets_table(table)
      values = targets.map do |target|
        milestone = target.target_group.milestone ? 'Yes' : 'No'

        [
          target_id(target),
          target.level.number,
          target.title,
          target_type(target),
          milestone,
          students_with_submissions(target),
          submissions_pending_review(target)
        ] + average_grades_for_target(target)
      end

      rows = ([
        ["ID", "Level", "Name", "Completion Method", "Milestone?", "Students with submissions", "Submissions pending review"] + evaluation_criteria_names
      ] + values).transpose

      table.add_rows(rows)
    end

    def evaluation_criteria_names
      @evaluation_criteria_names ||= EvaluationCriterion.where(id: evaluation_criteria_ids).map do |ec|
        ec.name + " (Average Grade)"
      end
    end

    def evaluation_criteria_ids
      @evaluation_criteria_ids ||= targets.map do |target|
        target.evaluation_criteria.pluck(:id)
      end.flatten.uniq
    end

    def average_grades_for_target(target)
      empty_grades = Array.new(evaluation_criteria_ids.length)

      target.evaluation_criteria.pluck(:id).each_with_object(empty_grades) do |evaluation_criterion_id, grades|
        grades[evaluation_criteria_ids.index(evaluation_criterion_id)] = TimelineEventGrade.joins(:timeline_event).where(timeline_events: { target_id: target.id }, evaluation_criterion_id: evaluation_criterion_id).average(:grade).round(2)
      end
    end

    def average_grades_for_student(student)
      evaluation_criteria_ids.map do |evaluation_criterion_id|
        TimelineEventGrade.joins(timeline_event: :founders).where(founders: { id: student.id }, evaluation_criterion_id: evaluation_criterion_id).average(:grade)&.round(2)
      end
    end

    def students_with_submissions(target)
      target.timeline_events.joins(:founders).distinct('founders.id').count('founders.id')
    end

    def submissions_pending_review(target)
      target.timeline_events.pending_review.distinct.count
    end

    def populate_students_table(table)
      student_rows = students.map do |student|
        user = student.user

        [
          user.email,
          user.name,
          user.title,
          user.affiliation,
          student.tags.pluck(:name).join(', ')
        ] + average_grades_for_student(student)
      end

      table.row do |row|
        row.add_cells ["Email Address", "Name", "Title", "Affiliation", "Tags"] + evaluation_criteria_names
      end

      table.add_rows student_rows
    end

    def populate_submissions_table(table)
      # Lay out the top row of target IDs.
      table.row do |row|
        row.cell 'Student Email / Target ID'

        formatted_target_ids = targets.map do |target|
          target_id(target)
        end

        row.add_cells(formatted_target_ids)
      end

      target_ids = targets.pluck(:id)

      # Now populate status for each student.
      students.each do |student|
        grading = compute_grading_for_submissions(student, target_ids)

        table.row do |row|
          row.add_cells([student.user.email] + grading)
        end
      end
    end

    # rubocop:disable Metrics/CyclomaticComplexity
    def compute_grading_for_submissions(student, target_ids)
      TimelineEvent.where(latest: true).includes(:timeline_event_grades)
        .joins(:founders).where(founders: { id: student.id }).distinct
        .each_with_object([]) do |submission, grading|
        grade_index = target_ids.index(submission.target_id)

        # We can't record grades for submissions where the target has been archived.
        next if grade_index.nil?

        evaluation_grade = submission.timeline_event_grades.pluck(:grade).join(',')

        grade, style = case [submission.passed_at.present?, evaluation_grade.empty?]
          when [true, true]
            [submission.quiz_score || '✓', '']
          when [true, false]
            [evaluation_grade, 'passing-grade']
          when [false, true]
            %w[RP pending-grade]
          when [false, false]
            [evaluation_grade, 'failing-grade']
        end

        grading[grade_index] = { value: grade, style: style }
      end
    end

    # rubocop:enable Metrics/CyclomaticComplexity

    def students
      @students ||= begin
        # Only scan 'active' students. Also filter by tag, if applicable.
        scope = course.founders.active.includes(:user)
        tags.present? ? scope.tagged_with(tags, any: true) : scope
      end.order('users.email')
    end

    def targets
      @targets ||= begin
        scope = course.targets.live
          .joins(:level)
          .includes(:level, :evaluation_criteria, :quiz, :target_group)

        scope = @course_export.reviewed_only ? scope.joins(:evaluation_criteria) : scope

        scope.order('levels.number ASC, target_groups.sort_index ASC, targets.sort_index ASC').load
      end
    end

    def target_id(target)
      "L#{target.level.number}T#{target.id}"
    end

    def target_type(target)
      if target.evaluation_criteria.present?
        'Graded'
      elsif target.quiz.present?
        'Take Quiz'
      elsif target.link_to_complete.present?
        'Visit Link'
      else
        'Mark as Complete'
      end
    end
  end
end
