class TimelineEventsController < ApplicationController
  before_action :authenticate_user!

  # GET /submissions/:id/review
  def review
    submission = authorize(TimelineEvent.find(params[:id]))
    @course = submission.target.course
    render 'courses/review', layout: 'student_course'
  end

  # GET /submissions/:id/review
  def review_v2
    submission = authorize(TimelineEvent.find(params[:id]))
    @course = submission.target.course
    render 'users/dashboard_v2', layout: 'app_router'
  end

  def show
    @submission = authorize(TimelineEvent.find(params[:id]))
    render layout: 'student'
  end
end
