class PlatformFeedbackController < ApplicationController
  before_action :require_active_subscription

  def create
    @platform_feedback = PlatformFeedback.new platform_feedback_params
    authorize @platform_feedback

    if @platform_feedback.save!
      PlatformFeedbackMailer.new_platform_feedback(@platform_feedback).deliver_later
      flash[:success] = 'Thank You! Your feedback has been sent to the SV.CO team!'
    else
      flash[:error] = 'Something went wrong while saving your feedback! Please try again.'
    end

    redirect_back(fallback_location: root_url)
  end

  private

  def platform_feedback_params
    params.require(:platform_feedback).permit(:founder_id, :feedback_type, :description, :attachment, :promoter_score)
  end
end
