class ApplicationStageOneForm < Reform::Form
  property :application_page_read, virtual: true, validates: { acceptance: true }
  property :team_lead_consent, virtual: true, validates: { acceptance: true }
  property :fees_consent, virtual: true, validates: { acceptance: true }
  property :university_id, validates: { presence: true }
  properties :college, :state, validates: { presence: true, length: { maximum: 250 } }
  property :team_achievement, validates: { presence: true, length: { maximum: 1000 } }
  property :cofounder_count, virtual: true

  property :team_lead do
    property :name, validates: { presence: true, length: { maximum: 250 } }
    property :phone, validates: { presence: true, length: { is: 10 } }
    property :email, writeable: false
    property :gender, validates: { presence: true, inclusion: Founder.valid_gender_values }
    property :role, validates: { inclusion: Founder.valid_roles }
  end

  collection :cofounders, populate_if_empty: BatchApplicant do
    property :email, validates: { presence: true, length: { maximum: 250 } }
    property :name, validates: { presence: true, length: { maximum: 250 } }
    property :role, validates: { inclusion: Founder.valid_roles }
  end

  # Custom validations.
  validate :prevent_team_lead_duplicate
  validate :prevent_cofounder_duplicate
  validate :cofounder_count_must_be_valid
  validate :emails_must_look_right
  validate :phone_number_must_look_right

  def prevent_team_lead_duplicate
    cofounders.each_with_index do |cofounder, index|
      next if cofounder.email != team_lead.email
      cofounders[index].errors[:email] << 'is a duplicate of team lead'
      errors[:base] << 'One or more cofounders are invalid.'
      errors[:base].uniq!
    end
  end

  def prevent_cofounder_duplicate
    previous_cofounder_emails = []

    cofounders.each_with_index do |cofounder, index|
      if cofounder.email.in?(previous_cofounder_emails)
        cofounders[index].errors[:email] << 'has been mentioned previously'
        errors[:base] << 'One or more cofounders are invalid.'
        errors[:base].uniq!
      end

      previous_cofounder_emails << cofounder.email
    end
  end

  def cofounder_count_must_be_valid
    return if cofounders.count.in? [2, 3, 4]
    errors[:base] << 'Must have at least two, and at most four co-founders.'
    errors[:base].uniq!
  end

  def emails_must_look_right
    cofounders.each_with_index do |cofounder, index|
      next if valid_email?(cofounder.email)
      cofounders[index].errors[:email] << "doesn't look like an email"
      errors[:base] << 'One or more cofounders are invalid.'
      errors[:base].uniq!
    end
  end

  def phone_number_must_look_right
    return if team_lead.phone =~ /\A[0-9]{10}\z/
    team_lead.errors[:phone] << 'must be a 10-digit phone number'
    errors[:base] << 'Supplied contact number does not look correct.'
    errors[:base].uniq!
  end

  def valid_email?(email)
    email =~ /\S+@\S+/
  end

  def prepopulate!(options)
    self.team_lead = options[:team_lead]
    self.cofounders = [BatchApplicant.new] * 2
    self.cofounder_count = 2
  end

  def save
    BatchApplication.transaction do
      create_application
      update_team_lead
      create_cofounders
    end
  end

  def create_application
    model.update!(
      university_id: university_id,
      team_achievement: team_achievement,
      college: college,
      state: state,
      team_lead_id: team_lead.id
    )
  end

  def update_team_lead
    team_lead.model.update!(
      name: team_lead.name,
      gender: team_lead.gender,
      role: team_lead.role,
      phone: team_lead.phone
    )

    model.batch_applicants << team_lead.model
  end

  def create_cofounders
    cofounders.each do |cofounder|
      applicant = BatchApplicant.find_or_initialize_by(email: cofounder.email)

      applicant.update!(
        name: cofounder.name,
        role: cofounder.role
      )

      model.batch_applicants << applicant
    end
  end
end
