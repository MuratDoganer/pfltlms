FactoryBot.define do
  factory :answer do
    description { Faker::Lorem.paragraph }
    question
    creator
  end
end
