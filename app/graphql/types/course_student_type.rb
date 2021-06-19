module Types
  class CourseStudentType < Types::BaseObject
    field :id, ID, null: false
    field :name, String, null: false
    field :email, String, null: false
    field :title, String, null: false
    field :affiliation, String, null: true
    field :avatar_url, String, null: true
    field :user_tags, [String], null: false
    field :issued_certificates, [Types::IssuedCertificateType], null: false

    def issued_certificates
      # rubocop:disable Lint/UselessAssignment
      BatchLoader::GraphQL
        .for(object.user_id)
        .batch(default_value: []) do |user_ids, loader|
          IssuedCertificate
            .where(user_id: user_ids, certificate: object.course.certificates)
            .order('created_at DESC')
            .each do |issued_certificate|
              loader.call(issued_certificate.user_id) do |memo|
                memo |= [issued_certificate]
              end
            end
        end
      # rubocop:enable Lint/UselessAssignment
    end

    def avatar_url
      BatchLoader::GraphQL
        .for(object.user_id)
        .batch do |user_ids, loader|
          User
            .includes(avatar_attachment: :blob)
            .where(id: user_ids)
            .each do |user|
              if user.avatar.attached?
                url =
                  Rails
                    .application
                    .routes
                    .url_helpers
                    .rails_representation_path(
                    user.avatar_variant(:thumb),
                    only_path: true
                  )
                loader.call(user.id, url)
              end
            end
        end
    end

    def user_tags
      BatchLoader::GraphQL
        .for(object.user_id)
        .batch do |user_ids, loader|
          tags =
            User
              .joins(taggings: :tag)
              .where(id: user_ids)
              .distinct('tags.name')
              .select(:id, 'array_agg(tags.name)')
              .group(:id)
              .reduce({}) do |acc, user|
                acc[user.id] = user.array_agg
                acc
              end
          user_ids.each { |id| loader.call(id, tags.fetch(id, [])) }
        end
    end
  end
end
