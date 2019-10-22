require "json"
require "net/http"
require "uri"

module Generated
  module V1
    class Things
      def initialize(origin, timeout = nil)
        @origin = URI(origin)
        @http = Net::HTTP.new(@origin.host, @origin.port)

        unless timeout.nil?
          @http.open_timeout = timeout
          @http.read_timeout = timeout
        end
        @http.use_ssl = @origin.scheme == 'https'
      end

      def post_uri()
        URI("#{@origin}")
      end

      def post(body:)
        req = Net::HTTP::Post.new(post_uri())
        req["Content-Type"] = "application/json"

        @http.request(req, body)
      end
    end
  end
end
