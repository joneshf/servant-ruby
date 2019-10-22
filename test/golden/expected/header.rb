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

      def post(moth:)
        req = Net::HTTP::Post.new(post_uri())
        req["moth"] = moth

        @http.request(req)
      end
    end
  end
end
