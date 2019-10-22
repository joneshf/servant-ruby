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

      def get_uri(beetle)
        URI("#{@origin}?beetle")
      end

      def get(beetle)
        req = Net::HTTP::Get.new(get_uri(beetle))

        @http.request(req)
      end
    end
  end
end
