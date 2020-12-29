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

      def get_uri(vw_beetle: false)
        URI("#{@origin}?#{vw_beetle ? 'vw-beetle' : ''}")
      end

      def get(vw_beetle: false)
        req = Net::HTTP::Get.new(get_uri(vw_beetle: vw_beetle))

        @http.request(req)
      end
    end
  end
end
