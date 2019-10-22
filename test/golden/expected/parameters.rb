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

      def get_by_butterfly_uri(butterfly)
        butterfly = if butterfly.kind_of?(Array) then butterfly.join(',') else butterfly end

        URI("#{@origin}/#{butterfly}")
      end

      def get_by_butterfly(butterfly)
        butterfly = if butterfly.kind_of?(Array) then butterfly.join(',') else butterfly end

        req = Net::HTTP::Get.new(get_by_butterfly_uri(butterfly))

        @http.request(req)
      end
    end
  end
end
