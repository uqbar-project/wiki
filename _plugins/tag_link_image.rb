#
# Provides a tag for linking images
#

module Jekyll

  module Article

    class LinkImage < Liquid::Tag

      def initialize(tag_name, text, tokens)
        super
        @text = text
      end

      def render(context)
        @site = context.registers[:site]
        image_name = @text

        %Q{<img src="#{@site.config["baseurl"]}/img/wiki/#{image_name}" title="#{image_name}" alt="">}
      end

    end

  end

end

Liquid::Template.register_tag('link_image', Jekyll::Article::LinkImage)


