#
# Provides tags for writing articles
#

module Jekyll

  module Article

    class ArticleTag < Liquid::Tag

      class << self
        attr_accessor :tag_name
      end

    end

    class ArticleBlock < Liquid::Block

      class << self
        attr_accessor :block_name
      end

    end

    #
    # ---
    #

    class SectionTag < ArticleTag

      @tag_name = "wiki_section"

      def initialize tag_name, text, tokens
        super
        @text = text
      end

      def render context
        %Q{<h1>#{@text}</h1>}
      end

    end

    class SubSectionTag < ArticleTag

      @tag_name = "wiki_subsection"

      def initialize tag_name, text, tokens
        super
        @text = text
      end

      def render context
        %Q{<h2>#{@text}</h2>}
      end

    end

    #
    # Superclass for generating alert boxes with foundation with a nice liquid
    # block
    #
    class AlertBoxBlock < ArticleBlock

      def open_div classattr
        "<div data-alert class='alert-box #{classattr}' tabindex='0' aria-live='assertive' role='alertdialog'>"
      end

      def close_div
        "</div>"
      end

    end

    #
    # For these words, classes will be generated:
    #
    #   <word capitalized>Block
    #
    # And blocks will be generated (wiki_note_<word>) which can be used as
    # liquid blocks then to generate alert box-divs for foundation.
    #
    [ "success", "warning", "info", "alert", "secondary" ].each do |str|
      klass = <<-EOS
        class #{str.capitalize}Block < AlertBoxBlock
          @block_name = "wiki_note_#{str}"

          def render context
            open_div("#{str}") + super + close_div
          end
        end
      EOS

      module_eval klass
    end

  end

end

[
  Jekyll::Article::SectionTag,
  Jekyll::Article::SubSectionTag,
].each do |klass|
  Liquid::Template.register_tag(klass.tag_name, klass)
end

[
  Jekyll::Article::SuccessBlock,
  Jekyll::Article::WarningBlock,
  Jekyll::Article::InfoBlock,
  Jekyll::Article::AlertBlock,
  Jekyll::Article::SecondaryBlock,
].each do |klass|
  Liquid::Template.register_tag(klass.block_name, klass)
end


