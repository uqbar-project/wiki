module Jekyll
  module CategoryFilter
    def category_url(category)
      site = @context.registers[:site]
      site.config['categories_dir'] ? "/#{site.config['categories_dir']}/#{category.tr(' ', '-')}/" : "/categories/#{category.tr(' ', '-')}/"
    end
  end
end

Liquid::Template.register_filter(Jekyll::CategoryFilter)