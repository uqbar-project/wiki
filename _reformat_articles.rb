#!/usr/bin/ruby

class String
  def capitalize_first_letter
    self.slice(0,1).capitalize + self.slice(1..-1)
  end
end

def write_article_header(f, article_name)
  f.write "---\n"
  f.write "layout: article\n"
  f.write "title: #{article_name}\n"
  f.write "---\n"
  f.write "\n"
end


#List all the documents in
Dir['wiki/articles/*'].each do |file_name|
  next if File.directory? file_name

  suffix = 'wiki/articles/'
  _article_name = file_name.sub(/\..*/, '')
  _article_name = _article_name[suffix.length..-1].gsub('-',' ')

  File.open(file_name, "r") do |orig|
    File.unlink(file_name)
    File.open(file_name, "w") do |new|
      write_article_header new, _article_name.capitalize_first_letter
      new.write(orig.read())
    end
  end

end

