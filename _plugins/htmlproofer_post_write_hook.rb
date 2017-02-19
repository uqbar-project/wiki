require 'html-proofer'

module Jekyll
  module HtmlProofer
    # run after any other hook
    HIGHEST_PRIORITY = Jekyll::Hooks::PRIORITY_MAP[:high] + 10

    Jekyll::Hooks.register(:site, :post_write, priority: HIGHEST_PRIORITY) do |site|
      begin
        options = {:external_only => true,
                   :parallel => {:in_processes => 6},
                   :cache => {:timeframe => '2w'},
                   :typhoeus => {:headers => {"User-Agent" => "Mozilla/5.0 (compatible; My New User-Agent)"}},
                   :url_ignore => []}
        HTMLProofer.check_directory(site.dest, options).run if site.config['watch']
      rescue Exception => e
        STDERR.puts e
      end
    end
  end
end
