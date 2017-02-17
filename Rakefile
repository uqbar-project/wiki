require "rubygems"
require "tmpdir"

require "bundler/setup"
require "jekyll"

require 'html-proofer'

# Change your GitHub reponame
GITHUB_REPONAME = "uqbar-project/wiki"
DEST_BRANCH = "gh-pages"
SOURCE_BRANCH = "master"

desc "Generate blog files"
task :generate do
  Jekyll::Site.new(Jekyll.configuration({
                                            "source"      => ".",
                                            "destination" => "_site"
                                        })).process
end

desc "Check the generated page with html proofer"
task :test do
  begin
    HTMLProofer.check_directory("./_site", {:external_only => true,
                                            :parallel => { :in_processes => 3},
                                            :url_ignore => [/uqbar-project/]}).run
  rescue => e
    puts "Task #{task_name} failed"
    puts "#{e.class}: #{e.message}"
  end
  puts "Finished running all tests"
end

desc "Generate and publish blog to gh-pages"
task :publish => [:generate] do
  Dir.mktmpdir do |tmp|
    puts "Created temporal directory #{tmp}"

    cp_r "_site/.", tmp

    pwd = Dir.pwd
    Dir.chdir tmp

    system "git init"

    #Create CNAME File
    system "echo wiki.uqbar.org > CNAME"

    system "git add ."
    message = "Site updated at #{Time.now}"
    system "git commit -m #{message.inspect}"
    system "git remote add origin git@github.com:#{GITHUB_REPONAME}.git"
    puts "Pushing to origin #{DEST_BRANCH}"
    system "git push -f origin #{SOURCE_BRANCH}:#{DEST_BRANCH}"

    Dir.chdir pwd
  end
end
