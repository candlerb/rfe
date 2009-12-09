require 'rake/clean'

directory 'ebin'
task :default => 'ebin'

def output_file(input_file, dir = 'ebin/')
  dir + File.basename(input_file,'.erl') + '.beam'
end

file "src/rfe_parse.erl" => ["src/rfe_parse.yrl"] do
  sh "erlc -o src src/rfe_parse.yrl"
end

SRC_FILES = Dir['src/**/*.erl']
SRC_FILES.each do |src|
  file output_file(src) => [src] do
    sh "erlc -o ebin #{src}"
  end
  task :default => output_file(src)
end

