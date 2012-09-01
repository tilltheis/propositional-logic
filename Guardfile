guard :shell do
  watch /.*\.l?hs$/ do |m|
    puts "building..."
    `(cd src && hastec --out=../app.js app.hs)`
    puts "done"
  end
end
