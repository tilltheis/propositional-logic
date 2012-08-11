guard :shell do
  watch /.*\.l?hs$/ do |m|
    `uhc -tjs -i 'lib/uhc-js/uhc-js/src;src' -o /dev/null src/App.hs`
  end
end
