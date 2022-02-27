require 'irb/completion'

IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:AUTO_INDENT] = true

require "awesome_print"
AwesomePrint.defaults = {
  :indent => -2,
}
AwesomePrint.irb!
