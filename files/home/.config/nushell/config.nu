# config.nu
#
# Installed by:
# version = "0.107.0"
#
# This file is used to override default Nushell settings, define
# (or import) custom commands, or run any other startup tasks.
# See https://www.nushell.sh/book/configuration.html
#
# Nushell sets "sensible defaults" for most configuration settings, 
# so your `config.nu` only needs to override these defaults if desired.
#
# You can open this file in your default editor using:
#     config nu
#
# You can also pretty-print and page through the documentation for configuration
# options using:
#     config nu --doc | nu-highlight | less -R

$env.config.show_banner = false

alias l = ls -al

alias gst = git status
alias gd  = git diff

def timestamp [] {
  date now | date to-timezone UTC | format date %Y%m%d-%H%M%S
}

alias mstar = C:\Users\shaws8\scoop\shims\bash.exe mstar

$env.EDITOR = 'nvim'
$env.config.edit_mode = 'vi'

$env.PATH = ($env.PATH | append (echo ~/Code/steshaw/larva-dev-notes/script | path expand))





# Common aliases

alias mstar-build-fastest = mvn -T 1C -P dev -P skipTestCompile install
alias mstar-build-fastest = mvn -T 1C -P dev -P skipTestCompile install

alias mstar-clean-build-fastest = mvn -T 1C -P dev -P skipTestCompile clean install
alias mstar-clean-build-fastest = mvn -T 1C -P dev -P skipTestCompile clean install

alias mstar-build-fast = mvn -T 1C -P dev -D skipTests install
alias mstar-build-fast = mvn -T 1C -P dev -D skipTests install

alias mstar-clean-build-fast = mvn -T 1C -P dev -D skipTests clean install
alias mstar-clean-build-fast = mvn -T 1C -P dev -D skipTests clean install

alias mstar-build = mvn -T 1C -D skipTests install
alias mstar-build = mvn -T 1C -D skipTests install


match $nu.os-info.name {
  'windows' => { 
    alias idea = d:/.local/ideaIU-2025.2.win/bin/idea64.exe

    # This is added in user environment variables, so no need to do it here too.
    #PATH=/d/.local/bin:$PATH

    $env.CDPATH = [
      "."
      ~/Code/steshaw
      ~/Code
      "d:/Code/minestar"
      "d:/Code"
      "c:/sbox"
    ]
  }
  _         => {
    $env.CDPATH = [
      "."
      ~/Code/steshaw
      ~/Code
    ]

    if ($env.WSL_DISTRO_NAME? | is-not-empty) {

      # WSL-specific.
      print "Running under WSL"
      export-env {
        $env.CNI_PATH = $"($nu.home-path)/.local/nerdctl-2.1.3/libexec/cni"
      }
      alias idea = /mnt/d/.local/ideaIU-2025.2.win/bin/idea64.exe
    } else {
      print "Not WSL"
    }
  }
}
