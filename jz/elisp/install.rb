#! /usr/bin/env ruby
require 'fileutils'

if $*.delete('-h') || $*.delete('--help')
  print <<"EOM"
Usage: #{$0} [OPTIONS] source...
Options:
  -p, --do-not-pull   Suppress pull action. (no)
  -o, --compile-only  Suppress pull and link action. (no)
  -c, --copy          Make copy instead of link. (no)
  -v, --verbose       Ask when override. (no)
  -s, --silent        Supress messages. (no)
  --directory=<dir>   Add <dir> to the list of load path.
  --install=<dir>     Target directory. (../site-lisp)
  --emacs=<bin>       Emacs binary path. (emacs)
EOM
  exit
end

class Info
  def initialize(silent, verbose) @s = silent; @v = verbose end
  def puts(msg) Kernel.puts(msg) unless @s end
  def ask(msg) return !@v || print(msg) || $stdin.gets =~ /^y/ end
end

dontpull = $*.delete('-p') || $*.delete('--do-not-pull')
compileonly = $*.delete('-o') || $*.delete('--compile-only')
copy = $*.delete('-c') || $*.delete('--copy')
verbose = $*.delete('-v') || $*.delete('--verbose')
s = $*.delete('-s') || $*.delete('--silent')
verbose = verbose && !s
info = Info.new(s, verbose)

arg=nil
install = $*.reject!{|x|x=~/^--install=(.*)$/&&arg=$1} && arg || '../site-lisp'
emacs = $*.reject!{|x|x=~/^--emacs=(.*)$/&&arg=$1} && arg || 'emacs'
loadpath = $*.reject!{|x|x=~/^--directory=(.*)$/&&arg=$1} && arg
emacs = emacs + " -L #{loadpath}" if loadpath

source = $*
source.push('*/') if source.size < 1

Dir.chdir(File.dirname($0))
source.map!{|dir|Dir.glob(dir)}.flatten.each do |dir|
  unless compileonly || dontpull
    info.puts("pull")
    info.puts(`cd #{dir}; git pull`)
    info.puts("install from #{dir}")
  end
  info.puts('')

  Dir.glob(File.join(dir, '*.el')) do |f|
    target = File.join(install, File.basename(f))
    src = File.expand_path(f)
    unless compileonly
      ovw_msg = proc{|name| "#{name} already exists. overwrite? (y/n) "}
      write = [ target, target+'c' ].all? do |t|
        !(File.exist?(t) || File.symlink?(t))||
          ( info.ask(ovw_msg.call(t)) && File.unlink(t) )
      end
      if write
        if copy
          info.puts("copy from #{src} to #{target}")
          FileUtils.copy(src, target)
        else
          info.puts("make link from #{src} to #{target}")
          File.symlink(src, target)
        end
      end
    end
    info.puts("compile for #{dir}")
    info.puts(`#{emacs} -batch -f batch-byte-compile #{target}`)
  end
end
