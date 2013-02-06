class Foo
  def bar
    puts "original bar method"
    private_method
  end

  # http://en.wikibooks.org/wiki/Ruby_Programming/Syntax/Classes#Private
  private
  def private_method
    puts "here's my private method"
  end
end

f = Foo.new
f.bar

def f.bar
  puts "later bar method"
  # accessing a private method from inside a public method is OK
  private_method
end

f.bar

begin
  f.private_method
rescue
  puts "You can't access a private method directly"
end
