module InputReader
  INPUT_FILES = {
    input: 'input.txt',
    test: 'test.txt'
  }.freeze

  def self.read
    path = INPUT_FILES[ARGV[0]&.to_sym || :input]
    File.readlines path, chomp: true
  end
end

# e.g., ruby part_2.rb test
# InputReader.read.reject!(&:empty?)
