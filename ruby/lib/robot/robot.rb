require_relative "./state"
require_relative "./parser"

module ToyRobot

  class Robot

    # check if the robot is on the table
    def on_table?
      !@state.nil? && @state.valid?
    end

    # check if current state matches given parameters
    def current_state?(x, y, direction)
      @state.current_state?(x, y, direction)
    end

    # run a command in the form of an array, where the first element is the
    # command name as a symbol and all other elements are optional arguments
    def execute(cmd)
      send(*cmd)
    end

    # run all the given parsed commands
    def execute_all(cmds)
      cmds.map { |cmd| execute(cmd) }
          .select { |result| result.is_a? String and !result.empty? }
    end

    # run simulation with commands in string format
    def Robot.simulate(cmds)
      parsed = Parser.parse_cmds(cmds)
      robot = Robot.new
      robot.execute_all(parsed)
    end

    private

    def place(x, y, direction)
      state = State.new(x, y, direction)
      @state = state if state.valid?
    end

    def report
      @state.to_s
    end

    # dispatch commands: MOVE, LEFT, RIGHT
    def method_missing(action)
      if [:move, :left, :right].include? action
        @state = @state.send(action) if on_table?
      end
    end
  end

end

