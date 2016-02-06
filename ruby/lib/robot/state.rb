module ToyRobot

  class State

    DIMENSION = 5 # table dimension

    def initialize(x, y, direction)
      @x = x
      @y = y
      @direction = direction
    end

    def current_state?(x, y, direction)
      @x == x && @y == y && @direction == direction
    end

    def to_s
      "#{@x},#{@y},#{@direction}"
    end

    def valid?
      @x.is_a?(Integer) && @x >= 0 && @x < DIMENSION &&
      @y.is_a?(Integer) && @y >= 0 && @y < DIMENSION &&
      [:NORTH, :SOUTH, :WEST, :EAST].include?(@direction)
    end

    def move
      result = self.clone
      case @direction
      when :NORTH
        result.set_y(@y+1)
      when :EAST
        result.set_x(@x+1)
      when :SOUTH
        result.set_y(@y-1)
      when :WEST
        result.set_x(@x-1)
      end
      if result.valid?
        result
      else
        result = self
      end
    end

    def left
      case @direction
      when :NORTH
        set_direction(:WEST)
      when :EAST
        set_direction(:NORTH)
      when :SOUTH
        set_direction(:EAST)
      when :WEST
        set_direction(:SOUTH)
      end
      self
    end

    def right
      case @direction
      when :NORTH
        set_direction(:EAST)
      when :EAST
        set_direction(:SOUTH)
      when :SOUTH
        set_direction(:WEST)
      when :WEST
        set_direction(:NORTH)
      end
      self
    end

    def set_x(x)
      @x = x
    end

    def set_y(y)
      @y = y
    end

    def set_direction(direction)
      @direction = direction
    end
  end

end
