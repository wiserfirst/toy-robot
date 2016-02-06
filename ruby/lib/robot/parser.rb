module ToyRobot

  class Parser

    def Parser.parse_line(cmd)
      cmd = cmd.strip
      if matches = /PLACE\s(\d),(\d),([[:upper:]]{4,5})/.match(cmd)
        [:place, matches[1].to_i, matches[2].to_i, matches[3].to_sym]
      elsif ["MOVE", "LEFT", "RIGHT", "REPORT"].include?(cmd)
        [cmd.downcase.to_sym]
      else
        []
      end
    end
  end

  def Parser.parse_cmds(cmds)
    cmds.map { |cmd| self.parse_line cmd }
        .select { |result| result != [] }
  end

end
