require 'robot/parser'

module ToyRobot

  describe Parser do

    context "parse PLACE command" do
      it "should ignore invalid PLACE command" do
        cmd = "PLACE"
        expect(Parser.parse_line(cmd)).to eq []
        cmd = "PLACE 00NORTH"
        expect(Parser.parse_line(cmd)).to eq []
      end

      it "should parse valid PLACE command" do
        cmd = "PLACE 2,1,SOUTH"
        expect(Parser.parse_line(cmd)).to eq [:place, 2, 1, :SOUTH]
      end
    end

    context "parse other commands" do
      it "should parse valid MOVE command" do
        cmd = "MOVE"
        expect(Parser.parse_line(cmd)).to eq [:move]
      end

      it "should parse valid MOVE command" do
        cmd = "MOVE"
        expect(Parser.parse_line(cmd)).to eq [:move]
      end

      it "should parse valid LEFT command" do
        cmd = "LEFT"
        expect(Parser.parse_line(cmd)).to eq [:left]
      end

      it "should parse valid RIGHT command" do
        cmd = "RIGHT"
        expect(Parser.parse_line(cmd)).to eq [:right]
      end

      it "should parse valid REPORT command" do
        cmd = "REPORT"
        expect(Parser.parse_line(cmd)).to eq [:report]
      end

      it "should parse invalid commands" do
        cmd = "MOEV"
        expect(Parser.parse_line(cmd)).to eq []
        cmd = "LIFT"
        expect(Parser.parse_line(cmd)).to eq []
        cmd = "RGIHT"
        expect(Parser.parse_line(cmd)).to eq []
        cmd = "REPROT"
        expect(Parser.parse_line(cmd)).to eq []
      end
    end

    context "parse multiple commands" do
      it "should parse multiple commands" do
        cmds = [ "PLACE 3,4,EAST",
                 "MOVE",
                 "REPORT",
                 "RIGHT",
                 "MOVE",
                 "MOVE",
                 "REPORT",
               ]
        results = [ [:place, 3, 4, :EAST],
                    [:move],
                    [:report],
                    [:right],
                    [:move],
                    [:move],
                    [:report],
                  ]
        expect(Parser.parse_cmds(cmds)).to eq results
      end
    end
  end

end
