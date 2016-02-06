require 'robot/robot'

module ToyRobot

  describe Robot do

    let(:robot) { Robot.new }
    describe "new robot object" do
      it "should not be on table" do
        expect(robot.on_table?).to eq false
      end
    end

    describe "#place" do
      it "should be ignored with invalid x or y" do
        robot.execute([:place, 0, -2, :SOUTH])
        expect(robot.on_table?).to eq false
      end

      it "should be ignored with invalid direction" do
        robot.execute([:place, 0, 2, :UNKNOWN])
        expect(robot.on_table?).to eq false
      end

      it "should be on table with correct arguments" do
        robot.execute([:place, 3, 2, :EAST])
        expect(robot.on_table?).to eq true
      end
    end

    describe "#move" do
      it "should be ignored if not on table" do
        robot.execute([:move])
        expect(robot.on_table?).to eq false
      end

      it "should be ignored if moving out of table from NORTH edge" do
        robot.execute([:place, 2, 4, :NORTH])
        robot.execute([:move])
        expect(robot.on_table?).to eq true
        expect(robot.current_state?(2, 4, :NORTH)).to be true
      end

      it "should be ignored if moving out of table from EAST edge" do
        robot.execute([:place, 4, 1, :EAST])
        robot.execute([:move])
        expect(robot.on_table?).to eq true
        expect(robot.current_state?(4, 1, :EAST)).to be true
      end

      it "should be ignored if moving out of table from SOUTH edge" do
        robot.execute([:place, 3, 0, :SOUTH])
        robot.execute([:move])
        expect(robot.on_table?).to eq true
        expect(robot.current_state?(3, 0, :SOUTH)).to be true
      end

      it "should be ignored if moving out of table from WEST edge" do
        robot.execute([:place, 0, 4, :WEST])
        robot.execute([:move])
        expect(robot.on_table?).to eq true
        expect(robot.current_state?(0, 4, :WEST)).to be true
      end

      it "should move forward one unit if still on table" do
        robot.execute([:place, 1, 2, :EAST])
        robot.execute([:move])
        expect(robot.on_table?).to eq true
        expect(robot.current_state?(2, 2, :EAST)).to be true
      end
    end

    describe "#left" do
      it "should be ignored if not on table" do
        robot.execute([:left])
        expect(robot.on_table?).to eq false
      end

      it "should move turn left if on table" do
        robot.execute([:place, 3, 4, :SOUTH])
        robot.execute([:left])
        expect(robot.on_table?).to eq true
        expect(robot.current_state?(3, 4, :EAST)).to be true
      end
    end

    describe "#right" do
      it "should be ignored if not on table" do
        robot.execute([:right])
        expect(robot.on_table?).to eq false
      end

      it "should move turn right if on table" do
        robot.execute([:place, 2, 3, :WEST])
        robot.execute([:right])
        expect(robot.on_table?).to eq true
        expect(robot.current_state?(2, 3, :NORTH)).to be true
      end
    end

    describe "#report" do
      it "should be ignored if not on table" do
        robot.execute([:report])
        expect(robot.on_table?).to eq false
      end

      it "should return current state as string if on table" do
        robot.execute([:place, 2, 1, :NORTH])
        status = robot.execute([:report])
        expect(status).to eq "2,1,NORTH"
        expect(robot.on_table?).to eq true
        expect(robot.current_state?(2, 1, :NORTH)).to be true
      end
    end

    describe "#execute_all" do
      it "sample a should give expected output" do
        sequence = [
          [:place, 0, 0, :NORTH],
          [:move],
          [:report],
        ]
        expect(robot.execute_all(sequence)).to eq ["0,1,NORTH"]
      end

      it "sample b should give expected output" do
        sequence = [
          [:place, 0, 0, :NORTH],
          [:left],
          [:report],
        ]
        expect(robot.execute_all(sequence)).to eq ["0,0,WEST"]
      end

      it "sample c should give expected output" do
        sequence = [
          [:place, 1, 2, :EAST],
          [:move],
          [:move],
          [:left],
          [:move],
          [:report],
        ]
        expect(robot.execute_all(sequence)).to eq ["3,3,NORTH"]
      end
    end

    describe "run simulation for commands as strings" do
      it "should give no output without a valid PLACE command" do
        cmds = [
          "PLACE 0,5,SOUTH",
          "MOVE",
          "LEFT",
          "REPORT",
        ]
        expect(Robot.simulate(cmds)).to eq []
      end

      it "should ignore all commands before a valid PLACE command" do
        cmds = [
          "RIGHT",
          "MOVE",
          "LEFT",
          "REPORT",
          "PLACE 1,3,EAST\n",
          "MOVE\t",
          "LEFT\n",
          "REPORT\t",
        ]
        expect(Robot.simulate(cmds)).to eq ["2,3,NORTH"]
      end

      it "should ignore MOVE commands which result in the robot \
          falling from the table" do
        cmds = [
          "PLACE 4,1,WEST\n",
          "LEFT\n",
          "LEFT\n",
          "MOVE\t",
          "REPORT\t",
          "LEFT\n",
          "MOVE\t",
          "MOVE\t",
          "REPORT\t",
        ]
        expect(Robot.simulate(cmds)).to eq ["4,1,EAST", "4,3,NORTH"]
      end

      it "should give correct output with a valid PLACE command" do
        cmds = [
          "PLACE 2,4,WEST",
          "MOVE\n",
          "LEFT",
          "MOVE\n",
          "REPORT",
          "MOVE\n",
          "RIGHT",
          "REPORT\n",
        ]
        expect(Robot.simulate(cmds)).to eq ["1,3,SOUTH", "1,2,WEST"]
      end
    end
  end

end
