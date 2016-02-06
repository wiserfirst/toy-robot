require 'robot/state'

module ToyRobot

  describe State do

    describe "#current_state?" do
      it "should return false when attributes not all equal" do
        state = State.new(1, 2, :NORTH)
        expect(state.current_state?(2, 3, :NORTH)).not_to be true
      end

      it "should return true when attributes all equal" do
        state = State.new(1, 2, :SOUTH)
        expect(state.current_state?(1, 2, :SOUTH)).to be true
      end
    end

    describe "#to_s" do
      it "should return the string representation of a state" do
        state = State.new(1, 2, :NORTH)
        expect(state.to_s).to eql "1,2,NORTH"
        state = State.new(4, 3, :WEST)
        expect(state.to_s).to eql "4,3,WEST"
      end
    end

    describe "#valid?" do
      it "should be invalid if x or y is not integer" do
        state = State.new(1.1, 3, :NORTH)
        expect(state.valid?).to eq false
        state = State.new(4, 'abc', :EAST)
        expect(state.valid?).to eq false
      end

      it "should be invalid if x or y is out of boundary" do
        state = State.new(-1, 3, :SOUTH)
        expect(state.valid?).to eq false
        state = State.new(4, 6, :WEST)
        expect(state.valid?).to eq false
      end

      it "should be invalid direction is invalid value" do
        state = State.new(1, 3, :CENTER)
        expect(state.valid?).to eq false
        state = State.new(1, 3, :LEFT)
        expect(state.valid?).to eq false
      end

      it "should be valid with correct arguments" do
        state = State.new(2, 4, :EAST)
        expect(state.valid?).to eq true
        state = State.new(3, 1, :NORTH)
        expect(state.valid?).to eq true
      end
    end
  end

end
