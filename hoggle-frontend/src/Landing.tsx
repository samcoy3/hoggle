import React, { Component, FormEvent, ChangeEvent, MouseEvent } from "react";
import "./main.css";

interface LandingProps {}

interface LandingState {
  lobbyCode: string;
}

class Landing extends Component<LandingProps, LandingState> {
  constructor(props: LandingProps) {
    super(props);
    this.state = {
      lobbyCode: "",
    };
    this.handleJoinChange = this.handleJoinChange.bind(this);
		this.handleJoinSubmit = this.handleJoinSubmit.bind(this);
		this.handleCreateClick = this.handleCreateClick.bind(this);
  }

  handleJoinChange(event: ChangeEvent<HTMLInputElement>): void {
    const lobbyCode = event.target.value;
    this.setState({ lobbyCode: lobbyCode });
  }

  handleJoinSubmit(event: FormEvent<HTMLFormElement>): void {
    event.preventDefault();
    const lobbyCode = this.state.lobbyCode;
		alert("Lobby code is " + lobbyCode);
		// TODO: join lobby
	}
	
	handleCreateClick(event: MouseEvent<HTMLButtonElement>): void {
		alert("Creating new lobby")
		// TODO: create new lobby
	}

  render() {
    const lobbyCode = this.state.lobbyCode;
    return (
      <div className="Landing">
        <JoinForm
          lobbyCode={lobbyCode}
          onChange={this.handleJoinChange}
          onSubmit={this.handleJoinSubmit}
        />
				<CreateLobbyButton onClick={this.handleCreateClick}/>
      </div>
    );
  }
}

interface JoinFormProps {
  lobbyCode: string;
  onChange: (event: ChangeEvent<HTMLInputElement>) => void;
  onSubmit: (event: FormEvent<HTMLFormElement>) => void;
}

function JoinForm({
  lobbyCode,
  onChange,
  onSubmit,
}: JoinFormProps): JSX.Element {
  return (
    <form className="JoinLobbyForm" onSubmit={onSubmit}>
      <label>
        Lobby Code:
        <input type="text" value={lobbyCode} onChange={onChange} />
      </label>
      <input type="submit" value="Join" />
    </form>
  );
}

interface CreateLobbyButtonProps {
  onClick: (event: MouseEvent<HTMLButtonElement>) => void;
}

function CreateLobbyButton({ onClick }: CreateLobbyButtonProps): JSX.Element {
  return (
    <button className="NewLobbyButton" onClick={onClick}>
      Create Lobby
    </button>
  );
}
export default Landing;
