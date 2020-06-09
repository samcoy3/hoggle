import React, { FormEvent, ChangeEvent, MouseEvent } from "react";
import "./main.css";

interface LandingProps {
  lobbyCode: string;
  onJoinChange: (event: ChangeEvent<HTMLInputElement>) => void;
  onJoinSubmit: (event: FormEvent<HTMLFormElement>) => void;
  onCreateClick: (event: MouseEvent<HTMLButtonElement>) => void;
}

function Landing({
  lobbyCode,
  onJoinChange: handleJoinChange,
  onJoinSubmit: handleJoinSubmit,
  onCreateClick: handleCreateClick,
}: LandingProps): JSX.Element {
  return (
    <div className="Landing">
      <JoinForm
        lobbyCode={lobbyCode}
        onChange={handleJoinChange}
        onSubmit={handleJoinSubmit}
      />
      <CreateLobbyButton onClick={handleCreateClick} />
    </div>
  );
}

interface JoinFormProps {
  lobbyCode: string;
  onChange: (event: ChangeEvent<HTMLInputElement>) => void;
  onSubmit: (event: FormEvent<HTMLFormElement>) => void;
}

function JoinForm({
  lobbyCode,
  onChange: handleChange,
  onSubmit: handleSubmit,
}: JoinFormProps): JSX.Element {
  return (
    <form className="JoinLobbyForm" onSubmit={handleSubmit}>
      <label>
        Lobby Code:
        <input type="text" value={lobbyCode} onChange={handleChange} />
      </label>
      <input type="submit" value="Join" />
    </form>
  );
}

interface CreateLobbyButtonProps {
  onClick: (event: MouseEvent<HTMLButtonElement>) => void;
}

function CreateLobbyButton({
  onClick: handleClick,
}: CreateLobbyButtonProps): JSX.Element {
  return (
    <button className="NewLobbyButton" onClick={handleClick}>
      Create Lobby
    </button>
  );
}
export default Landing;
