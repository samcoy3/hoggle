import React from "react";
import "./main.css";
import {
  ChangeEventFunction,
  SubmitEventFunction,
  ClickEventFunction,
} from "./types";
import { TextInput } from "./InputComponents"

type LandingProps = {
  nickname: string;
  lobbyCode: string;
  valid: { nickname?: boolean; lobbyCode?: boolean };
  errors: { nickname: string[]; lobbyCode: string[] };
  handleChangeFunction: ChangeEventFunction;
  joinLobbyFunction: SubmitEventFunction;
  createLobbyFunction: ClickEventFunction;
};

const Landing = (props: LandingProps) => (
  <div className="landing">
    {props.valid.nickname === false || props.valid.lobbyCode === false ? (
      <ErrorList errors={props.errors} />
    ) : null}
    <TextInput
      label={"Nickname:"}
      name={"nickname"}
      value={props.nickname}
      valid={props.valid.nickname}
      handleChangeFunction={props.handleChangeFunction}
      info={"Max 16 chars. [a-zA-Z0-9,.?!_- ]"}
    />
    <JoinForm
      lobbyCode={props.lobbyCode}
      handleChangeFunction={props.handleChangeFunction}
      joinLobbyFunction={props.joinLobbyFunction}
    />
    <p>or</p>
    <button onClick={props.createLobbyFunction}>Create New Lobby</button>
  </div>
);

type ErrorListProps = {
  errors: { nickname: string[]; lobbyCode: string[] };
};

const ErrorList = (props: ErrorListProps) => {
  const errors = props.errors.nickname.concat(props.errors.lobbyCode);
  const listItems = errors.map((error, i) => <li key={i}>{error}</li>);
  return (
    <div className="join-error-wrapper">
      <ul className="error">{listItems}</ul>
    </div>
  );
};

type JoinFormProps = {
  lobbyCode: string;
  handleChangeFunction: ChangeEventFunction;
  joinLobbyFunction: SubmitEventFunction;
};

const JoinForm = (props: JoinFormProps) => (
  <div className="form-wrapper">
    <form onSubmit={props.joinLobbyFunction} name={"lobby"} noValidate>
      <TextInput
        label={"Lobby Code:"}
        name={"lobbyCode"}
        value={props.lobbyCode}
        handleChangeFunction={props.handleChangeFunction}
      />
      <input type="submit" value="Join" />
    </form>
  </div>
);

export default Landing;
