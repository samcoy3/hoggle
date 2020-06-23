import React from "react";
import "./main.css";
import {
  ChangeEventFunction,
  SubmitEventFunction,
  ClickEventFunction,
} from "./App";

type LandingProps = {
  nickname: string;
  lobbyCode: string;
  valid: { nickname?: boolean; lobbyCode?: boolean };
  errors: { nickname?: string; lobbyCode?: string };
  handleChangeFunction: ChangeEventFunction;
  joinLobbyFunction: SubmitEventFunction;
  createLobbyFunction: ClickEventFunction;
};

const Landing = (props: LandingProps) => (
  <div className="landing">
    {props.valid.nickname === false ? (
      <p className="error">{props.errors.nickname}</p>
    ) : null}
    {props.valid.lobbyCode === false ? (
      <p className="error">{props.errors.lobbyCode}</p>
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
    <button onClick={props.createLobbyFunction}>Create New Lobby</button>
  </div>
);

type TextInputProps = {
  label: string;
  name: string;
  value: string;
  valid?: boolean;
  handleChangeFunction: ChangeEventFunction;
  info?: string;
};

const TextInput = (props: TextInputProps) => (
  <div className="text-input-wrapper">
    <label htmlFor={props.name}>{props.label}</label>
    <input
      className={`text-input ${props.valid === false ? "invalid" : ""}`}
      type="text"
      name={props.name}
      value={props.value}
      onChange={props.handleChangeFunction}
    />
    {props.info ? (
      <div className="info">
        <small>{props.info}</small>
      </div>
    ) : null}
  </div>
);

type JoinFormProps = {
  lobbyCode: string;
  handleChangeFunction: ChangeEventFunction;
  joinLobbyFunction: SubmitEventFunction;
};

const JoinForm = (props: JoinFormProps) => (
  <div className="form-wrapper">
    <form onSubmit={props.joinLobbyFunction} noValidate>
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
