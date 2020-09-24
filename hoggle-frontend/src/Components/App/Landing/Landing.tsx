import React from "react";
import {
  ChangeEventFunction,
  SubmitEventFunction,
  ClickEventFunction,
} from "../../../types";
import ErrorList from "./ErrorList/ErrorList";
import TextInput from "../TextInput/TextInput";
import TextInputForm from "../TextInputForm/TextInputForm";

type LandingProps = {
  nickname: string;
  lobbyCode: string;
  nicknameErrors?: string[];
  lobbyCodeErrors?: string[];
  handleChangeFunction: ChangeEventFunction;
  joinLobbyFunction: SubmitEventFunction;
  createLobbyFunction: ClickEventFunction;
};

const Landing = (props: LandingProps) => (
  <div className="landing">
    <ErrorList
      nicknameErrors={props.nicknameErrors}
      lobbyCodeErrors={props.lobbyCodeErrors}
    />
    <TextInput
      label={"Nickname:"}
      name={"nickname"}
      value={props.nickname}
      valid={props.lobbyCodeErrors ? false : true}
      handleChangeFunction={props.handleChangeFunction}
      info={"Max 16 chars. [a-zA-Z0-9,.?!_- ]"}
    />
    <TextInputForm
      formName={"lobbyCode"}
      inputs={[{ name: "lobbyCode", value: props.lobbyCode }]}
      autocompleteOff={true}
      handleChangeFunction={props.handleChangeFunction}
      handleSubmitFunction={props.joinLobbyFunction}
    />
    <p>or</p>
    <button onClick={props.createLobbyFunction}>Create New Lobby</button>
  </div>
);

export default Landing;
