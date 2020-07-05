import React from "react";
import "./main.css";
import {
  ClickEventFunction,
  SubmitEventFunction,
  ChangeEventFunction,
} from "./types";
import { TextInputForm } from "./InputComponents";

type AdminPanelProps = {
  newSettings: { size: string; timeInSeconds: string };
  handleChangeFunction: ChangeEventFunction;
  handleSubmitFunction: SubmitEventFunction;
  startGameFunction: ClickEventFunction;
};

const AdminPanel = (props: AdminPanelProps) => (
  <div id="admin-panel">
    <div id="admin-panel-title">Admin Panel</div>
    <div id="settings-form">
      <div id="settings-title">Settings</div>
      <TextInputForm
        formName={"settings"}
        inputs={[
          {
            name: "size",
            value: props.newSettings.size,
            label: "Board Size:",
          },
          {
            name: "time",
            value: props.newSettings.timeInSeconds,
            label: "Game time (seconds):",
          },
        ]}
        handleChangeFunction={props.handleChangeFunction}
        handleSubmitFunction={props.handleSubmitFunction}
      />
    </div>
    <button id="start-button" onClick={props.startGameFunction}>
      Start Game
    </button>
  </div>
);

export { AdminPanel };
