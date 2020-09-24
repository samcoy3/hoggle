import React from "react";
import {
  ClickEventFunction,
  SubmitEventFunction,
  ChangeEventFunction,
} from "../../../types";
import TextInputForm from "../TextInputForm/TextInputForm";

type AdminPanelProps = {
  location: "Lobby" | "Game";
  newSettings: { size: string; timeInSeconds: string };
  handleChangeFunction: ChangeEventFunction;
  handleSubmitFunction: SubmitEventFunction;
  gameFunction: ClickEventFunction;
};

const AdminPanel = (props: AdminPanelProps) => (
  <div id="admin-panel">
    <h2 id="admin-panel-title">Admin Panel</h2>
    <div id="settings-form">
      <h3 id="settings-title">Settings</h3>
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
    <button id="game-button" onClick={props.gameFunction}>
      {props.location === "Lobby" ? "Start Game" : "Reroll Game"}
    </button>
  </div>
);

export default AdminPanel;
