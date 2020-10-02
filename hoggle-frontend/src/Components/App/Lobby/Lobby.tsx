import React from "react";
import {
  ButtonClickEventFunction,
  LastRound,
  SubmitEventFunction,
  ChangeEventFunction,
  NewSettings,
} from "../../../types";
import AdminPanel from "../AdminPanel/AdminPanel";
import PlayerList from "./PlayerList/PlayerList";
import LastRoundScores from "./LastRoundScores/LastRoundScores";

type LobbyProps = {
  playerNames: string[];
  lastRound?: LastRound;
  newSettings?: NewSettings;
  handleChangeFunction: ChangeEventFunction;
  handleSubmitFunction: SubmitEventFunction;
  startGameFunction: ButtonClickEventFunction;
};

const Lobby = (props: LobbyProps) => {
  return (
    <div id="lobby">
      {props.newSettings ? (
        <AdminPanel
          location="Lobby"
          newSettings={props.newSettings}
          handleChangeFunction={props.handleChangeFunction}
          handleSubmitFunction={props.handleSubmitFunction}
          gameFunction={props.startGameFunction}
        />
      ) : null}
      <PlayerList players={props.playerNames} />
      {props.lastRound ? (
        <LastRoundScores
          playerNames={props.playerNames}
          lastRound={props.lastRound}
        />
      ) : null}
    </div>
  );
};

export default Lobby;
