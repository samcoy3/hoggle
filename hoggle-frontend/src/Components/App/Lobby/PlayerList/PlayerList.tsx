import React from "react";

type PlayerListProps = {
  players: string[];
};

const PlayerList = (props: PlayerListProps) => {
  const listPlayers = props.players.map((player, i) => (
    <h4 className="player-name" key={i}>
      {player}
    </h4>
  ));
  return (
    <div id="lobby-players">
      <h2 id="lobby-players-title">Players In Lobby:</h2>
      <div id="player-list">{listPlayers}</div>
    </div>
  );
};

export default PlayerList;
