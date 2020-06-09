import React, { Component } from "react";
// import logo from "./logo.svg";
import "./App.css";

import Landing from "./Landing";
import Lobby from "./Lobby";
import Game from "./Game";

export enum GameState {
  InLanding,
  InLobby,
  InGame,
}

interface AppProps {
  gameState: GameState;
}

interface AppState {
  gameState: GameState;
}

class App extends Component<AppProps, AppState> {
  constructor(props: AppProps) {
    super(props);
    this.state = {
      gameState: props.gameState,
    };
  }

  render() {
    const gameState: GameState = this.state.gameState;
    return (
      <div className="App">
        <header className="App-header">
          {/* <img src={logo} className="App-logo" alt="logo" /> */}
          <h1>Hoggle</h1>
          <h2>An Online Multiplayer Boggle Game</h2>
        </header>
        <body className="App-body">
          {(() => {
            switch (gameState) {
              case GameState.InLanding:
                return <Landing />;
              case GameState.InLobby:
                return <Lobby />;
              case GameState.InGame:
                return <Game />;
              default:
                return null;
            }
          })()}
        </body>
      </div>
    );
  }
}

export default App;
