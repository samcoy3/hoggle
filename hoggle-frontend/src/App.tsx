import React, { Component, FormEvent, ChangeEvent, MouseEvent } from "react";
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
  lobbyCode: string;
}

class App extends Component<AppProps, AppState> {
  constructor(props: AppProps) {
    super(props);
    this.state = {
      gameState: props.gameState,
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
    alert("Creating new lobby");
    // TODO: create new lobby
  }

  render() {
    const gameState: GameState = this.state.gameState;
    const lobbyCode: string = this.state.lobbyCode;
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
                return (
                  <Landing
                    lobbyCode={lobbyCode}
                    onJoinChange={this.handleJoinChange}
                    onJoinSubmit={this.handleJoinSubmit}
                    onCreateClick={this.handleCreateClick}
                  />
                );
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
