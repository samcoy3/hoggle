import React, { Component, FormEvent, ChangeEvent, MouseEvent } from "react";
import "./App.css";

import Landing from "./Landing";
import Lobby from "./Lobby";
import Game from "./Game";

export type ChangeEventFunction = (
  event: ChangeEvent<HTMLInputElement>
) => void;
export type SubmitEventFunction = (event: FormEvent<HTMLFormElement>) => void;
export type ClickEventFunction = (event: MouseEvent<HTMLButtonElement>) => void;

export enum GameState {
  InLanding,
  InLobby,
  InGame,
}

type AppProps = {
};

type AppState = {
  gameState: GameState;
  nickname: string;
  lobbyCode: string;
};

class App extends Component<AppProps, AppState> {
  constructor(props: AppProps) {
    super(props);
    this.state = {
      gameState: GameState.InLanding,
      nickname: "",
      lobbyCode: "",
    };
    this.handleTextChange = this.handleTextChange.bind(this);
    this.joinLobby = this.joinLobby.bind(this);
    this.createLobby = this.createLobby.bind(this);
  }

  handleTextChange(event: ChangeEvent<HTMLInputElement>): void {
    const { name, value } = event.target;
    switch (name) {
      case "nickname":
        this.setState({ nickname: value });
        break;
      case "lobbyCode":
        this.setState({ lobbyCode: value });
        break;
      default:
        alert("Unknown text field name: " + name);
        break;
    }
  }

  joinLobby(event: FormEvent<HTMLFormElement>): void {
    event.preventDefault();
    const nickname = this.state.nickname;
    const lobbyCode = this.state.lobbyCode;
    alert(`Welcome to lobby ${lobbyCode}, ${nickname}`);
    // TODO: join lobby
  }

  createLobby(): void {
    alert("Creating new lobby");
    // TODO: create new lobby
  }

  render() {
    const gameState: GameState = this.state.gameState;
    const nickname: string = this.state.nickname;
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
                    nickname={nickname}
                    lobbyCode={lobbyCode}
                    handleChangeFunction={this.handleTextChange}
                    joinLobbyFunction={this.joinLobby}
                    createLobbyFunction={this.createLobby}
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
