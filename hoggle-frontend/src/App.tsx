import React, { Component, FormEvent, ChangeEvent, MouseEvent } from "react";
import "./App.css";

import { validateNickname, joinLobby, JoinError } from "./helpers";

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
  JoiningLobby,
}

type AppProps = {};

type AppState = {
  gameState: GameState;
  nickname: string;
  lobbyCode: string;
  valid: { nickname?: boolean; lobbyCode?: boolean };
  errors: { nickname: string[]; lobbyCode: string[] };
  playerId: number;
};

class App extends Component<AppProps, AppState> {
  constructor(props: AppProps) {
    super(props);
    this.state = {
      gameState: GameState.InLanding,
      nickname: "",
      lobbyCode: "",
      valid: {},
      errors: { nickname: [], lobbyCode: [] },
      playerId: -1,
    };
    this.handleTextChange = this.handleTextChange.bind(this);
    this.joinLobby = this.joinLobby.bind(this);
    this.createLobby = this.createLobby.bind(this);
  }

  handleTextChange(event: ChangeEvent<HTMLInputElement>): void {
    const { name, value } = event.target;
    switch (name) {
      case "nickname":
        let valid = this.state.valid;
        let errors = this.state.errors;
        const nicknameReturn = validateNickname(value);
        if (nicknameReturn === true) {
          valid.nickname = true;
          errors.nickname = [];
        } else {
          valid.nickname = false;
          errors.nickname = nicknameReturn;
        }
        this.setState({ nickname: value, valid: valid, errors: errors });
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
    const lobbycode = this.state.lobbyCode;
    let valid = this.state.valid;
    let errors = this.state.errors;

    const joinResponse = joinLobby(nickname, lobbycode);
    if (typeof joinResponse === "number") {
      valid.nickname = true;
      valid.lobbyCode = true;
      errors.nickname = [];
      errors.lobbyCode = [];
      this.setState({
        playerId: joinResponse,
        gameState: GameState.JoiningLobby,
        valid: valid,
        errors: errors,
      });
      // TODO get lobby info
      this.setState({ gameState: GameState.InLobby });
    } else {
      errors.nickname = joinResponse.nickname;
      errors.lobbyCode = joinResponse.lobbyCode;
      if (errors.nickname === []) {
        valid.nickname = true;
      } else {
        valid.nickname = false;
      }
      if (errors.lobbyCode === []) {
        valid.lobbyCode = true;
      } else {
        valid.lobbyCode = false;
      }
      this.setState({ valid: valid, errors: errors });
    }
  }

  createLobby(): void {
    alert("Creating new lobby");
    // TODO: create new lobby
  }

  render() {
    return (
      <div className="App">
        <header className="App-header">
          {/* <img src={logo} className="App-logo" alt="logo" /> */}
          <h1>Hoggle</h1>
          <h2>An Online Multiplayer Boggle Game</h2>
        </header>
        <body className="App-body">
          {(() => {
            switch (this.state.gameState) {
              case GameState.InLanding:
                return (
                  <Landing
                    nickname={this.state.nickname}
                    lobbyCode={this.state.lobbyCode}
                    valid={this.state.valid}
                    errors={this.state.errors}
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
