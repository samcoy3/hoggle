import React, { Component, FormEvent, ChangeEvent, MouseEvent } from "react";
import "./App.css";

import { join, newLobby, info } from "./requests";
import { GameState, LobbyInfo } from "./types";

import Landing from "./Landing";
import Lobby from "./Lobby";
import Game from "./Game";

type AppProps = {};

type AppState = {
  gameState: GameState;
  nickname: string;
  lobbyCode: string;
  valid: { nickname?: boolean; lobbyCode?: boolean; server?: boolean };
  errors: { nickname: string[]; lobbyCode: string[]; server: string[] };
  uuid: string;
  lobby?: LobbyInfo;
};

class App extends Component<AppProps, AppState> {
  constructor(props: AppProps) {
    super(props);
    this.state = {
      gameState: GameState.InLanding,
      nickname: "",
      lobbyCode: "",
      valid: {},
      errors: { nickname: [], lobbyCode: [], server: [] },
      uuid: "",
    };
    this.handleTextChange = this.handleTextChange.bind(this);
    this.joinLobby = this.joinLobby.bind(this);
    this.createLobby = this.createLobby.bind(this);
  }

  validateNickname() {
    const maxLength = 16;
    const regex = new RegExp(/^[a-zA-Z0-9,.?!\-_ ]+$/);

    const nickname = this.state.nickname;
    let valid = this.state.valid;
    let errors = this.state.errors;
    errors.nickname = [];

    if (!nickname) {
      errors.nickname.push("Please enter a nickname");
    } else {
      if (nickname.length > maxLength) {
        errors.nickname.push("Nickname is too long");
      }
      if (!regex.test(nickname)) {
        errors.nickname.push("Nickname uses invalid characters");
      }
    }

    if (errors.nickname.length > 0) {
      valid.nickname = false;
    } else {
      valid.nickname = true;
    }

    this.setState({ valid: valid, errors: errors });
  }

  validateLobbyCode() {
    const regex = new RegExp(/^[a-zA-Z]+$/);

    const lobbyCode = this.state.lobbyCode;
    let valid = this.state.valid;
    let errors = this.state.errors;
    errors.lobbyCode = [];

    if (!lobbyCode) {
      errors.lobbyCode.push("Please enter a lobby code");
    } else {
      if (lobbyCode.length !== 4) {
        errors.lobbyCode.push("Lobby code must be 4 characters long");
      }
      if (!regex.test(lobbyCode)) {
        errors.lobbyCode.push("Lobby code must only contain letters");
      }
    }

    if (errors.lobbyCode.length > 0) {
      valid.lobbyCode = false;
    } else {
      valid.lobbyCode = true;
    }

    this.setState({ valid: valid, errors: errors });
  }

  handleTextChange(event: ChangeEvent<HTMLInputElement>): void {
    const { name, value } = event.target;
    switch (name) {
      case "nickname":
        this.setState({ nickname: value }, this.validateNickname);
        break;
      case "lobbyCode":
        this.setState({ lobbyCode: value.toUpperCase() });
        break;
      default:
        alert("Unknown text field name: " + name);
        break;
    }
  }

  handleUUIDReturn(response: { success: boolean; data: string }) {
    let valid = this.state.valid;
    let errors = this.state.errors;
    errors.server = [];
    if (response.success) {
      valid.server = true;
      this.setState({
        gameState: GameState.InLobby,
        uuid: response.data,
        errors: errors,
        valid: valid,
      });
      info(response.data).then((lobbyInfo: LobbyInfo) => {
        console.log("hello");
        this.setState({ lobby: lobbyInfo });
      });
    } else {
      errors.server.push(response.data);
      valid.server = false;
      this.setState({ errors: errors, valid: valid });
    }
  }

  async joinLobby(event: FormEvent<HTMLFormElement>): Promise<void> {
    event.preventDefault();
    this.validateNickname();
    this.validateLobbyCode();
    let valid = this.state.valid;
    if (valid.nickname && valid.lobbyCode) {
      join(this.state.nickname, this.state.lobbyCode).then((UUIDReturn) => {
        this.handleUUIDReturn(UUIDReturn);
      });
    }
  }

  async createLobby(): Promise<void> {
    this.validateNickname();
    let valid = this.state.valid;
    if (valid.nickname) {
      newLobby(this.state.nickname).then((UUIDReturn) => {
        this.handleUUIDReturn(UUIDReturn);
      });
    }
  }

  render() {
    return (
      <div className="App">
        <div className="App-header">
          {/* <img src={logo} className="App-logo" alt="logo" /> */}
          {(() => {
            switch (this.state.gameState) {
              case GameState.InLanding:
                return (
                  <div>
                    <h1>Hoggle</h1>
                    <h2>An Online Multiplayer Boggle Game</h2>
                  </div>
                );
              case GameState.InLobby:
                return (
                  <section className="lobby-header">
                    <h1>{this.state.lobby?.lobbyCode}</h1>
                    <h1> 1:45 </h1>
                    <h1>{this.state.nickname}</h1>
                  </section>
                );
              case GameState.InGame:
              default:
                return null;
            }
          })()}
        </div>
        <div className="App-body">
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
        </div>
      </div>
    );
  }
}

export default App;
