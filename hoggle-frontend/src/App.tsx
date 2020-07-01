import React, { Component, FormEvent, ChangeEvent, MouseEvent } from "react";
import { Async } from "react-async";
import "./App.css";

import { join, newLobby } from "./requests";

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
  valid: { nickname?: boolean; lobbyCode?: boolean; server?: boolean };
  errors: { nickname: string[]; lobbyCode: string[]; server: string[] };
  uuid: string;
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

  validateNickname(nickname: string) {
    const maxLength = 16;
    const regex = new RegExp(/^[a-zA-Z0-9,.?!\-_ ]+$/);

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
        this.setState({ nickname: value });
        this.validateNickname(value);
        break;
      case "lobbyCode":
        this.setState({ lobbyCode: value });
        break;
      default:
        alert("Unknown text field name: " + name);
        break;
    }
  }

  async joinLobby(event: FormEvent<HTMLFormElement>): Promise<void> {
    event.preventDefault();
    this.validateNickname(this.state.nickname);
    this.validateLobbyCode();

    let valid = this.state.valid;

    if (valid.nickname && valid.lobbyCode) {
      let errors = this.state.errors;
      errors.server = [];
      let response = await join(this.state.nickname, this.state.lobbyCode);
      alert(response.data);
      if (response.success) {
        valid.server = true;
        this.setState({
          gameState: GameState.InLobby,
          uuid: response.data,
          errors: errors,
          valid: valid,
        });
      } else {
        errors.server.push(response.data);
        valid.server = false;
        this.setState({ errors: errors, valid: valid });
      }
    }
  }

  async createLobby(): Promise<void> {
    this.validateNickname(this.state.nickname);
    let valid = this.state.valid;
    if (valid.nickname) {
      let errors = this.state.errors;
      errors.server = [];
      let response = await newLobby(this.state.nickname);
      alert(response.data)
      if (response.success) {
        valid.server = true;
        this.setState({
          gameState: GameState.InLobby,
          uuid: response.data,
          errors: errors,
          valid: valid,
        });
      } else {
        errors.server.push(response.data);
        valid.server = false;
        this.setState({ errors: errors,
          valid: valid, });
      }
    }
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
