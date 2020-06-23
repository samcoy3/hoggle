export enum LobbyState {
  Waiting,
  Playing,
}

export type Lobby = {
  lobbyState: LobbyState;
  players: [string];
  changeAt: 0 | Date;
};

type JoinError = {
  nickname: string;
  lobbyCode: string;
};

export function validateNickname(nickname: string): true | string {
  const maxLength = 16;
  const regex = new RegExp(/^[a-zA-Z0-9,.?!\-_ ]*$/);

  if (!nickname) {
    return "Please enter a nickname";
  }

  var errorString = "";
  if (nickname.length > maxLength) {
    errorString += "Nickname is too long\n";
  }
  if (!regex.test(nickname)) {
    errorString += "Nickname uses invalid characters\n";
  }

  if (errorString) {
    return errorString.slice(0, -1);
  }

  return true;
}

function validateLobbyCode(lobbyCode: string): true | string {
  const regex = new RegExp(/^[a-zA-Z]{4}$/);

  if (!lobbyCode) {
    return "Please enter a lobby code";
  }

  if (!regex.test(lobbyCode)) {
    return "Lobby code should be 4 letters";
  }

  if (!lobbyExists(lobbyCode)) {
    return "Lobby not found";
  }

  return true;
}

function lobbyExists(lobbyCode: string): boolean {
  return true;
}

export function joinLobby(
  nickname: string,
  lobbyCode: string
): number | JoinError {
  // Get true/error message for nick and lobbycode
  const nicknameReturn = validateNickname(nickname);
  const lobbyCodeReturn = validateLobbyCode(lobbyCode);

  // If nick and lobbycode both valid, get playerID
  if (nicknameReturn === true && lobbyCodeReturn === true) {
    // TODO: server function that adds player to lobby, returns playerID or error code
    let playerID = 1;
    switch (playerID) {
      case -1:
        return { nickname: "Nickname already in use", lobbyCode: "" };
      default:
        return playerID;
    }
  }

  // Else return error message(s)
  let errors = { nickname: "", lobbyCode: "" };
  if (typeof nicknameReturn === "string") {
    errors.nickname = nicknameReturn;
  }
  if (typeof lobbyCodeReturn === "string") {
    errors.lobbyCode = lobbyCodeReturn;
  }
  return errors;
}

export const fetchLobby = async (lobbyCode: String) => {
  return true;
};
