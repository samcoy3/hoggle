export enum LobbyState {
  Waiting,
  Playing,
}

export type Lobby = {
  lobbyState: LobbyState;
  players: string[];
  changeAt: 0 | Date;
};

export type JoinError = {
  nickname: string[];
  lobbyCode: string[];
};

export function validateNickname(nickname: string): true | string[] {
  const maxLength = 16;
  const regex = new RegExp(/^[a-zA-Z0-9,.?!\-_ ]+$/);

  if (!nickname) {
    return ["Please enter a nickname"];
  }

  var errors: string[] = [];
  if (nickname.length > maxLength) {
    errors.push("Nickname is too long");
  }
  if (!regex.test(nickname)) {
    errors.push("Nickname uses invalid characters");
  }

  if (errors.length > 0) {
    return errors;
  }

  return true;
}

function validateLobbyCode(lobbyCode: string): true | string[] {
  const regex = new RegExp(/^[a-zA-Z]+$/);

  if (!lobbyCode) {
    return ["Please enter a lobby code"];
  }

  var errors: string[] = [];
  if (lobbyCode.length !== 4) {
    errors.push("Lobby code must be 4 characters long");
  }
  if (!regex.test(lobbyCode)) {
    errors.push("Lobby code must only contain letters");
  }

  if (errors.length > 0) {
    return errors;
  } else if (!lobbyExists(lobbyCode)) {
    return ["Lobby not found"];
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
        return { nickname: ["Nickname already in use"], lobbyCode: [] };
      default:
        return playerID;
    }
  }

  // Else return error message(s)
  let errors: JoinError = { nickname: [], lobbyCode: [] };
  if (nicknameReturn !== true) {
    errors.nickname = nicknameReturn;
  }
  if (lobbyCodeReturn !== true) {
    errors.lobbyCode = lobbyCodeReturn;
  }
  return errors;
}

export const fetchLobby = async (lobbyCode: String) => {
  return true;
};
