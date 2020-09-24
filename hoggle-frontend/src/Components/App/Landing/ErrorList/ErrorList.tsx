import React from "react";

type ErrorListProps = {
  nicknameErrors?: string[];
  lobbyCodeErrors?: string[];
};

const ErrorList = (props: ErrorListProps) => {
  let errors;
  if (props.nicknameErrors && props.lobbyCodeErrors) {
    // If nickname and lobby errors, combine lists
    errors = props.nicknameErrors?.concat(props.lobbyCodeErrors);
  } else if (props.nicknameErrors) {
    // If only nickname errors, use nickname errors list
    errors = props.nicknameErrors;
  } else if (props.lobbyCodeErrors) {
    // If only lobby code errors, use lobby code errors list
    errors = props.lobbyCodeErrors;
  } else {
    // If no errors, return null
    return null;
  }
  const listItems = errors.map((error, i) => <li key={i}>{error}</li>);
  return (
    <div className="join-error-wrapper">
      <ul className="error">{listItems}</ul>
    </div>
  );
};

export default ErrorList;
