import React from "react";
import { ChangeEventFunction, SubmitEventFunction } from "./types";

type TextInputProps = {
  name: string;
  value: string;
  label?: string;
  info?: string;
  valid?: boolean;
  handleChangeFunction: ChangeEventFunction;
};

const TextInput = (props: TextInputProps) => (
  <div className="text-input-wrapper">
    {props.label ? <label htmlFor={props.name}>{props.label}</label> : null}
    <input
      className={`text-input ${props.valid === false ? "invalid" : ""}`}
      type="text"
      name={props.name}
      value={props.value}
      onChange={props.handleChangeFunction}
    />
    {props.info ? (
      <div className="info">
        <small>{props.info}</small>
      </div>
    ) : null}
  </div>
);

type TextInputFormProps = {
  formName: string;
  inputs: Array<{ name: string; value: string; label?: string }>;
  handleChangeFunction: ChangeEventFunction;
  handleSubmitFunction: SubmitEventFunction;
};

const TextInputForm = (props: TextInputFormProps) => {
  const getInputs = props.inputs.map((input) => (
    <TextInput
      name={input.name}
      label={input.label}
      value={input.value}
      handleChangeFunction={props.handleChangeFunction}
    />
  ));
  return (
    <div className="form-wrapper">
      <form
        name={props.formName}
        onSubmit={props.handleSubmitFunction}
        noValidate
      >
        <div>{getInputs}</div>
        <input type="submit" value="Sumbit" />
      </form>
    </div>
  );
};

export { TextInput, TextInputForm };
