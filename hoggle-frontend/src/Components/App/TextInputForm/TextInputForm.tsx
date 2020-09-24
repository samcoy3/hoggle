import React from "react";
import { ChangeEventFunction, SubmitEventFunction } from "../../../types";
import TextInput from "../TextInput/TextInput";

type TextInputFormProps = {
  formName: string;
  inputs: Array<{ name: string; value: string; label?: string }>;
  autocompleteOff?: boolean;
  handleChangeFunction: ChangeEventFunction;
  handleSubmitFunction: SubmitEventFunction;
};

const TextInputForm = (props: TextInputFormProps) => {
  const getInputs = props.inputs.map((input, i) => (
    <TextInput
      key={i}
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
        autoComplete={props.autocompleteOff ? "off" : ""}
        onSubmit={props.handleSubmitFunction}
        noValidate
      >
        <div>{getInputs}</div>
        <input type="submit" value="Submit" />
      </form>
    </div>
  );
};

export default TextInputForm;
