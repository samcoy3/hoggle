export function validNickname(
  nickname: string
): { valid: boolean; errors?: string } {
  const regex = new RegExp(/^[a-zA-Z0-9,.?!\-_ ]{1,16}$/);

  if (nickname.length < 1) {
    return { valid: false, errors: "Please enter a nickname" };
  } else if (nickname.length > 16) {
    return { valid: false, errors: "Nickname is too long" };
  } else if (!regex.test(nickname)) {
    return { valid: false, errors: "Nickname uses invalid characters" };
  }
  return { valid: true };
}
