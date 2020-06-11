export function validNickname(nickname: string): boolean {
  const regex = new RegExp(/^[a-zA-Z0-9,.?!\-_ ]{1,16}$/);
  return regex.test(nickname);
}
