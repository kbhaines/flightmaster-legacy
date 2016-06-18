StringBuffer str = new StringBuffer("");
int j, uidLen;
int code;
uidLen = userID.length();
if (uidLen>10) {
/* chop out first 5 and last 5 characters */
for (j=0;j<5;j++) {
str.append(userID.charAt(j));
}
for (j=0;j<5;j++) {
str.append(userID.charAt(j+uidLen-5));
}
} else {
str.append(userID);
}
code = 49151;
for (j=0;j<str.length();j++) {
code += str.charAt(j)*str.charAt(j)*(89*j+2579);
}
result = Integer.toString(code & 0xFFFF);
