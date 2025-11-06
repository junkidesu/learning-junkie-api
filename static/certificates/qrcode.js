function generateVerificationQr(verificationUrl) {
  new QRCode(document.getElementById("qrcode"), {
    text: verificationUrl,
    width: 100,
    height: 100,
    colorDark: "#000000",
    colorLight: "#ffffff",
    correctLevel: QRCode.CorrectLevel.H,
  });
}
