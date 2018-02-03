/*
  (C) 2018 David Lettier
  lettier.com
  LDA Topic Modeling
*/

function perform(message) {
  var result = self.Lda.performLda(self.PS["Data.Eq"].eqInt)()(message.data.input)();
  self.postMessage({ "result": result });
}
self.addEventListener("message", perform);
