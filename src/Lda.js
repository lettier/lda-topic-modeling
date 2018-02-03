/*
  (C) 2018 David Lettier
  lettier.com
  LDA Topic Modeling
*/

exports._performLdaAsyncViaWorker = function(input) {
  return function(onSuccess) {
  return function() {
    var worker = new Worker('Worker.js');

    function success(message) {
      onSuccess(message.data.result)();
    }

    worker.addEventListener("message", success);
    worker.postMessage({ "input": input });
  };
  };
};

exports._performLdaAsyncViaRAF = function(input) {
  return function(performLdaInitialization) {
  return function(performLdaIteration) {
  return function(ldaIterationOutputToLdaOutput) {
  return function(onSuccess) {
  return function() {
    var iterations = input.iterations;
    var init = performLdaInitialization(input)();
    var result = performLdaIteration(init)();
    var lastIteration = result.iteration;

    function successReturn(result) {
      onSuccess(ldaIterationOutputToLdaOutput(result))();
    }

    if (typeof iterations !== "number" || iterations < 0) {
      successReturn(result);
      return;
    }

    function step() {
      if (iterations === -1) {
        successReturn(result);
      } else {
        result = performLdaIteration(result)();

        if (result.iteration !== lastIteration + 1) {
          throw "_performLdaAsyncViaRAF: iteration did not progress!";
        }
        lastIteration = result.iteration;
        iterations -= 1;

        window.requestAnimationFrame(step);
      }
    }

    window.requestAnimationFrame(step);
  };
  };
  };
  };
  };
};
