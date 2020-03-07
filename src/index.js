var app = Elm.Donate.init();

var stripe = Stripe("pk_test_abcdefghijklmnopqrstuvwxyz");

app.ports.checkout.subscribe(function(id) {
  stripe
    .redirectToCheckout({
      sessionId: id
    })
    .then(function(result) {
      app.ports.failure.send(result.error.message);
    });
});
