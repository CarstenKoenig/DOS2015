function ViewModel() {
    var self = this;
    self.gotBusted = ko.observable(false);

    self.goal = ko.observable(null);
    self.numbers = ko.observableArray();
    self.isRunning = ko.observable(false);
    self.isWaiting = ko.observable(false);
    self.secondsLeft = ko.observable(0);
    self.scores = ko.observableArray();

    self.formula = ko.observable("");
    self.result = ko.observable(0);
    self.error = ko.observable("");

    self.resetValues = function() {
	self.goal(null);
	self.numbers.removeAll();
	self.isRunning(false);
	self.secondsLeft(0);
	self.scores.removeAll();
    };

    self.setValues = function(res) {
	if (res) {
	    self.goal(res.goal);
	    self.numbers(res.availableNrs);
	    self.isWaiting(res.isWaiting);
	    self.isRunning(res.isRunning);
	    self.secondsLeft(res.secondsLeft);

	    if (!self.isRunning()) {
		self.formula("");
		self.result("");
		self.error("");
		self.scores(res.scoreBoard);
	    } else {
		self.scores.removeAll();
	    }
	} else {
	    self.resetValue();
	}
    };

    self.setError = function (err) {
	if (err.responseText) {
	    self.error(err.responseText);
	} else {
	    self.error("Verbindung verloren!");
	}
    };

    self.queryState = function () {
	if (!self.gotBusted()) {
	    timer.pause();
	    $.ajax({
		url: "/api/current", 
		cache: false,
		success: function(res) {
		    self.setValues(res);
		    timer.play();
		}
	    }).fail(function(err) {
		self.resetValues();
		self.setError(err);
		self.bust();
	    });
	}
    };

    self.eval = function () {
	if (!self.gotBusted()) {
	    var f = self.formula();
	    self.error("");
	    self.result("");
	    $.get("/api/eval/" + encodeURIComponent(f), null, function(res) {
		if (!res) {
		    self.error("keine Antwort");
		} else if (res.info != "OK") {
		    self.error(res.info);
		} else {
		    self.result(res.value);
		}
	    }).fail(self.setError);
	}
    };

    self.bust = function () {
	self.gotBusted(true);
	timer.stop();
	self.resetValues();
    };

    var timer = $.timer(self.queryState, 500, true);
};

$(function() {
    ko.applyBindings(new ViewModel());
});
