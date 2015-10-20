function ViewModel() {
    var self = this;
    self.goal = ko.observable(null);
    self.numbers = ko.observableArray();
    self.isRunning = ko.observable(false);
    self.isWaiting = ko.observable(false);
    self.secondsLeft = ko.observable(null);
    self.scores = ko.observableArray();

    self.resetValues = function() {
	self.goal(null);
	self.numbers.removeAll();
	self.isStartable(false);
	self.isRunning(false);
	self.isWaiting(false);
	self.secondsLeft(null);
	self.scores.removeAll();
    };

    self.setValues = function(res) {
	if (res) {
	    self.goal(res.goal);
	    self.numbers(res.availableNrs);
	    self.isRunning(res.isRunning);
	    self.isWaiting(res.isWaiting);
	    self.secondsLeft(res.secondsLeft);
	    self.scores(res.scoreBoard);
	} else {
	    self.resetValue();
	}
    };

    self.queryState = function () {
	timer.pause();
	$.ajax({
	    url: "/api/current", 
	    cache: false,
	    success: function(res) {
		self.setValues(res);
		timer.play();
	    }
	}).fail(function() {
	    self.resetValues();
	    timer.play();
	});
    };
    
    var timer = $.timer(self.queryState, 500, true);
};

$(function() {
    ko.applyBindings(new ViewModel());
});
