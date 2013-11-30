var ROWS = 20; // y values
var COLS = 30; // x values
var NUM_OF_TRAINING_RECEIVED = 10; // number of training_received msgs required before considered complete
var GARBAGE_INTERVAL = 200; // in milliseconds
var mapArray = zero2D(COLS, ROWS);
var server;


$(function() {
	// automatically sets url to the server
	var url = 'ws://' + location.hostname + ':8080/websocket';
	server = new FancyWebSocket(url);
	
	// when server returns a list of trainers
	server.bind('trainers', function(trainers) {
		var $trainers = $('#trainers');
		$trainers.empty();
		$.each(trainers, function(index, value) {
			$trainers.append($('<option />').val(index).text(value));
		});
	});
	
	// when server returns a list of trained coordinates
	server.bind('trained_coords', function(list) {
		list.forEach(function(coord) {
			$('#map td[x=' + coord[0] + '][y=' +  coord[1] + ']').attr({
				status: 'complete',
				'class': 'complete'
			});
		});
	});
	
	// when the server starts training
	server.bind('training_started', function(trainer) {
		console.log(trainer);
		$('#map td[x=' + trainer[1] + '][y=' + trainer[2] + ']').attr({
			status: 'inprogress',
			'class': 'inprogress'
		});
	});
	
	// when the server receives a complete training packet from capture nodes
	server.bind('training_received', function(trainer) {
		console.log(trainer);
		mapArray[trainer[1]][trainer[2]]++;
		if (mapArray[trainer[1]][trainer[2]] == NUM_OF_TRAINING_RECEIVED) {
			toggleGarbage(false);
			server.send('end_training', [trainer[0], trainer[1], trainer[2]]);
		};
	});
	
	// when the server stops training
	server.bind('training_ended', function(trainer) {
		$('#map td[x=' + trainer[1] + '][y=' + trainer[2] + ']').attr({
			status: 'complete',
			'class': 'complete'
		});
	});

	// when connection to the server is made
	server.bind('open', function() {
		$('#connection').html('Connected');
		createButtons();
		server.send('get_trainers', null);
		server.send('get_trained_coords', null);
	});
	
	// when connection to the server is broken
	server.bind('close', function() {
		$('#connection').html('Disconnected');
		toggleGarbage(false);
	});
});

function zero2D(rows, cols) {
	var array = [],
		row = [];
	while (cols--) row.push(0);
	while (rows--) array.push(row.slice());
	return array;
}

function createButtons() {
	$('#map').empty();
	for (var j = 0; j < ROWS; j++) {
		$('#map').append('<tr>');
		for (var i = 0; i < COLS; i++) {
			var $cell = $('<td>').attr({
				x: i,
				y: j,
				id: i.toString() + j.toString(),
				status: 'incomplete',
				'class': 'incomplete'
			}).html(i.toString() + ',' + j.toString()).click(onClick);
			$('#map tr:last-child').append($cell);
		}
	}
}

function onClick(evt) {
	var $cell = $(evt.target);
	var x = parseInt($cell.attr('x'));
	var y = parseInt($cell.attr('y'));
	if ($cell.attr('status') == 'incomplete') { // not done yet, start training
		var trainer = $('#trainers').val();
		server.send('start_training', [trainer, x, y]);
		toggleGarbage(true);
	}
}

function toggleGarbage(status) {
	if (status == false) {
		clearInterval(toggleGarbage.interval);
	} else if (status == true) {
		clearInterval(toggleGarbage.interval);
		toggleGarbage.interval = setInterval(function() {
			server.send('garbage', $.now());
		}, GARBAGE_INTERVAL);
	}
}
