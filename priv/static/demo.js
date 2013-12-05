var demo = {};
demo.step = 0;

var node;
var foot;

var canvas = new fabric.Canvas('canvas');

var boundingRect = new fabric.Rect({
	fill: 'transparent',
	width: 290,
	height: 290,
	opacity: 0.2
});

var map = new fabric.Group([boundingRect], {
	left: 120,
	top: 190,
	width: 290,
	height: 290,
	fill: 'transparent',
	backgroundColor: 'red',
	selectable: false,
	angle: -12
});
canvas.add(map);
canvas.setBackgroundImage('static/map/LokeyBirdeye.png', canvas.renderAll.bind(canvas));

canvas.on('mouse:up', function(options) {
	if (demo.step == 0) {
		demo.addNodes();
	} else if (demo.step == 1) {
		demo.radiateNodes();
	} else if (demo.step == 2) {
		demo.radiateNodes.abort = true;
		demo.addPhone();
	} else if (demo.step == 3) {
		demo.phonePing();
	} else if (demo.step == 4) {
		demo.addSignals();
	} else if (demo.step == 5) {
		demo.phonePing.abort = true;
		canvas.clear();
	} else if (demo.step == 6) {
		demo.addTrack();
	}
	demo.step++;
});

demo.addTrack = function() {
	var foot;
	fabric.Image.fromURL('static/map/shoeprints.png', function(img) {
		foot = img;
		foot.originX = 'center';
		foot.originY = 'center';
		foot.height = 30;
		foot.width = 16.5;
		node.add(foot);
	});
	var text = new fabric.Text('Kevin\'s iPhone', {
		fontSize: 15,
		textAlign: 'center',
		originX: 'center',
		originY: 'center'
	});
	node = new fabric.Group([text], {
		originX: 'center',
		originY: 'center',
	});
	text.top = 30;
	canvas.add(node)
	node.center();
	node.animate('opacity', 1, {
		from: 0,
		onChange: canvas.renderAll.bind(canvas),
		onComplete: function() {
			node.animate('top', 200, {
				duration: 5000,
				onChange: canvas.renderAll.bind(canvas)
			});
			node.animate('left', 300, {
				duration: 5000,
				onChange: canvas.renderAll.bind(canvas),
				onComplete: demo.trackLeft
			});		
		}
	});
}

demo.trackLeft = function() {
	node.item(1).animate('angle', -160, {
		duration: 100,
		onChange: canvas.renderAll.bind(canvas)
	});
	node.animate('top', 500, {
		duration: 8000,
		onChange: canvas.renderAll.bind(canvas)
	});
	node.animate('left', 150, {
		duration: 8000,
		onChange: canvas.renderAll.bind(canvas)
	});
}

demo.addSignals = function() {
	var circle1 = new fabric.Circle({
		top: 100,
		left: 130,
		radius: 30,
		originX: 'center',
		originY: 'center',
		fill: 'white'
	});
	var text1 = new fabric.Text('-60', {
		top: 80,
		left: 110,
	    textAlign: 'center',
		fontSize: 28
	});
	canvas.add(circle1);
	canvas.add(text1);
	var circle2 = new fabric.Circle({
		top: 350,
		left: 115,
		radius: 30,
		originX: 'center',
		originY: 'center',
		fill: 'white'
	});
	var text2 = new fabric.Text('-70', {
		top: 350,
		left: 115,
	    textAlign: 'center',
		fontSize: 28,
		originX: 'center',
		originY: 'center',
	});
	canvas.add(circle2);
	canvas.add(text2);
	var circle3 = new fabric.Circle({
		top: 350,
		left: 505,
		radius: 30,
		originX: 'center',
		originY: 'center',
		fill: 'white'
	});
	var text3 = new fabric.Text('-68', {
		top: 350,
		left: 503,
	    textAlign: 'center',
		fontSize: 28,
		originX: 'center',
		originY: 'center',
	});
	canvas.add(circle3);
	canvas.add(text3);
}

demo.addPhone = function() {
	var phone = fabric.Image.fromURL('static/map/iphone.png', function(img) {
		img.width = 50;
		img.height = 50;
		canvas.add(img);
		img.center();
	});
}

demo.phonePing = function() {
	if (demo.phonePing.abort) return;
	var tmp;
	if (!demo.phonePing.created) {
		tmp = new fabric.Circle({
			originX: 'center',
			originY: 'center',
			fill: '',
			radius: 8,
			stroke: 'purple',
			strokeWidth: 2,
			selectable: false
		});
		canvas.add(tmp);
		tmp.center();
		demo.phonePing.created = true;		
	};
	canvas.item(5).animate('radius', 300, {
		from: 8,
		onChange: canvas.renderAll.bind(canvas),
		duration: 1000
	});
	canvas.item(5).animate('opacity', 0, {
		from: 1,
		onChange: canvas.renderAll.bind(canvas),
		duration: 1000,
		onComplete: function() { if (!demo.phonePing.abort) demo.phonePing(); },
	});
}

demo.radiateNodes = function() {
	if (demo.radiateNodes.abort) return;
	var duration = 1000;
	var nodeA = canvas.item(1);
	var nodeC = canvas.item(2);
	var nodeB = canvas.item(3);
	nodeA.item(1).animate('radius', 300, {
		from: 8,
		onChange: canvas.renderAll.bind(canvas),
		duration: duration,
	}); 
	nodeA.item(1).animate('opacity', 0, {
		from: 1,
		onChange: canvas.renderAll.bind(canvas),
		duration: duration,
	});
	nodeB.item(1).animate('radius', 300, {
		from: 8,
		onChange: canvas.renderAll.bind(canvas),
		duration: duration,
	}); 
	nodeB.item(1).animate('opacity', 0, {
		from: 1,
		onChange: canvas.renderAll.bind(canvas),
		duration: duration,
	});
	nodeC.item(1).animate('radius', 300, {
		from: 8,
		onChange: canvas.renderAll.bind(canvas),
		duration: duration,
	}); 
	nodeC.item(1).animate('opacity', 0, {
		from: 1,
		onChange: canvas.renderAll.bind(canvas),
		duration: duration,
		onComplete: function() { if (!demo.radiateNodes.abort) demo.radiateNodes(); },
	});
}

demo.addNodes = function() {
	var duration = 250;
	
	var text1 = new fabric.Text('nodeA', {
		top: 20,
	    textAlign: 'center',
		fontSize: 28,
	});
	var circle1 = new fabric.Circle({
		left: 35,
		radius: 8,
		fill: 'blue',
		originX: 'center',
		originY: 'center'
	});
	var nodeA = new fabric.Group([text1, circle1], {
		left: 80,
		top: 350,
	});
	nodeA.animate('opacity', 1, {
		from: 0,
		onChange: canvas.renderAll.bind(canvas),
		duration: duration
	});
	canvas.add(nodeA);

	var text2 = new fabric.Text('nodeC', {
		top: 20,
	    textAlign: 'center',
		fontSize: 28,
	});
	var circle2 = new fabric.Circle({
		left: 35,
		radius: 8,
		fill: 'blue',
		originX: 'center',
		originY: 'center'
	});
	var nodeC = new fabric.Group([text2, circle2], {
		left: 100,
		top: 95
	});
	nodeC.animate('opacity', 1, {
		from: 0,
		onChange: canvas.renderAll.bind(canvas),
		duration: duration
	});
	canvas.add(nodeC);

	var text3 = new fabric.Text('nodeB', {
		top: 20,
	    textAlign: 'center',
		fontSize: 28,
	});
	var circle3 = new fabric.Circle({
		left: 35,
		radius: 8,
		fill: 'blue',
		originX: 'center',
		originY: 'center'
	});
	var nodeB = new fabric.Group([text3, circle3], {
		left: 470,
		top: 350
	});
	nodeB.animate('opacity', 1, {
		from: 0,
		onChange: canvas.renderAll.bind(canvas),
		duration: duration
	});
	canvas.add(nodeB);
}



demo.update = function() {
	
}

// When DOM ready
$(function() {
	// Setup WebSocket connection
	var host = location.hostname;
	if (host == '') {
		host = 'localhost'
	}
	var url = 'ws://' + host + ':8080/websocket';
	demo.server = new FancyWebSocket(url);
	
	
	// Binds a function to the "position" event from server
	// Called whenever the server returns a position of a device
	demo.server.bind('position', function(position) {
		position[0].time = ~~(Date.now() / 1000);
		demo.pointData[position[0].i] = position[0];
	});
	
	setInterval(function() {
		//var temp = [];
		for (var key in demo.pointData) {
			if (demo.pointData.hasOwnProperty(key) && ((Date.now() / 1000) - demo.pointData[key].time > 10)) {
				delete demo.pointData[key];
			}
		}
		// Keep only most recent 10 points
		//if (temp.length > 15) temp.splice(0, temp.length - 15);
		//MM.pointData = {};
		demo.update();
	}, 15);
});