var server;
var chart;

var colors = ["Navy", "Teal", "Lime", "Cyan", "ForestGreen", "Indigo"];

// var margin = {top: 160, right: 140, bottom: 160, left: 140},
// 	width  = 640 - margin.left - margin.right,
// 	height = 640 - margin.top - margin.bottom;
var width = 640;
var height = 640;

// Setups scale based on width and height
var x = d3.scale.linear()
	.domain([0, 20])
	.range([0, 300]);

var y = d3.scale.linear()
	.domain([30, 0])
	.range([290, 0]);

var xAxis = d3.svg.axis()
	.scale(x)
	.orient("top");

var yAxis = d3.svg.axis()
	.scale(y)
	.orient("left");
	
// Add the SVG element to the body
var $canvas = d3.select('#canvas').append('svg')
	.attr('width', width)
	.attr('height', height)
	.attr('class', 'lokey');
	
var $main = $canvas.append('g')
	.attr('transform', 'translate(128,190),rotate(-5),skewX(7),skewY(-3)')
	.attr('width', width)
	.attr('height', height)
	.attr('class', 'main');

// Draws the axes
$main.append("g")
	.attr("class", "axis")
	.call(xAxis);

$main.append("g")
	.attr("class", "axis")
	.call(yAxis);

// When DOM ready
$(function() {
	// Setup WebSocket connection
	var host = location.hostname;
	if (host == '') {
		host = 'localhost'
	}
	var url = 'ws://' + host + ':8080/websocket';
	server = new FancyWebSocket(url);
	
	
	// Binds a function to the "position" event from server
	// Called whenever the server returns a position of a device
	var i = 0;
	server.bind('position', function(position) {
		
		update([position[i++]]);
		if (i > 32)
			i = 0;
		console.log(position[i]);
	});
});

// Updates, adds, and removes points on the graph.
// Data format must be an array of x,y arrays. Example: [[.3, .7], [.2, .4]]
function update(data) {
	var node = $main.selectAll(".node")
		.data(data);
		
	node.each(function (d,i) {
		var s = d3.select(this);
		var c = s.selectAll("circle");
		var t = s.selectAll("text");

		var tranDuration = lineDistance(c.attr("cx"), c.attr("cy"), x(d.x), y(d.y)) * 10;

		c.transition()
			.ease("sin-out")
			.duration(tranDuration)
			.attr("cx", x(d.x))
			.attr("cy", y(d.y))
			.attr("r", 10);

		t.transition()
			.ease("sin-out")
			.duration(tranDuration)
			.attr("x", x(d.x))
			.attr("y", y(d.y)+20);
	});

	var enter = node.enter().append("g")
		.attr("class", "node")

	enter.append("circle")
		.attr("cx", function (d) {return x(d.x);})
		.attr("cy", function (d) {return y(d.y);})
		.attr("r", 10)
		.style("fill", function (d) {return d.c;})
		.attr("class", "point");

	enter.append("text")
		.text(function(d) { return d.l;})
		.attr("x", function(d) {return x(d.x);})
		.attr("y", function(d) {return y(d.y)+25;})
		.attr("class", "label")
		.style("text-anchor", "middle");

	// Add a fade in transition to all entering elements and child elements
	enter.selectAll("*")
		.style("opacity", 0)
		.transition()
		.duration(300)
		.style("opacity", 1)

	// Fade out and remove all exiting elements
	node.exit()
		.style("opacity", 1)
		.transition()
		.duration(300)
		.style("opacity", 0)
		.remove()
}

function lineDistance(x1, y1, x2, y2) {
	var d1 = x1 - x2;
	var d2 = y1 - y2;
	return Math.sqrt(d1 * d1 + d2 * d2);
}

function randomPoint(i) {
	var pointData = [];
	for (var j = 0; j < i; j++) {
		pointData.push({"x": Math.random()*20, "y": Math.random()*30, "l": "Point" + j, "c": colors[j]});
	}
	update(pointData);
}