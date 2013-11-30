var server;
var chart;
var width = 640;
var height = 640;
var colors = ["Navy", "Teal", "Lime", "Cyan", "ForestGreen", "Indigo"];

// Setups scale based on width and height
var x = d3.scale.linear()
	.domain([0, 30])
	.range([50, width-50]);

var y = d3.scale.linear()
	.domain([20, 0])
	.range([height-40, 60]);

var xAxis = d3.svg.axis()
	.scale(x)
	.orient("top");

var yAxis = d3.svg.axis()
	.scale(y)
	.orient("left");
	
// Add the SVG element to the body
var $chart = d3.select('#chart').append('svg')
	.attr('width', width)
	.attr('height', height);
	
var $main = $chart.append('g')
	.attr("class", "addborder")
	.attr('width', width)
	.attr('height', height)
	.attr('class', 'main');
//.attr('transform', 'translate(130,190),rotate(-7),skewX(5)')
// Draws the axes
$main.append("g")
	.attr("class", "axis")
	// .attr("transform", "translate(0," + height + ")")
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
	
	// Title animations for stylistic purposes
	$('#title').hide().delay(1000).fadeIn(1000).delay(1000).animate({
		opacity: 0.75,
		height: 150
	}, 1000, function() {
		$('#chart').fadeIn(1000);
	});
	$('#chart').hide();
	

});

// Updates, adds, and removes points on the graph.
// Data format must be an array of x,y arrays. Example: [[.3, .7], [.2, .4]]
function update(data) {
	var node = $main.selectAll(".node")
		.data(data);
		
	var point = node.enter().append('g')
		.attr('class', 'node');
		
	point.append('image')
		.attr('width', 24)
		.attr('height', 24)
		.attr('cx', function(d) { return x(d.x); })
		.attr('cy', function(d) { return y(d.y); })
		.attr("x", function(d) { return x(d.x) - 12; })
		.attr("y", function(d) { return y(d.y) - 12; })
		.attr('xlink:href', 'static/map/footprint.svg')
		.attr('transform', 'rotate(90,'+function(d) { return x(d.x); }+','+function(d) { return y(d.y); }+')');
	point.append('text')
		.attr('class', 'label')
		.text(function(d) { return d.l;})
		.attr("x", function(d) { return x(d.x); })
		.attr("y", function(d) { return y(d.y) + 30; })
		.style("text-anchor", "middle");
		
	node.each(function(d, i) {
		var i = d3.select(this).selectAll('image');
		var t = d3.select(this).selectAll('text');
		var oldX = i.attr('cx');
		var oldY = i.attr('cy');
		var deltaX = x(d.x) - oldX;
		var deltaY = y(d.y) - oldY;
		
		var tranDuration = lineDistance(oldX, oldY, x(d.x), y(d.y)) * 10;
		
		var oldRotation = i.attr('rotation');
		var angle = Math.atan2(-deltaY, deltaX) * 180 / Math.PI;
		console.log('the angle:' + angle+' oldrotation:'+oldRotation);
		
		d3.select(this).transition()
			//.duration(tranDuration)
			.attr('transform', 'translate('+deltaX+','+deltaY+')');
		var centerX = x(d.x);
		var centerY = y(d.y);
		
		if (angle == oldRotation) {
			// no change in angle
		} else {
			console.log('------------------');
			console.log('rotating!');
			//i.attr('transform', 'rotate('+(-oldRotation)+','+centerX+','+centerY+')')
				//i.attr('transform', 'rotate('+(angle+oldRotation)+','+centerX+','+centerY+')')
				//.attr('rotation', angle);
		}
		console.log('angle: '+angle+' rotation:' + i.attr('rotation')+' x:'+centerX+' y:'+centerY);
		
	});
		
	node.exit()
		.remove();

	node.each(function (d,i) {
		//for(o in d) {
		//	console.log("" + o + ": " + d[o]);
		//}
		/*var s = d3.select(this);
		var c = s.selectAll("circle");
		var t = s.selectAll("text");
		//var i = s.selectAll("image");

		var tranDuration = lineDistance(c.attr("cx"), c.attr("cy"), x(d.x), y(d.y)) * 10;

		var deltaX = x(d.x) - c.attr("cx");
		var deltaY = y(d.y) - c.attr("cy");
		var angleInDegrees = Math.atan2(deltaY, deltaX) * 180 / Math.PI + 90;

		s.transition()
			.ease("sin-out")
			.duration(tranDuration)
			.attr("transform", "rotate("+angleInDegrees+","+x(d.x)+","+y(d.y)+")");
		
		/*c.transition()
			.ease("sin-out")
			.duration(tranDuration)
			.attr("cx", x(d.x))
			.attr("cy", y(d.y));
		t.transition()
			.ease("sin-out")
			.duration(tranDuration)
			.attr("x", x(d.x))
			.attr("y", y(d.y)+20);*/
			
		
		/*i.transition()
			.ease("sin-out")
			.duration(tranDuration)
			.attr("x", x(d.x) - 12)
			.attr("y", y(d.y) - 18.5);	*/
	});

	/*var enter = node.enter().append("g")
		.attr("class", "node");
	
	enter.append("circle")
		.attr("cx", function(d) {return x(d.x);})
		.attr("cy", function(d) {return y(d.y);})
		.attr("r", 10)
		.style("fill", function (d) {return d.c;});
	;
	
	enter.append("text")
		.text(function(d) { return d.l;})
		.attr("x", function(d) {return x(d.x);})
		.attr("y", function(d) {return y(d.y)+20;})
		.attr("class", "label")
		.style("text-anchor", "middle");
		
	// Add a fade in transition to all entering elements and child elements
	enter.selectAll("*")
		.style("opacity", 0)
		.transition()
		.duration(300)
		.style("opacity", 1);

	// Fade out and remove all exiting elements
	node.exit()
		.style("opacity", 1)
		.transition()
		.duration(300)
		.style("opacity", 0)
		.remove();*/
}

function lineDistance(x1, y1, x2, y2) {
	var d1 = x1 - x2;
	var d2 = y1 - y2;
	return Math.sqrt(d1 * d1 + d2 * d2);
}

function randomPoint(i) {
	var pointData = [];
	for (var j = 0; j < i; j++) {
		pointData.push({"x": Math.random(), "y": Math.random(), "l": "Point" + j, "c": colors[j]});
	}

	update(pointData);
}