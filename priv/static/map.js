var MM = {};

var colors = ["Navy", "Teal", "Lime", "Cyan", "ForestGreen", "Indigo"];

MM.pointData = {};

// var margin = {top: 160, right: 140, bottom: 160, left: 140},
// 	width  = 640 - margin.left - margin.right,
// 	height = 640 - margin.top - margin.bottom;
MM.width = 640;
MM.height = 640;

// Setups scale based on width and height
MM.x = d3.scale.linear()
	.domain([0, 29])
	.range([290, 0])
	.clamp(true);

MM.y = d3.scale.linear()
	.domain([0, 19])
	.range([0, 290])
	.clamp(true);

MM.xAxis = d3.svg.axis()
	.scale(MM.x)
	.ticks(15)
	.orient("left");

MM.yAxis = d3.svg.axis()
	.scale(MM.y)
	.orient("bottom");
	
// Add the SVG element to the body
MM.canvas = d3.select('#canvas').append('svg')
	.attr('width', MM.width)
	.attr('height', MM.height)
	.attr('class', 'lokey');
	
MM.main = MM.canvas.append('g')
	.attr('transform', 'translate(120,190),rotate(-5),skewX(7),skewY(-3)')
	.attr('width', 290)
	.attr('height', 290)
	.attr('class', 'main');

// Draws the axes
MM.main.append("g")
	.attr("class", "axis")
	.call(MM.xAxis);

MM.main.append("g")
	.attr("class", "axis")
	.attr("transform", "translate(0, 290)")
	.call(MM.yAxis);

function getX(d) {
	return MM.y(d.y)
}

function getY(d) {
	return MM.x(d.x)
}

// Updates, adds, and removes points on the graph.
// Data format must be an array of x,y arrays. Example: [[.3, .7], [.2, .4]]
MM.update = function(data) {
	// Data Join
	var node = MM.main.selectAll(".node").data(d3.values(data), function(d) {
		return d.i;
	});
	
	var old = node.selectAll("image");
	
	// 1. exit
	node.exit().remove()
	// var exitTransition = d3.transition().each(function() {
	// 	node.exit()
	// 		//.transition()
	// 		//.style("opacity", 0)
	// 		.remove();
	// });
	
	// 2. update
	// var updateTransition = exitTransition.transition().each(function() {
		node.transition()
			.attr("x", function(d) {return getX(d);})
			.attr("y", function(d) {return getY(d);})
		/*.attr("transform", function(d) {
			return "translate("+(MM.x(d.x) - old.attr("cx"))+","+(MM.y(d.y) - old.attr("cy"))+")";
		});*/
	// });
	
	// 3. enter
	// var enterTransition = updateTransition.transition().each(function() {
		var enter = node.enter().append("svg").attr("class", "node");
		enter.attr("x", function(d) {return getX(d)})
			.attr("y", function(d) {return getY(d)});
		var group = enter.append("g");

		group.append("circle")
			//.attr("cx", function (d) {return MM.x(d.x);})
			//.attr("cy", function (d) {return MM.y(d.y);})
			.attr("r", 1)
			.style("fill", "none")
			.attr("class", "point");
		
		group.append("svg:image")
			//.attr("x", function(d) { return MM.x(d.x) -12; })
			//.attr("y", function(d) { return MM.y(d.y) -12; })
			//.attr("cx", function (d) {return MM.x(d.x);})
			//.attr("cy", function (d) {return MM.y(d.y);})
			.attr("xlink:href","static/map/shoeprints.png")
			.attr("height", 24)
			.attr("width", 24)
			.attr("rotation", 0);

		group.append("text")
			.text(function(d) { return d.name; })
			//.attr("x", function(d) {return MM.x(d.x);})
			//.attr("y", function(d) {return MM.y(d.y)+25;})
			.attr("class", "label")
			.style("text-anchor", "middle");

		group.selectAll("*")
			.style("opacity", 0)
			.transition()
			.duration(100)
			.style("opacity", 1);
	// });
	// Update
	/*old
		.attr("transform", function(d) {
				//return "rotate("+i.attr("rotation")+","+(x(d.x))+","+(y(d.y))+")", "rotate("+angle+","+(x(d.x))+","+(y(d.y))+")";
				var deltaX = MM.x(d.x) - old.attr("cx");
				var deltaY = MM.y(d.y) - old.attr("cy");
				var angle = Math.atan2(deltaY, deltaX) * 180 / Math.PI+90;
				return "rotate("+d3.select(this).attr("rotation")+","+(MM.x(d.x))+","+(MM.y(d.y))+")", "rotate("+angle+","+(MM.x(d.x))+","+(MM.y(d.y))+")";
		});*/

	
	/*node.selectAll("circle")
		.attr("cx", function(d) { return MM.x(d.value.x); })
		.attr("cy", function(d) { return MM.y(d.value.y); });
		
	node.selectAll("image")
		.attr("x", function(d) { return MM.x(d.value.x) - 12; })
		.attr("y", function(d) { return MM.y(d.value.y) - 12; });
		
	node.selectAll("text")
		.text("updated")
		.attr("x", function(d) { return MM.x(d.value.x); })
		.attr("y", function(d) { return MM.y(d.value.y)+25; });*/
	// Enter
	
		
	
	// Enter + Update
	//node.transition()
	//	.attr("transform", function(d) {
	//		return "translate("+(MM.x(d.x) - old.attr("cx"))+","+(MM.y(d.y) - old.attr("cy"))+")";
	//});
	
	// Exit
	//node.exit()
	//	.remove();

		
	/*var node = MM.main.selectAll(".node")
		.data(d3.entries(data), function(d) {
			return d.key;
		});
		
	node.each(function (d,i) {
		var s = d3.select(this);
		var c = s.selectAll("circle");
		var t = s.selectAll("text");
		var i = s.selectAll("image");

		var tranDuration = lineDistance(c.attr("cx"), c.attr("cy"), MM.x(d.value.x), MM.y(d.value.y)) * 10;
		var deltaX = MM.x(d.value.x) - c.attr("cx");
		var deltaY = MM.y(d.value.y) - c.attr("cy");
		var angle = Math.atan2(deltaY, deltaX) * 180 / Math.PI + 90;
		c.transition()
			.ease("sin-out")
			.duration(tranDuration)
			.attr("cx", MM.x(d.value.x))
			.attr("cy", MM.y(d.value.y))
			.style("opacity", 1)
			.attr("r", 1);
			
		i.attr("transform", function() {
				//return "rotate("+i.attr("rotation")+","+(x(d.x))+","+(y(d.y))+")", "rotate("+angle+","+(x(d.x))+","+(y(d.y))+")";
				return "rotate("+i.attr("rotation")+","+(MM.x(d.value.x))+","+(MM.y(d.value.y))+")", "rotate("+angle+","+(MM.x(d.value.x))+","+(MM.y(d.value.y))+")";
			});
			
		//i.transition()
		//	.ease("sin-out")
		//	.duration(tranDuration)
			i.attr("x", MM.x(d.value.x) - 12)
			.attr("y", MM.y(d.value.y) - 12);

		t.transition()
			.ease("sin-out")
			.duration(tranDuration)
			.attr("x", MM.x(d.value.x))
			.style("opacity", 1)
			.attr("y", MM.y(d.value.y)+20);
	});

	var enter = node.enter().append("g")
		.attr("class", "node")

	enter.append("circle")
		.attr("cx", function (d) {return MM.x(d.value.x);})
		.attr("cy", function (d) {return MM.y(d.value.y);})
		.attr("r", 1)
		.style("fill", "black")
		.attr("class", "point");
		
	enter.append("image")
		.attr("x", function(d) { return MM.x(d.value.x) -12; })
		.attr("y", function(d) { return MM.y(d.value.y) -12; })
		.attr("xlink:href","static/map/footprint.svg")
		.attr("height", 24)
		.attr("width", 24)
		.attr("rotation", 0);

	enter.append("text")
		.text(function(d) { return d.value.l; })
		.attr("x", function(d) {return MM.x(d.value.x);})
		.attr("y", function(d) {return MM.y(d.value.y)+25;})
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
		.remove()*/
};

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

// When DOM ready
$(function() {
	// Setup WebSocket connection
	var host = location.hostname;
	if (host == '') {
		host = 'localhost'
	}
	var url = 'ws://' + host + ':8080/websocket';
	MM.server = new FancyWebSocket(url);
	
	
	// Binds a function to the "position" event from server
	// Called whenever the server returns a position of a device
	MM.server.bind('position', function(position) {
		position[0].time = ~~(Date.now() / 1000);
		MM.pointData[position[0].i] = position[0];
	});
	
	setInterval(function() {
		//var temp = [];
		for (var key in MM.pointData) {
			if (MM.pointData.hasOwnProperty(key) && ((Date.now() / 1000) - MM.pointData[key].time > 10)) {
				delete MM.pointData[key];
			}
		}
		// Keep only most recent 10 points
		//if (temp.length > 15) temp.splice(0, temp.length - 15);
		//MM.pointData = {};
		MM.update(MM.pointData);
	}, 15);
});

