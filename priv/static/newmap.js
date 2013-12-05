var MAP_MODE = {
	TITLE: 0,
	ABOUT: 1,
	NORMAL: 2,
};
var server;
var renderer;
var stage;
var map;
var mapMode;
var logo;
var bg;
var points = {};
var feet = {};
var shoestillTexture = PIXI.Texture.fromImage('static/map/shoestill.png');
var shoeleftTexture = PIXI.Texture.fromImage('static/map/shoeleft.png');
var shoerightTexture = PIXI.Texture.fromImage('static/map/shoeright.png');
var box;
var blurFilter, colorStepFilter;
var fnt;

// Setups scale based on width and height
var y = d3.scale.linear()
	.domain([0, 29])
	.range([290, 0])
	.clamp(true);

var x = d3.scale.linear()
	.domain([0, 19])
	.range([0, 290])
	.clamp(true);

// When DOM is ready
$(function() {
	init();
	animate();
});

function animate() {
	requestAnimFrame(animate);
	renderer.render(stage);
}

function init() {
	// Setup WebSocket connection
	var host = location.hostname;
	if (host == '') {
		host = 'localhost'
	}
	var url = 'ws://' + host + ':8080/websocket';
	server = new FancyWebSocket(url);
	
	// Server event bindings
	// Binds a function to the "position" event from server
	// Called whenever the server returns a position of a device
	server.bind('position', function(position) {
		if (mapMode == MAP_MODE.NORMAL) {
			position[0].time = ~~(Date.now() / 1000);
			updatePoint(position[0]);
		}
	});
	
	// pixi setup
	renderer = new PIXI.autoDetectRenderer(640, 640, null, true);
	document.body.appendChild(renderer.view);
	stage = new PIXI.Stage;
	mapMode = MAP_MODE.TITLE;
	console.log('init ok');
	
	var loader = new PIXI.AssetLoader(['static/map/fipps.fnt', 'static/map/bitout.fnt']);
	loader.load();
	
	blurFilter = new PIXI.BlurFilter();
	blurFilter.blur = 8;
	colorStepFilter = new PIXI.ColorStepFilter();
	colorStepFilter.step = 2;
	
	mapMode = MAP_MODE.TITLE;
	// logo animation
	logo = new PIXI.Sprite.fromImage('static/map/TheMaraudersMap.png');
	stage.addChild(logo);
	logo.anchor.x = 0.5;
	logo.anchor.y = 0.5;
	logo.alpha = 0;
	logo.position.x = 320;
	logo.position.y = 320;
	logo.filters = [blurFilter, colorStepFilter];
	
	var logoFadeInDuration = 1;
	
	TweenLite.to(colorStepFilter, logoFadeInDuration*2, {
		step: 10,
		delay: 1,
		ease: Power1.easeInOut
	});
	TweenLite.to(blurFilter, logoFadeInDuration, {
		blur: 0,
		delay: 1,
		ease: Power1.easeInOut
	});
	TweenLite.to(logo, logoFadeInDuration, {
		alpha: 1,
		delay: 0.5,
		ease: Power1.easeIn,
		onComplete: logoFadedIn
	});
	
	// Clear old data
	setInterval(function() {
		for (var key in points) {
			if (points.hasOwnProperty(key) && ((Date.now() / 1000) - points[key].time > 5)) {
				removeWalker(key, feet[key]);
			}
		}
	}, 15);
}

function logoFadedIn() {
	TweenLite.to(colorStepFilter, 2, {
		step: 1,
		delay: 2.5,
		ease: Power1.easeInOut
	});
	TweenLite.to(blurFilter, 1, {
		blur: 10,
		delay: 2.5,
		ease: Power1.easeInOut
	});
	TweenLite.to(logo, 2, {
		alpha: 0,
		delay: 3,
		onComplete: logoFadedOut
	});
}

function logoFadedOut() {
	bg = new PIXI.Sprite.fromImage('static/map/LokeyBirdeye.png');
	stage.addChild(bg);
	bg.anchor.x = 0.5;
	bg.anchor.y = 0.5;
	bg.alpha = 0;
	bg.position.x = 320;
	bg.position.y = 320;
	
	TweenLite.to(bg, 1, {
		alpha: 1,
		onComplete: main
	});
	
	box = new PIXI.DisplayObjectContainer();
	stage.addChild(box);
	//box.anchor.x = 0.5;
	//box.anchor.y = 0.5;
	box.rotation = -0.174;
	box.position.x = 130;
	box.position.y = 190;
	
	var graphics = new PIXI.Graphics();
	graphics.lineStyle(1, 0xFF0000);
	graphics.drawRect(0, 0, 290, 290);
	box.addChild(graphics);
}

function main() {
	mapMode = MAP_MODE.NORMAL;
}

function updatePoint(point) {
	if (points[point.i] == null) { // new
		points[point.i] = point;
		addWalker(point);
	} else { // update
		points[point.i] = point;
		updateWalker(point.i, feet[point.i]);
	}
	
	
	
}

function addWalker(newPoint) {
	var newX = x(newPoint.y);
	var newY = y(newPoint.x)
	var text = new PIXI.BitmapText(newPoint.name, {
		align: 'center',
		font: '8px Fipps',
	});
	text.position.x = newX-text.width/2  ;
	text.position.y = newY+text.height/2;
	box.addChild(text);
	var walker = new PIXI.MovieClip([
		shoestillTexture,
		shoeleftTexture,
		shoestillTexture,
		shoerightTexture
	]);
	walker.anchor.x = 0.5;
	walker.anchor.y = 0.5;
	walker.position.x = newX;
	walker.position.y = newY;
	walker.scale.x = 0.05;
	walker.scale.y = 0.05;
	walker.alpha = 0;
	walker.animationSpeed = 0.07; 
	walker.text = text;
	walker.time = newPoint.time;
	feet[newPoint.i] = walker;
	box.addChild(walker);
	
	TweenLite.to(walker, 0.5, {
		alpha: 1
	});
}

function updateWalker(key, walker) {
	var newX = x(points[key].y);
	var newY = y(points[key].x);
	var deltaTime = points[key].time - walker.time;
	var deltaX = newX - walker.position.x;
	var deltaY = newY - walker.position.y;
	var distance = Math.sqrt(deltaX*deltaX + deltaY*deltaY);
	var angle = Math.atan2(deltaY, deltaX) + (Math.PI/2);
	walker.animationSpeed = distance/(deltaTime*200);
	walker.play();
	walker.rotation = angle;
	//console.log("angle:"+angle+"x:"+x(points[key].y)+"y:"+y(points[key].x));
	TweenLite.to(walker.text.position, deltaTime, {
		x: newX - walker.text.width/2,
		y: newY + walker.text.height/2,
		ease: Power1.easeInOut
	});
	TweenLite.to(walker.position, deltaTime, {
		x: newX,
		y: newY,
		ease: Power1.easeInOut,
		onComplete: function() {
			walker.gotoAndStop(0);
		}
	});
	//particle(walker);
}

function removeWalker(key, walker) {
	delete feet[key];
	delete points[key];
	TweenLite.to(walker.text, 1, {
		alpha: 0
	});
	TweenLite.to(walker, 1, {
		alpha: 0,
		onComplete: function() {
			box.removeChild(walker.text);
			box.removeChild(walker);
		}
	});
}