function makeBarChart(data, divId, height, width, fill, opacity, barspacing) {


var root = d3.select(divId).append('svg')

var template = root.append("svg")
    .attr("x", 80)
    .attr("y", 30)
    .attr("width", width)
    .attr("height", height)
    .attr("id", "a-chart");


var maxDataValue = d3.max(evenNumbers);
var barHeight = height / evenNumbers.length;
var barWidth = function(datum) {
  return datum * (width / maxDataValue);
};
var barX = 0;
var barY = function(datum, index) {
  return index * barHeight;
};

template.selectAll('rect.number')
    .data(evenNumbers).enter()
  .append('rect')
    .attr({
      'x': barX,
      'y': barY,
      'width': barWidth,
      'height': barHeight-barspacing,
      'fill': fill,
      'stroke': '#444',
      'opacity':opacity
    });

}

function makeVerticalBarChart(dataset, divId, height, width, fill, opacity, barspacing) {


var root = d3.select(divId).append('svg')

var template = root.append("svg")
    .attr("x", 80)
    .attr("y", 30)
    .attr("width", width)
    .attr("height", height)
    .attr("id", "a-chart");


var maxDataValue = d3.max(dataset);
var barWidth = width / dataset.length;
var barHeight = height;
var barY = function(datum, index) {
  return height - datum * (height / maxDataValue);
};

var barX = function(datum, index) {
  return index * barWidth;
};

template.selectAll('rect.number')
    .data(dataset).enter()
  .append('rect')
    .attr({
      'x': barX,
      'y': barY,
      'width': barWidth - barspacing,
      'height': barHeight ,
      'fill': fill,
      'stroke': '#444',
      'opacity':opacity
    });

}

function makeVerticalBarChart2(dataset, w, h, divId, color){
//Width and height

var svg = d3.select(divId)
            .append("svg")
            .attr("width", w)
            .attr("height", h);

svg.selectAll("rect")
   .data(dataset)
   .enter()
   .append("rect")
   .attr("x", function(d, i) {
    return i * (w / dataset.length);  //Bar width of 20 plus 1 for padding 
    })
   .attr("y", function(d) {
    return h - d*4;  //Height minus data value
    })
   .attr("width", 20)
   .attr("height", function(d) {
    return 4*d;  //Just the data value
    })
   .attr("fill", color);

svg.selectAll("text")
   .data(dataset)
   .enter()
   .append("text")
   .text(function(d) {
        return d;
   })
   .attr("x", function(d, i) {
        return i * (w / dataset.length)+4;
   })
   .attr("y", function(d) {
        return h - (d * 4)+15;
   })
   .attr("font-family", "sans-serif")
   .attr("font-size", "11px")
   .attr("fill", "white");
}

function makeVerticalBarChart3(dataset, w, h, divId, color, padding){
//Width and height


var yScale = d3.scale.linear()
                     .domain([0, d3.max(dataset, function(d) { return d; })])
                     .range([h-padding, padding]);

var svg = d3.select(divId)
            .append("svg")
            .attr("width", w)
            .attr("height", h);



svg.selectAll("rect")
   .data(dataset)
   .enter()
   .append("rect")
   .attr("x", function(d, i) {
    return i * (w / dataset.length);  //Bar width of 20 plus 1 for padding 
    })
   .attr("y", function(d) {
    return yScale(d);  //Height minus data value
    })
   .attr("width", 20)
   .attr("height", function(d) {
    return 4*d;  //Just the data value
    })
   .attr("fill", color);

svg.selectAll("text")
   .data(dataset)
   .enter()
   .append("text")
   .text(function(d) {
        return d;
   })
   .attr("x", function(d, i) {
        return i * (w / dataset.length)+4;
   })
   .attr("y", function(d) {
        return yScale(d);
   })
   .attr("font-family", "sans-serif")
   .attr("font-size", "11px")
   .attr("fill", "white");
}

function makeHorizontalBarChart(dataset, w, h, divId, color, padding){
//Width and height


var yScale = d3.scale.linear()
                     .domain([0, d3.max(dataset, function(d) { return d; })])
                     .range([h-padding, padding]);
var xScale = d3.scale.linear()
                     .domain([0, d3.max(dataset, function(d) { return d; })])
                     .range([padding, w-padding]);

var svg = d3.select(divId)
            .append("svg")
            .attr("width", w)
            .attr("height", h);



svg.selectAll("rect")
   .data(dataset)
   .enter()
   .append("rect")
   .attr("y", function(d, i) {
    return i * (h / dataset.length);  //Bar width of 20 plus 1 for padding 
    })
   .attr("x", 0)
   .attr("width", function(d) {return xScale(d); })
   .attr("height", 20)
   .attr("fill", color);

}

function makeScatterPlot(dataset, w, h, padding, divId)
{

var xScale = d3.scale.linear()
                     .domain([0, d3.max(dataset, function(d) { return d[0]; })])
                     .range([padding, w-padding]);

var yScale = d3.scale.linear()
                     .domain([0, d3.max(dataset, function(d) { return d[1]; })])
                     .range([h-padding, padding]);

var rScale = d3.scale.linear()
                     .domain([0, d3.max(dataset, function(d) { return d[1]; })])
                     .range([5, 12]);

var svg = d3.select(divId)
            .append("svg")
            .attr("width", w)
            .attr("height", h);

var circles = svg.selectAll("circle")
            .data(dataset)
            .enter()
            .append("circle")
            .attr("cx", function(d) {return xScale(d[0]);})
            .attr("cy", function(d) {return yScale(d[1]);})
            .attr("r",  function(d) {return rScale(d[1]);});


var words = svg.selectAll("text")
            .data(dataset)
            .enter()
            .append("text")
            .text(function(d) {return d[0] + "," + d[1];})
            .attr("x", function(d) {return xScale(d[0]);})
            .attr("y", function(d) {return yScale(d[1]);})
            .attr("font-family", "sans-serif")
            .attr("font-size", "11px")
            .attr("fill", "red");

var xAxis = d3.svg.axis()
            .scale(xScale)
            .orient("bottom")
            .ticks(5);

var axisX = svg.append("g")
            .attr("id", "axis")  //Assign "axis" class
            .attr("transform", "translate(0," + (h - padding) + ")")
            .call(xAxis);

var yAxis = d3.svg.axis()
            .scale(yScale)
            .orient("left")
            .ticks(5);

var axisY = svg.append("g")
            .attr("class", "axis")
            .attr("transform", "translate(" + padding + ",0)")
            .call(yAxis);
}

function makeTable(divId, dataset)  {
d3.select(divId)
    .append("table")
    .style("border-collapse", "collapse")
    .style("border", "2px black solid")
    
    .selectAll("tr")
    .data(dataset)
    .enter().append("tr")
    
    .selectAll("td")
    .data(function(d){return d;})
    .enter().append("td")
    .style("border", "1px black solid")
    .style("padding", "10px")
    .on("mouseover", function(){d3.select(this).style("background-color", "aliceblue")}) 
    .on("mouseout", function(){d3.select(this).style("background-color", "white")}) 
    .text(function(d){return d;})
    .style("font-size", "12px");
  }