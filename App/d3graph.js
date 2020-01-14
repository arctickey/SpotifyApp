d3.selectAll("svg").remove();

// set the dimensions and margins of the graph
var margin = {top: 10, right: 20, bottom: 300, left: 400 },
    width = 900 - margin.left - margin.right,
    height = 650 - margin.top - margin.bottom;

// append the svg object to the body of the page
var svg = d3.select("body")
    .append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");
          
var hours = svg.append("g")
    .attr("class", "hours");
    
// X axis
var x = d3.scaleBand()
  .range([ 0, width ])
  .domain(data.map(function(d) { return d.artistName; }))
  .padding(0.2);
  
svg.append("g")
  .attr("transform", "translate(0," + height + ")")
  .call(d3.axisBottom(x))
  .selectAll("text")
    .attr("transform", "translate(-10,0)rotate(-45)")
    .style("text-anchor", "end");

// Add Y axis
var y = d3.scaleLinear()
  .range([ height, 0])
  //.domain([0, 200])
  .domain([0, 1.2 * d3.max(data, function(d){ return d.time; })]);
  
svg.append("g")
  .call(d3.axisLeft(y));

svg.selectAll("mybar")
  .data(r2d3.data)
  .enter()
  .append("rect")
    .attr("x", function(d) { return x(d.artistName); })
    .attr("width", x.bandwidth())
    .attr("fill", "orange")
    // no bar at the beginning thus:
    .attr("height", function(d) { return height - y(0); }) // always equal to 0
    .attr("y", function(d) { return y(0); })

    
svg.selectAll("rect")
  .transition()
  .duration(800)
  .attr("y", function(d) { return y(d.time); })
  .attr("height", function(d) { return height - y(d.time); })
  .delay(function(d,i){console.log(i) ; return(i*100)})