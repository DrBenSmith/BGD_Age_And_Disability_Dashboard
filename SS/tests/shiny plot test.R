library(shiny)
library(shinyjs)
library(highcharter)
library(htmltools)

# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
  tags$head(
    HTML('<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />'),
    includeCSS("../styles.css"), # If included, the plot window is formatted
    HTML(
      "
      <script src='https://d3js.org/d3.v4.min.js'></script>
      <script>
      function chart(name, values, labels, color, mode) {
  var data = {
  labels: labels,
  series: values};

var barHeight        = 20,
    groupHeight      = barHeight * data.series.length,
    gapBetweenGroups = 10,
    spaceForLabels   = 180,
    spaceForLegends = 75;

var zippedData = [];
for (var i=0; i<data.labels.length; i++) {
  for (var j=0; j<data.series.length; j++) {
  if(data.series[j].values[i] != null)
    zippedData.push(data.series[j].values[i]);
  }
}

var color = d3.scaleOrdinal()
            .range(color);
if(mode == 'gender_multiple'){
color = d3.scaleOrdinal()
            .range(['#D2CBB8','#58585A']);
}
var chartHeight = barHeight * zippedData.length + gapBetweenGroups * data.labels.length;

var x = d3.scaleLinear()
    .domain([0, d3.max(zippedData)])
    .range([0, d3.max(zippedData) * 2]);

var y = d3.scaleLinear()
    .range([chartHeight + gapBetweenGroups, 0]);

var yAxis = d3.axisLeft()
    .scale(y)
    .tickFormat('')
    .tickSize(0);

var chart = d3.select(name)
    .attr('width', 500)
    .attr('height', chartHeight);

var bar = chart.selectAll('g')
    .data(zippedData)
    .enter().append('g')
    .attr('transform', function(d, i) {
      return 'translate(' + spaceForLabels + ',' + (i * barHeight + gapBetweenGroups * (0.5 + Math.floor(i/data.series.length))) + ')';
    });

bar.append('rect')
    .attr('fill', function(d,i) { return color(i % data.series.length); })
    .attr('class', 'bar')
    .attr('width', 0)
    .transition()
   .duration(500)
   .delay((d, i) => i * 100)
   .attr('width', (d) => x(d))
  .attr('height', barHeight - 1)

bar.append('text')
    .attr('class', 'label')
    .attr('x', function(d) { return - 10; })
    .attr('y', groupHeight / 2)
    .attr('dy', '.35em')
    .text(function(d,i) {
      if (i % data.series.length === 0)
        return data.labels[Math.floor(i/data.series.length)];
      else
        return ''});

  bar.append('text')
    .attr('class', 'label2')
    .transition()
   .duration(450)
   .delay((d, i) => i * 100)
    .attr('x', function(d) {
    return (d * 2) + 5;
  })
    .attr('y', 14)
    .text(function(d,i) {
        return d + '%'
  });

chart.append('g')
      .attr('class', 'y axis')
      .attr('transform', 'translate(' + spaceForLabels + ', ' + -gapBetweenGroups/2 + ')')
      .call(yAxis);

if(mode == 'multiple' || mode == 'multiple'){
var legendRectSize = 10,
    legendSpacing  = 4;

var legend = chart.selectAll('.legend')
    .data(data.series)
    .enter()
    .append('g')
    .attr('transform', function (d, i) {
        var height = legendRectSize + legendSpacing;
        var offset = -gapBetweenGroups/2;
        var horz = spaceForLabels + d3.max(zippedData) * 2 + 60;
        var vert = i * height - offset;
        return 'translate(' + horz + ',' + vert + ')';
    });

legend.append('rect')
    .attr('width', legendRectSize)
    .attr('height', legendRectSize)
    .style('fill', function (d, i) { return color(i); })
    .style('stroke', function (d, i) { return color(i); });

legend.append('text')
    .attr('class', 'legend')
    .attr('x', legendRectSize + legendSpacing)
    .attr('y', legendRectSize - legendSpacing)
    .text(function (d) { return d.label; });
}
}
                </script>")),

    # App title ----
  absolutePanel(

  HTML(sprintf(
    ' <div class="card" style="background-color:#FFFFFF;text-align:center;margin-bottom:20px">
              <h4 class="card-header" >%s</h4>
                <div class="card-body">
                <svg id="%s" class="chart"></svg>
              </div></div>
                <script type="text/javascript">
                chart(%s, %s, %s, %s, "single");
                </script>',
    "A NICE HEADER",
    "BENSPLOT",
    "'#BENSPLOT'",
    "[{values:[10, 20]}]",
    "['TOP', 'BOTTOM']",
    paste0("'#E6EFE5'")),
    )
))

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)
