(function ( monitor ) {
    /*
     * Private Properties
     */
    var data = {};
    var length = 30;
    var interval = 1000;
    var _Y_AXIS_STEPS = 10;

    function get_data(graph, int_count, draw_callback) {
        function success_callback( received ) {
            //console.log(received);

            for(var i = 0; i<int_count; i++){
                if(data[graph]){
                    data[graph].shift();
                } else {
                    data[graph] = []
                }
            }
            for(var i = 0; i<int_count; i++){
                data[graph].push(received[i]);
            }
            draw_callback(data[graph]);
        }

        var json = {url: "/monitor",
            type: "POST",
            contentType: "application/json", 
            data: JSON.stringify({"monitor-type":graph, 
                  "interval-count": int_count, 
                  "interval-length":interval, 
                  "until":(new Date()).toJSON()}),
                  success: success_callback,
                  error: function() {console.log(data);}};
        $.ajax(json)
    }
    
    /*
     * Public interface
     */
    monitor.display_graph = function(id, max_x, max_y) {
        var data_selector = d3.select("#"+id+" > .data");
        var width = data_selector[0][0].offsetWidth;
        var height = data_selector[0][0].offsetHeight;
        var offset = 20;
        
        // create an SVG element inside the #graph div that fills 100% of the div
        var graph = data_selector.append("svg:svg")
            .attr("width", width).attr("height", height);
    
        //draws x axis before and after graph to avoid laggy draws
        var x = d3.scale.linear().domain([0, max_x-1]).range([-offset, width+offset]); 
        var y = d3.scale.linear().domain([0, max_y]).range([0, height]);
        //axes scales
        var x_axis_scale = d3.scale.linear().domain([-(max_x-1), 0]).range([0,width]);
        var y_axis_scale = d3.scale.linear().domain([max_y, 0]).range([0,height]);

        // create a line object that represents the SVN line we're creating
        var line = d3.svg.line()
            .x(function(d,i) { return x(i); })
            .y(function(d) { return y(max_y-d); })
            .interpolate("linear");

        function draw_axes(current_data) {
            var col_width = width/max_x;
            var row_height = height/_Y_AXIS_STEPS;
            var x_axis = d3.select("#"+id+" > .x_axis");
            var y_axis = d3.select("#"+id+" > .y_axis");
            x_axis.append("div").attr("class","col")
                .style("width", col_width+"px")
                .text("now");
            
            var k = _Y_AXIS_STEPS/100 * max_y;
            for(var i=0, y_data=[], val=0; i<=_Y_AXIS_STEPS; i++){
                val+=k;
                y_data.push(max_y+k - val);
            }
            y_axis.selectAll("div").data(y_data).enter()
                .append("div").attr("class", "row")
                .style("height", row_height+"px")
                .text(function(d) {return Math.round(d)});
        }

        //display data appended to the line
        function draw(current_data) {
            graph.selectAll("path").data([current_data]).enter()
                .append("g").attr("transform", "translate(0, 0)")
                .append("svg:path")
                .attr("d", line).attr("class","queue");

            //x axis
            var x_axis = d3.svg.axis().scale(x_axis_scale).orient("bottom");
            graph.append("g").attr("transform", "translate(0, "+height+")")
                .call(x_axis).attr("class", "axis");
            graph.append("g").attr("class", "grid")
                .attr("transform", "translate(0, "+height+")")
                .call(x_axis.tickSize(-height).tickFormat(""));
            //y axis
            var y_axis = d3.svg.axis().scale(y_axis_scale).orient("left");
            graph.append("g").attr("transform", "translate(0, 0)")
                .call(y_axis).attr("class","axis");
            graph.append("g").attr("class", "grid")
                .attr("transform", "translate(0, 0)")
                .call(y_axis.tickSize(-width).tickFormat(""));

            draw_axes(current_data);
        } 
        get_data(id, max_x, draw);

        function get_data_redraw(){
            function redraw(current_data) {
                // update with animation
                graph.selectAll("path")
                    .data([current_data]) // set the new data
                    .attr("transform", "translate(" + x(1) + ")") 
                    .attr("d", line) 
                    .transition() 
                    .ease("linear")
                    .duration(interval-50) 
                    .attr("transform", "translate(" + x(0) + ")");
            }
            get_data(id, 1, redraw);
        }

        setInterval(function() {
            get_data_redraw();
        }, interval);
    }
}(window.monitor = window.monitor || {}));

monitor.display_graph("queue", 30, 10);		
