function sk_rect(svg,x,y,dx,dy,color) {
    var r = svg.append("rect")
                .attr("x", x)
                .attr("y", y)
                .attr("width", dx)
                .attr("height", dy);
    if(color) {
        r.style("fill", d => color);
    }
    return svg;
}

function sk_svg() {
    return d3.selectAll('svg');
}

function sk_clear() {
    d3.selectAll('svg *').remove();
}
