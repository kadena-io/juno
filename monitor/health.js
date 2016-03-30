import React from 'react';
import d3 from 'd3';

const width = 500;
const height = 40;

function clusterStatus(data) {
  const cluster = data.juno.cluster;
  return [
    { title: 'quorum', value: cluster.quorum_size.val },
    { title: 'avail', value: cluster.available_size.val },
    { title: 'size', value: cluster.size.val },
  ];
}

function Tick({ text, transform }) {
  return (
    <g className="tick" transform={`translate(${transform}, 0)`}>
      <line x1={0} y1={0} x2={0} y2={0} />
      <text dy=".71em" x={0} y={0} style={{textAnchor: 'middle'}}>{text}</text>
    </g>
  );
}

function Axis({ scale }) {
  const ticks = scale.domain();
  const tickElems = ticks.map(tick => (
    <Tick text={tick} key={tick} transform={scale(tick)} />
  ));

  return (
    <g className="axis" transform={`translate(0, 20)`}>
      {tickElems}
    </g>
  );
}

function Rects({ scale, data }) {
  const rects = data.map(datum => (
    <rect
      className="bar"
      key={datum.title}
      width={scale(datum.title)}
      height={height}
    />
  ));

  return (
    <g className="rects">{rects}</g>
  );
}

export default class Health extends React.Component {
  render() {
    const {data} = this.props;
    const d3data = data != null ? clusterStatus(data) : [];

    // XXX this is so wrong
    const scale = d3.scale.ordinal()
      .domain(['quorum', 'avail', 'size'])
      .rangePoints([25, width-25]);

    return (
      <div className="section">
        <h2>
          HEALTH
          <div className="border-underline" />
        </h2>
        <svg
          className="health"
          ref={ref => this._d3 = ref}
          width={width}
          height={height}
        >
          <Axis scale={scale} />
          <Rects scale={scale} data={d3data} />
        </svg>
      </div>
    );
  }
}
