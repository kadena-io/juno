import React from 'react';
import d3 from 'd3';

const width = 500;
const height = 40;

function clusterStatus(data) {
  const cluster = data.juno.cluster;
  return {
    quorum: cluster.quorum_size.val,
    avail: cluster.available_size.val,
    size: cluster.size.val,
    term: data.juno.consensus.term.val,
  };
}

export default function Health(props) {
  if (props.data == null) {
    return <div />;
  }
  const data = clusterStatus(props.data);

  const scale = d3.scale.ordinal()
    .domain(['quorum', 'avail', 'size'])
    .range([data.quorum, data.avail, data.size])
  const scaleMult = 500 / data.size;

  return (
    <div className="section">
      <h2>
        HEALTH
        <div className="border-underline" />
      </h2>
      <div className="transaction-point">
        Election Term: <span className="transaction-datum">{data.term}</span>
      </div>
      <div className="div-bars" style={{width}}>
        <div
          className="div-bar"
          style={{
            width: scale('quorum') * scaleMult,
            height: 30,
          }}
        >
          quorum
          {data.quorum}
        </div>
        <div
          className="div-bar"
          style={{
            width: scale('avail') * scaleMult,
            height: 30,
          }}
        >
          available
          {data.avail}
        </div>
      </div>
      <div className="flange" />
      <h3 className="cluster-size">{'cluster size: ' + data.size}</h3>
    </div>
  );
}
