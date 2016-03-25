import React from 'react';
import d3 from 'd3';

const width = 500;
const height = 400;

const styles = {
  quorum: { backgroundColor: '#8ac' },
  avail: { backgroundColor: '#678' },
  size: { backgroundColor: '#456' },
};

const data = [
  { label: 'quorum', size: 26 },
  { label: 'avail', size: 47 },
  { label: 'size', size: 50 },
];

function clusterStatus(data) {
  const cluster = data.juno.cluster;
  return {
    quorum: cluster.quorum_size.val,
    avail: cluster.available_size.val,
    size: cluster.size.val,
  };
}

export default class Health extends React.Component {
  render() {
    return (
      <div>
        <h2>HEALTH</h2>
        <div ref={ref => this._d3 = ref} />
      </div>
    );
  }

  componentDidMount() {
    this._renderD3();
  }

  componentDidUpdate() {
    this._renderD3();
  }

  _renderD3() {
    const {data} = this.props;
    if (data == null) {
      return;
    }

    const d3data = d3.map(clusterStatus(data)).entries();

    d3.select(this._d3)
      .selectAll('div')
        .data(d3data)
      .enter().append('div')
        .style('width', d => `${d.value * 10}px`)
        .style('background-color', d => styles[d.key].backgroundColor)
        .text(d => d.key);
  }
}
