import 'metrics-graphics/dist/metricsgraphics.css';
import React from 'react';
import d3 from 'd3';
import MG from 'metrics-graphics';

type NodeProps = {
  // metrics: {
  //   [key: string]: Function;
  // };
};

function wrapMetric(callback) {
  return (data, prev) => {
    try {
      return {
        value: callback(data, prev),
        date: new Date(data.ekg.server_timestamp_ms.val),
      };
    } catch (e) {
      console.err(e)
      return {
        date: new Date(),
        value: 0,
      };
    }
  };
}

const metrics = {
  // 'cpu time': wrapMetric((data, prev) => data.rts.gc.cpu_ms.val - prev),
  // 'wall ms': wrapMetric((data, prev) => data.rts.gc.wall_ms.val - prev),
  // 'bytes allocated': wrapMetric((data, prev) => data.rts.gc.bytes_allocated.val - prev),
  // 'gcs': wrapMetric((data, prev) => data.rts.gc.num_gcs.val - prev),
  'applied index': wrapMetric(data => data.juno.node.applied_index.val),
};

function nodeStatus(data) {
  const node = data.juno.node;
  return {
    port: node.port.val,
    applied_index: node.applied_index.val,
    role: node.role.val,
  };
}

export default class Node extends React.Component {
  render(): React.Element {
    return <div ref={ref => this._d3 = ref} />;
  }

  componentDidMount() {
    this._renderD3();
  }

  componentWillReceiveProps() {
    this._renderD3();
  }

  _renderD3() {
    const {port, data} = this.props;
    const names = Object.keys(metrics);
    const renderData = [];
    for (let name of names) {
      const summary = [];
      renderData.push(summary);
      const metric = metrics[name];
      for (let datum of data) {
        summary.push(metric(datum));
      }
    }

    if (renderData[0].length > 0) {
      MG.data_graphic({
        title: `NODE ACTIVITY ON PORT ${port}`,
        data: renderData,
        buffer: 50,
        width: 500,
        height: 100,
        target: this._d3,
        x_accessor: 'date',
        y_accessor: 'value',
        interpolate: 'monotone',
        legend: names,
      });
    }
  }
}

export default class Nodes extends React.Component {
  render() {
    const {data} = this.props;
    return (
      <div>
        <h2>NODES</h2>
        <Node port={10000} data={data['127.0.0.1:10000']} />
        <Node port={10001} data={data['127.0.0.1:10001']} />
        <Node port={10002} data={data['127.0.0.1:10002']} />
        <Node port={10003} data={data['127.0.0.1:10003']} />
      </div>
    );
  }
}
