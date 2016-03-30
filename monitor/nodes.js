import 'metrics-graphics/dist/metricsgraphics.css';
import React from 'react';
import d3 from 'd3';
import MG from 'metrics-graphics';
import RadioGroup from 'react-radio-group';

type NodeProps = {
  // metrics: {
  //   [key: string]: Function;
  // };
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
    const {port, role} = this.props;

    return (
      <div className="node">
        <h3 className="node-header">
          <span className="node-header-port">{port}</span>
          <span className={`node-role-${role}`}>{role}</span>
        </h3>
        <div className="node-graph" ref={ref => this._d3 = ref} />
      </div>
    );
  }

  componentDidMount() {
    this._renderD3();
  }

  componentWillReceiveProps() {
    this._renderD3();
  }

  _renderD3() {
    const {port, data} = this.props;
    const now = new Date();

    if (data.length > 0) {
      MG.data_graphic({
        // title: `NODE ACTIVITY ON PORT ${port}`,
        title: '',
        data,
        // buffer: 50,
        width: 600,
        height: 200,
        target: this._d3,
        x_accessor: 'date',
        xax_format(f) {
          const secsAgo = Math.round((now - f) / 1000, 0);
          return `${secsAgo} s ago`;
        },
        y_accessor: 'value',
        interpolate: 'monotone',
      });
    }
  }
}

type Time = 30 | 1 | 5;

export default class Nodes extends React.Component {
  constructor() {
    super();
    this.state = {
      selectedMetric: 'appliedIndex',
      selectedTime: '1',
    };
  }

  render() {
    const {selectedMetric, selectedTime} = this.state;
    const hosts = [10000, 10001, 10002, 10003];

    const nodeElems = [];
    for (let hostPort of hosts) {
      const hostName = `127.0.0.1:${hostPort}`;
      const hostInfo = this.props.data[hostName];

      const points = {
        30: 30,
        1: 60,
        5: 300,
      };

      // get last n points
      const data = hostInfo
        .computedData
        .slice(-points[selectedTime])
        .map(({date, commitPeriod}) => ({date, value: commitPeriod}));

      nodeElems.push(
        <Node
          key={hostPort}
          port={hostPort}
          data={data}
          role={hostInfo.role}
        />
      );
    }

    return (
      <div className="section">
        <h2>
          NODES
          <div className="border-underline" />
        </h2>

        <div className="nodes-selector">
          <div>
            <RadioGroup
              selectedValue={selectedMetric}
              onChange={selectedMetric => this.handleMetric(selectedMetric)}
            >
              {Radio => (
                <ul>
                  <li className="node-option"><label><Radio value="appliedIndex" /><span className="check" />Applied Index</label></li>
                  <li className="node-option"><label><Radio value="commitPeriod" /><span className="check" />Commit Period</label></li>
                </ul>
              )}
            </RadioGroup>
          </div>

          <div>
            <RadioGroup
              selectedValue={selectedTime}
              onChange={selectedTime => this.handleTime(selectedTime)}
            >
              {Radio => (
                <ul>
                  <li className="node-option"><label><Radio value="30" /><span className="check" />30 s</label></li>
                  <li className="node-option"><label><Radio value="1" /><span className="check" />1 min</label></li>
                  <li className="node-option"><label><Radio value="5" /><span className="check" />5 min</label></li>
                </ul>
              )}
            </RadioGroup>
          </div>
        </div>

        {nodeElems}

      </div>
    );
  }

  handleMetric(selectedMetric: string) {
    this.setState({ selectedMetric });
  }

  handleTime(selectedTime: string) {
    this.setState({ selectedTime });
  }
}
