require('./public/css/reset.css');
require('./public/css/font-awesome.min.css');
require('./public/css/bootstrap.min.css');
require('./public/css/style.css');
import React from 'react';
import ReactDOM from 'react-dom';

//// import Health from './health';
//// import Nodes from './nodes';
//// import Consensus from './consensus';
import HeaderNav from './header-nav';
import Sidebar from './sidebar';
import Detail from './detail';


//// const ports = [10000, 10001, 10002, 10003];

////// number of datapoints to keep around (5 mins + 1 min)
//// const dataWindow = 60 * 6;

//// const LOST_NODE = 'LOST_NODE';


class App extends React.Component {
  constructor(props) {
    super();

    const data = {

    };
    //// for (let port of ports) {
    ////   data[`127.0.0.1:${port}`] = LOST_NODE;
    //// };

    this.state = {
      currentPane: "add-payments"
    };

  }

  handleChangePane(currentPane) {
    this.setState({currentPane});
    //alert(currentPane);
  }
  render() {
////    const {leaderData, data} = this.state;
    const {data,currentPane} = this.state;
    return (
      <div className="app">
        <HeaderNav currentPane={currentPane} />
        <Sidebar handleChangePane={(pane)=>this.handleChangePane(pane)} currentPane={currentPane} />
        <Detail currentPane={currentPane}/>

   </div>

////      <section>
////        <header>
////          <nav class="rad-navigation">
////            <div class="rad-logo-container">
////              <a href="#" class="rad-logo">Payments Ledger</a>
////              <a href="#" class="rad-toggle-btn pull-right"><i class="fa fa-bars"></i></a>
////            </div>
////	      <a href="#" class="rad-logo-hidden">Payments Ledger</a>
////      <div className="app">
////        <h1 className="cluster-header">
////          JUNO CLUSTER
////          <div className="border-underline" />
////        </h1>
////        <div className="float-section">
////          <Consensus data={leaderData} />
////          <Health data={leaderData} />
////        </div>
////        <div className="float-section">
////          <Nodes data={data} />
////        </div>
////      </div>
    );
  }

  componentDidMount() {
    ////this._setInterval();
  }

  componentDidUpdate() {
    ////this._setInterval();
  }

////  _setInterval() {
////    for (let port of ports) {
////      const id = `_id${port}`;
////      window.clearInterval(this[id]);
////      this[id] = window.setInterval(() => {
////        this._fetch(port);
////      }, 1000);
////    }
////  }
////
////  _fetch(port: number) {
////    fetch(`//localhost:${port+80}`, {
////      method: 'get',
////      headers: new Headers({
////        'Accept': 'application/json',
////      }),
////      mode: 'cors'
////    }).then(response => response.json())
////      .then(newDatum => {
////        const nodeDatum = newDatum.juno.node;
////        const id = nodeDatum.id.val;
////        const role = nodeDatum.role.val;
////        const appliedIndex = nodeDatum.applied_index.val;
////        const leaderData = role === "Leader" ? newDatum : this.state.leaderData;
////        const stateData = this.state.data;
////
////        // re-initialize if we've found it again; add in new data and cap the
////        // size at the number of data points we track
////        const newData = stateData[id] === LOST_NODE
////          ? [newDatum]
////          : stateData[id].data.concat([newDatum]).slice(-dataWindow);
////
////        const data = {
////          ...stateData,
////          [id]: {
////            data: newData,
////            role,
////            appliedIndex,
////          },
////        };
////
////        this.setState({ data, leaderData });
////    }).catch(err => {
////      const id = `127.0.0.1:${port}`;
////      const stateData = this.state.data;
////      const data = {
////        ...stateData,
////        [id]: LOST_NODE,
////      };
////
////      this.setState({data});
////    });
////  }
////
}

ReactDOM.render(<App />, document.querySelector("#myApp"));
