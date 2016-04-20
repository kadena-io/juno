require('./public/css/reset.css');
require('./public/css/font-awesome.min.css');
require('./public/css/bootstrap.min.css');
require('./public/css/style.css');
import React from 'react';
import ReactDOM from 'react-dom';

import HeaderNav from './header-nav';
import Sidebar from './sidebar';
import Detail from './detail';


//// const ports = [10000, 10001, 10002, 10003];

////// number of datapoints to keep around (5 mins + 1 min)
//// const dataWindow = 60 * 6;

//// const LOST_NODE = 'LOST_NODE';

const junoUrl = '//localhost:8000/api';
const [londonNostro,tokyoNostro,londonBranch,tokyoBranch]=['101','102','100','103'];
const accounts = {
  '003': {
    name: 'Nintendo',
    type: 'Client'
  },
  '004': {
    name: 'Sony',
    type: 'Client'
  },

  '000': {
    name: 'Tesla',
    type: 'Client'
  },

  '001': {
    name: 'Amazon',
    type: 'Client'
  },

  londonBranch: {
    name: 'London',
    type: 'Branch'
  },

  londonNostro: {
    name: 'London Nostro',
    type: 'Correspondent'
  },

  tokyoNostro: {
    name: 'Tokyo Nostro',
    type: 'Correspondent'
  },

  tokyoBranch: {
    name: 'Tokyo',
    type: 'Branch'
  }
};


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
    switch (currentPane) {
    case "add-payments":
      break;
    case "new-york-branch":
      this.fetchNostro(currentPane);
      break;
    case "london-branch":
      break;
    case "tokyo-branch":
      break;
    }
  }

  fetchNostro(currentPane) {
    fetch(`${junoUrl}/ledger-query?account=${londonNostro}`, {
      method: 'get',
      mode: 'cors'
    }).then(response => response.json())
      .then(jsonData => {
        const model = jsonData.trans;
        this.setState({model});
      });
  }

  render() {
    const {model,currentPane} = this.state;
    return (
      <div className="app">
        <HeaderNav currentPane={currentPane} />
        <Sidebar handleChangePane={(pane)=>this.handleChangePane(pane)} currentPane={currentPane} />
        <Detail currentPane={currentPane} junoUrl={junoUrl} accounts={accounts} model={model}
      tokyoNostro={tokyoNostro} londonNostro={londonNostro}
      londonBranch={londonBranch} tokyoBranch={tokyoBranch}/>
      </div>
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
