require('./public/css/reset.css');
require('./public/css/font-awesome.min.css');
require('./public/css/bootstrap.min.css');
require('./public/css/style.css');
import React from 'react';
import ReactDOM from 'react-dom';

import HeaderNav from './header-nav';
import Sidebar from './sidebar';
import Detail from './detail';
import SwiftDetails from './swift-detail.js';


const londonNostro = '101',
      tokyoNostro = '102',
      londonBranch = '100',
      tokyoBranch = '103',
      nintendo = '003',
      sony = '004',
      tesla = '000',
      amazon = '001';

const accounts = { londonNostro,tokyoNostro,londonBranch,tokyoBranch,
                   nintendo,sony,tesla,amazon };

const acctInfo = {
  [nintendo]: { name: 'Nintendo', type: 'Client' },
  [sony]: { name: 'Sony', type: 'Client' },
  [tesla]: { name: 'Tesla', type: 'Client' },
  [amazon]: { name: 'Amazon', type: 'Client' },
  [londonBranch]: { name: 'London', type: 'Branch' },
  [londonNostro]: { name: 'London Nostro', type: 'Correspondent' },
  [tokyoNostro]: { name: 'Tokyo Nostro', type: 'Correspondent' },
  [tokyoBranch]: { name: 'Tokyo', type: 'Branch' }
};

const initPane = "add-payments";


class App extends React.Component {
  constructor(props) {
    super();

    this.state = {
      currentPane: initPane,
      junoUrl: '/api'
    };

  }

  handleChangePane(currentPane) {
    this.setState({currentPane});
    switch (currentPane) {
    case "add-payments":
      break;
    case "new-york-branch":
      this.fetchNostro();
      break;
    case "london-branch":
      this.fetchBranch(londonBranch);
      break;
    case "tokyo-branch":
      this.fetchBranch(tokyoBranch);
      break;
    }
  }

  handleSwiftText(e) {
    this.setState({ swiftText: e.target.value, submitResponse: null });
  }

  handleSwiftSubmit(e) {
    e.preventDefault();
    if (this.state.swiftText == null || this.state.swiftText == "") { return; }
    this.setState({ submitResponse: { status: "Sending ..." }});
    fetch(`${this.state.junoUrl}/swift-submit`, {
      method: 'POST',
      mode: 'cors',
      headers: { 'Content-Type': 'text/plain' },
      body: this.state.swiftText
    }).then(response => response.json())
      .then(response => {
        this.setState({ submitResponse: response });
    });

  }

  fetchTx(transId,branch) {
    fetch(`${this.state.junoUrl}/ledger-query?tx=${transId}`, {
      method: 'get',
      mode: 'cors'
    }).then(response => response.json())
      .then(jsonData => {
        this.setState((prev,curr) => {
          const txData = prev.txData == null ? {} : prev.txData;
          txData[branch] = jsonData;
          return { txData };
        });
      });
  }

  fetchNostro() {
    fetch(`${this.state.junoUrl}/ledger-query?account=${londonNostro}`, {
      method: 'get',
      mode: 'cors'
    }).then(response => response.json())
      .then(jsonData => {
        const nostroData = jsonData.trans;
        this.setState({nostroData});
      });
  }

  fetchBranch(branch) {
    fetch(`${this.state.junoUrl}/ledger-query?account=${branch}`, {
      method: 'get',
      mode: 'cors'
    }).then(response => response.json())
      .then(jsonData => {
        this.setState((prev,curr) => {
          const branchData = prev.branchData == null ? {} : prev.branchData;
          branchData[branch] = jsonData;
          return { branchData };
        });
      });

  }

handleJunoUrlChange(e) {
  this.setState({junoUrl: e.target.value});
}

handleJunoUrlSubmit(e) {
  e.preventDefault();
  const s = {currentPane: this.state.currentPane,
             junoUrl: this.state.junoUrl}
  this.resetState(s);

}

  render() {
    return (
        <div className="app">
        <HeaderNav handleJunoUrlChange={e=>this.handleJunoUrlChange(e)}
           handleJunoUrlSubmit={this.handleJunoUrlSubmit}
           {...this.state} />
        <Sidebar handleChangePane={(pane)=>this.handleChangePane(pane)} {...this.state} />
        <Detail acctInfo={acctInfo}
      handleSwiftText={e=>this.handleSwiftText(e)}
      handleSwiftSubmit={e=>this.handleSwiftSubmit(e)}
                fetchTx={(t,a)=>this.fetchTx(t,a)}
                {...accounts} {...this.state} />
        </div>
    );
  }

}

ReactDOM.render(<App />, document.querySelector("#myApp"));
