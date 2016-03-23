{-# LANGUAGE OverloadedStrings #-}

module WireFormatSpec where

import Test.Hspec
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Juno.Runtime.Types


spec :: Spec
spec = describe "WireFormat RoundTrips" testWireRoundtrip

testWireRoundtrip :: Spec
testWireRoundtrip = do
  it "Command" $
    fromWire defaultReceivedAt keySet cmdSignedRPC1
      `shouldBe`
        (Right $ cmdRPC1 {
            _cmdProvenance = ReceivedMsg
              { _pDig = _sigDigest cmdSignedRPC1
              , _pOrig = _sigBody cmdSignedRPC1
              , _pTimeStamp = defaultReceivedAt}})

  it "CommandResponse" $
    fromWire defaultReceivedAt keySet cmdrSignedRPC
      `shouldBe`
        (Right $ cmdrRPC {
            _cmdrProvenance = ReceivedMsg
              { _pDig = _sigDigest cmdrSignedRPC
              , _pOrig = _sigBody cmdrSignedRPC
              , _pTimeStamp = defaultReceivedAt}})
  it "Seq LogEntry" $
    leSeqDecoded `shouldBe` leSeq
  it "RequestVoteResponse" $
    fromWire defaultReceivedAt keySet rvrSignedRPC1
      `shouldBe`
        (Right $ rvrRPC1 {
            _rvrProvenance = ReceivedMsg
              { _pDig = _sigDigest rvrSignedRPC1
              , _pOrig = _sigBody rvrSignedRPC1
              , _pTimeStamp = defaultReceivedAt}})
  it "Set RequestVoteResponse" $
    decodeRVRWire defaultReceivedAt keySet rvrSignedRPCList `shouldBe` Right rvrRPCSet'
  it "AppendEntries" $
    fromWire defaultReceivedAt keySet aeSignedRPC
      `shouldBe`
        (Right $ aeRPC {
            _aeProvenance = ReceivedMsg
              { _pDig = _sigDigest aeSignedRPC
              , _pOrig = _sigBody aeSignedRPC
              , _pTimeStamp = defaultReceivedAt}})
  it "AppendEntriesResponse" $
    fromWire defaultReceivedAt keySet aerSignedRPC
      `shouldBe`
        (Right $ aerRPC {
            _aerProvenance = ReceivedMsg
              { _pDig = _sigDigest aerSignedRPC
              , _pOrig = _sigBody aerSignedRPC
              , _pTimeStamp = defaultReceivedAt}})
  it "RequestVote" $
    fromWire defaultReceivedAt keySet rvSignedRPC
      `shouldBe`
        (Right $ rvRPC {
            _rvProvenance = ReceivedMsg
              { _pDig = _sigDigest rvSignedRPC
              , _pOrig = _sigBody rvSignedRPC
              , _pTimeStamp = defaultReceivedAt}})
  it "Revolution" $
    fromWire defaultReceivedAt keySet revSignedRPC
      `shouldBe`
        (Right $ revRPC {
            _revProvenance = ReceivedMsg
              { _pDig = _sigDigest revSignedRPC
              , _pOrig = _sigBody revSignedRPC
              , _pTimeStamp = defaultReceivedAt}})


-- ##########################################################
-- ####### All the stuff we need to actually run this #######
-- ##########################################################

-- #######################################################################
-- NodeID's + Keys for Client (10002), Leader (10000) and Follower (10001)
-- #######################################################################
nodeIdLeader, nodeIdFollower, nodeIdClient :: NodeID
nodeIdLeader = NodeID "localhost" 10000
nodeIdFollower = NodeID "localhost" 10001
nodeIdClient = NodeID "localhost" 10002

privKeyLeader, privKeyFollower, privKeyClient :: SecretKey
privKeyLeader = SecretKey "\132h\138\225\233\237%\\\SOnZH\196\138\232\&7\239c'p)YE\192\136\DC3\217\170N\231n\236\199\NAK\238\171\\\161\222\247\186/\DC3\204Qqd\225}\202\150e~q\255;\223\233\211:\211\SUBT\145"
privKeyFollower = SecretKey "\244\228\130\r\213\134\171\205!\141z\238\nJd\170\208%_\188\196\150\152$\178\153\SO\240\192\&4\202Q\164}\DC2`\245Bh-Mj!\227\220A\EOTfN\129\&5\213Z\ENQ\155\129\155d\SUB\129\194&\SUB4"
privKeyClient = SecretKey "h\129\140\207.\166\210\253\STXo\FS\201\186\185a\202\240\158\234\132\254\212\ETB\138\220\189a2\232K\128\SOH[\DC4\228\242  \209A\161\219\179\223(ty\FS$!{(\230\DC4V\184~\133>\255|\RS,\231"

pubKeyLeader, pubKeyFollower, pubKeyClient :: PublicKey
pubKeyLeader = toPublicKey privKeyLeader
pubKeyFollower = toPublicKey privKeyFollower
pubKeyClient = toPublicKey privKeyClient

keySet :: KeySet
keySet = KeySet
  { _ksCluster = Map.fromList [(nodeIdLeader, pubKeyLeader),(nodeIdFollower, pubKeyFollower)]
  , _ksClient = Map.fromList [(nodeIdClient, pubKeyClient)] }


-- #####################################
-- Commands, with and without provenance
-- #####################################
cmdRPC1, cmdRPC2 :: Command
cmdRPC1 = Command
  { _cmdEntry = CommandEntry "CreateAccount foo"
  , _cmdClientId = nodeIdClient
  , _cmdRequestId = RequestId 0
  , _cmdProvenance = NewMsg }
cmdRPC2 = Command
  { _cmdEntry = CommandEntry "CreateAccount foo"
  , _cmdClientId = nodeIdClient
  , _cmdRequestId = RequestId 1
  , _cmdProvenance = NewMsg }

cmdSignedRPC1, cmdSignedRPC2 :: SignedRPC
cmdSignedRPC1 = toWire nodeIdClient pubKeyClient privKeyClient cmdRPC1
cmdSignedRPC2 = toWire nodeIdClient pubKeyClient privKeyClient cmdRPC2

-- these are signed (received) provenance versions
cmdRPC1', cmdRPC2' :: Command
cmdRPC1' = (\(Right v) -> v) $ fromWire defaultReceivedAt keySet cmdSignedRPC1
cmdRPC2' = (\(Right v) -> v) $ fromWire defaultReceivedAt keySet cmdSignedRPC2

-- #############################################
-- CommandResponse, with and without provenance
-- #############################################
cmdrRPC :: CommandResponse
cmdrRPC = CommandResponse
  { _cmdrResult     = CommandResult "account created: foo"
  , _cmdrLeaderId   = nodeIdLeader
  , _cmdrNodeId     = nodeIdLeader
  , _cmdrRequestId  = RequestId 1
  , _cmdrProvenance = NewMsg
  }

cmdrSignedRPC :: SignedRPC
cmdrSignedRPC = toWire nodeIdLeader pubKeyLeader privKeyLeader cmdrRPC

-- ########################################################
-- LogEntry(s) and Seq LogEntry with correct hashes.
-- LogEntry is not an RPC but is(are) nested in other RPCs.
-- Given this, they are handled differently (no provenance)
-- ########################################################
logEntry1, logEntry2 :: LogEntry
logEntry1 = LogEntry
  { _leTerm    = Term 0
  , _leCommand = cmdRPC1'
  , _leHash    = "\237\157D\GS\158k\214\188\219.,\248\226\232\174\227\228\236R\t\189\&3v\NUL\255\&5\224\&4|\178\STX\252"
  }
logEntry2 = LogEntry
  { _leTerm    = Term 0
  , _leCommand = cmdRPC2'
  , _leHash    = "\244\136\187c\222\164\131\178;D)M\DEL\142|\251Kv\213\186\247q;3`\194\227O\US\223Q\157"
  }

leSeq, leSeqDecoded :: Seq LogEntry
leSeq = Seq.fromList [logEntry1, logEntry2]
leSeqDecoded = (\(Right v) -> v) $ decodeLEWire defaultReceivedAt keySet leWire

leWire :: [LEWire]
leWire = encodeLEWire nodeIdLeader pubKeyLeader privKeyLeader leSeq

-- ################################################
-- RequestVoteResponse, with and without provenance
-- ################################################

rvrRPC1, rvrRPC2 :: RequestVoteResponse
rvrRPC1 = RequestVoteResponse
  { _rvrTerm        = Term 0
  , _rvrCurLogIndex = LogIndex (-1)
  , _rvrNodeId      = nodeIdLeader
  , _voteGranted    = True
  , _rvrCandidateId = nodeIdLeader
  , _rvrProvenance  = NewMsg
  }
rvrRPC2 = RequestVoteResponse
  { _rvrTerm        = Term 0
  , _rvrCurLogIndex = LogIndex (-1)
  , _rvrNodeId      = nodeIdFollower
  , _voteGranted    = True
  , _rvrCandidateId = nodeIdLeader
  , _rvrProvenance  = NewMsg
  }

rvrSignedRPC1, rvrSignedRPC2 :: SignedRPC
rvrSignedRPC1 = toWire nodeIdLeader pubKeyLeader privKeyLeader rvrRPC1
rvrSignedRPC2 = toWire nodeIdFollower pubKeyFollower privKeyFollower rvrRPC2

rvrRPC1', rvrRPC2' :: RequestVoteResponse
rvrRPC1' = (\(Right v) -> v) $ fromWire defaultReceivedAt keySet rvrSignedRPC1
rvrRPC2' = (\(Right v) -> v) $ fromWire defaultReceivedAt keySet rvrSignedRPC2

rvrRPCSet' :: Set RequestVoteResponse
rvrRPCSet' = Set.fromList [rvrRPC1', rvrRPC2']

rvrSignedRPCList :: [SignedRPC]
rvrSignedRPCList = [rvrSignedRPC1, rvrSignedRPC2]

-- #############################################
-- AppendEntries, with and without provenance
-- #############################################
aeRPC :: AppendEntries
aeRPC = AppendEntries
  { _aeTerm        = Term 0
  , _leaderId      = nodeIdLeader
  , _prevLogIndex  = LogIndex (-1)
  , _prevLogTerm   = Term 0
  , _aeEntries     = leSeq
  , _aeQuorumVotes = rvrRPCSet'
  , _aeProvenance  = NewMsg
  }

aeSignedRPC :: SignedRPC
aeSignedRPC = toWire nodeIdLeader pubKeyLeader privKeyLeader aeRPC

aeRPC' :: RequestVoteResponse
aeRPC' = (\(Right v) -> v) $ fromWire defaultReceivedAt keySet aeSignedRPC

-- #####################
-- AppendEntriesResponse
-- #####################
aerRPC :: AppendEntriesResponse
aerRPC = AppendEntriesResponse
  { _aerTerm       = Term 0
  , _aerNodeId     = nodeIdFollower
  , _aerSuccess    = True
  , _aerConvinced  = True
  , _aerIndex      = LogIndex 1
  , _aerHash       = "\244\136\187c\222\164\131\178;D)M\DEL\142|\251Kv\213\186\247q;3`\194\227O\US\223Q\157"
  , _aerProvenance = NewMsg
  }

aerSignedRPC :: SignedRPC
aerSignedRPC = toWire nodeIdFollower pubKeyFollower privKeyFollower aerRPC

-- ###########
-- RequestVote
-- ###########
rvRPC :: RequestVote
rvRPC = RequestVote
  { _rvTerm        = Term 0
  , _rvCandidateId = nodeIdLeader
  , _lastLogIndex  = LogIndex (-1)
  , _lastLogTerm   = Term (-1)
  , _rvProvenance  = NewMsg
  }

rvSignedRPC :: SignedRPC
rvSignedRPC = toWire nodeIdLeader pubKeyLeader privKeyLeader rvRPC

-- ##########
-- Revolution
-- ##########
revRPC :: Revolution
revRPC = Revolution
  { _revClientId   = nodeIdClient
  , _revLeaderId   = nodeIdLeader
  , _revRequestId  = RequestId 2
  , _revProvenance = NewMsg
  }

revSignedRPC :: SignedRPC
revSignedRPC = toWire nodeIdClient pubKeyClient privKeyClient revRPC
