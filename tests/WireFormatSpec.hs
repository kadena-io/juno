{-# LANGUAGE OverloadedStrings #-}

module WireFormatSpec where

import Test.Hspec
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Juno.Types

spec :: Spec
spec = describe "WireFormat RoundTrips" testWireRoundtrip

testWireRoundtrip :: Spec
testWireRoundtrip = do
  it "Command" $
    fromWire Nothing keySet cmdSignedRPC1
      `shouldBe`
        (Right $ cmdRPC1 {
            _cmdProvenance = ReceivedMsg
              { _pDig = _sigDigest cmdSignedRPC1
              , _pOrig = _sigBody cmdSignedRPC1
              , _pTimeStamp = Nothing}})
  it "CommandBatch" $
    fromWire Nothing keySet cmdbSignedRPC
      `shouldBe`
        (Right $ cmdbRPC {
              _cmdbBatch = [cmdRPC1', cmdRPC2']
            , _cmdbProvenance = ReceivedMsg
              { _pDig = _sigDigest cmdbSignedRPC
              , _pOrig = _sigBody cmdbSignedRPC
              , _pTimeStamp = Nothing}})
  it "CommandResponse" $
    fromWire Nothing keySet cmdrSignedRPC
      `shouldBe`
        (Right $ cmdrRPC {
            _cmdrProvenance = ReceivedMsg
              { _pDig = _sigDigest cmdrSignedRPC
              , _pOrig = _sigBody cmdrSignedRPC
              , _pTimeStamp = Nothing}})
  it "Seq LogEntry" $
    leSeqDecoded `shouldBe` leSeq
  it "RequestVoteResponse" $
    fromWire Nothing keySet rvrSignedRPC1
      `shouldBe`
        (Right $ rvrRPC1 {
            _rvrProvenance = ReceivedMsg
              { _pDig = _sigDigest rvrSignedRPC1
              , _pOrig = _sigBody rvrSignedRPC1
              , _pTimeStamp = Nothing}})
  it "Set RequestVoteResponse" $
    decodeRVRWire Nothing keySet rvrSignedRPCList `shouldBe` Right rvrRPCSet'
  it "AppendEntries" $
    fromWire Nothing keySet aeSignedRPC
      `shouldBe`
        (Right $ aeRPC {
            _aeProvenance = ReceivedMsg
              { _pDig = _sigDigest aeSignedRPC
              , _pOrig = _sigBody aeSignedRPC
              , _pTimeStamp = Nothing}})
  it "AppendEntriesResponse" $
    fromWire Nothing keySet aerSignedRPC
      `shouldBe`
        (Right $ aerRPC {
            _aerProvenance = ReceivedMsg
              { _pDig = _sigDigest aerSignedRPC
              , _pOrig = _sigBody aerSignedRPC
              , _pTimeStamp = Nothing}})
  it "RequestVote" $
    fromWire Nothing keySet rvSignedRPC
      `shouldBe`
        (Right $ rvRPC {
            _rvProvenance = ReceivedMsg
              { _pDig = _sigDigest rvSignedRPC
              , _pOrig = _sigBody rvSignedRPC
              , _pTimeStamp = Nothing}})
  it "Revolution" $
    fromWire Nothing keySet revSignedRPC
      `shouldBe`
        (Right $ revRPC {
            _revProvenance = ReceivedMsg
              { _pDig = _sigDigest revSignedRPC
              , _pOrig = _sigBody revSignedRPC
              , _pTimeStamp = Nothing}})


-- ##########################################################
-- ####### All the stuff we need to actually run this #######
-- ##########################################################

-- #######################################################################
-- NodeID's + Keys for Client (10002), Leader (10000) and Follower (10001)
-- #######################################################################
nodeIdLeader, nodeIdFollower, nodeIdClient :: NodeID
nodeIdLeader = NodeID "localhost" 10000 "tcp://127.0.0.1:10000"
nodeIdFollower = NodeID "localhost" 10001 "tcp://127.0.0.1:10001"
nodeIdClient = NodeID "localhost" 10002 "tcp://127.0.0.1:10002"

privKeyLeader, privKeyFollower, privKeyClient :: PrivateKey
privKeyLeader = maybe (error "bad leader key") id $ importPrivate "\204m\223Uo|\211.\144\131\&5Xmlyd$\165T\148\&11P\142m\249\253$\216\232\220c"
privKeyFollower = maybe (error "bad leader key") id $ importPrivate "$%\181\214\b\138\246(5\181%\199\186\185\t!\NUL\253'\t\ENQ\212^\236O\SOP\217\ACK\EOT\170<"
privKeyClient = maybe (error "bad leader key") id $ importPrivate "8H\r\198a;\US\249\233b\DLE\211nWy\176\193\STX\236\SUB\151\206\152\tm\205\205\234(\CAN\254\181"

pubKeyLeader, pubKeyFollower, pubKeyClient :: PublicKey
pubKeyLeader = maybe (error "bad leader key") id $ importPublic "f\t\167y\197\140\&2c.L\209;E\181\146\157\226\137\155$\GS(\189\215\SUB\199\r\158\224\FS\190|"
pubKeyFollower = maybe (error "bad leader key") id $ importPublic "\187\182\129\&4\139\197s\175Sc!\237\&8L \164J7u\184;\CANiC\DLE\243\ESC\206\249\SYN\189\ACK"
pubKeyClient = maybe (error "bad leader key") id $ importPublic "@*\228W(^\231\193\134\239\254s\ETBN\208\RS\137\201\208,bEk\213\221\185#\152\&7\237\234\DC1"

keySet :: KeySet
keySet = KeySet
  { _ksCluster = Map.fromList [(nodeIdLeader, pubKeyLeader),(nodeIdFollower, pubKeyFollower)]
  , _ksClient = Map.fromList [(nodeIdClient, pubKeyClient)] }

-- #####################################
-- Commands, with and without provenance
-- #####################################
cmdbRPC :: CommandBatch
cmdbRPC = CommandBatch
  { _cmdbBatch = [cmdRPC1, cmdRPC2]
  , _cmdbProvenance = NewMsg }

cmdbSignedRPC :: SignedRPC
cmdbSignedRPC = toWire nodeIdClient pubKeyClient privKeyClient cmdbRPC

-- these are signed (received) provenance versions
cmdbRPC' :: CommandBatch
cmdbRPC' = (\(Right v) -> v) $ fromWire Nothing keySet cmdbSignedRPC


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
cmdRPC1' = (\(Right v) -> v) $ fromWire Nothing keySet cmdSignedRPC1
cmdRPC2' = (\(Right v) -> v) $ fromWire Nothing keySet cmdSignedRPC2

-- #############################################
-- CommandResponse, with and without provenance
-- #############################################
cmdrRPC :: CommandResponse
cmdrRPC = CommandResponse
  { _cmdrResult     = CommandResult "account created: foo"
  , _cmdrLeaderId   = nodeIdLeader
  , _cmdrNodeId     = nodeIdLeader
  , _cmdrRequestId  = RequestId 1
  , _cmdrLatency    = 1
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
leSeqDecoded = (\(Right v) -> v) $ decodeLEWire Nothing keySet leWire

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
rvrRPC1' = (\(Right v) -> v) $ fromWire Nothing keySet rvrSignedRPC1
rvrRPC2' = (\(Right v) -> v) $ fromWire Nothing keySet rvrSignedRPC2

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
aeRPC' = (\(Right v) -> v) $ fromWire Nothing keySet aeSignedRPC

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
  , _aerWasVerified = True
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
  , _rvLastLogIndex  = LogIndex (-1)
  , _rvLastLogTerm   = Term (-1)
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
