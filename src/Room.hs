module Room where

import Algebra.Graph.Undirected

type RoomGraph = Graph Room

data RoomType = End | Hallway | Fork | PassThrough deriving (Show,Eq,Ord,Enum)
data Room = Room { name::String, roomType::RoomType} deriving (Show,Eq,Ord)



-- roomGraph :: Graph Room
-- roomGraph = connect (vertex cryptRoom) (vertex)


hasUnexploredExit :: Room -> Graph (Graph Room) -> Bool 
hasUnexploredExit Room {roomType = End} _ = False
hasUnexploredExit room@Room {roomType = roomType} roomGraph = (1 + fromEnum roomType) >  length (neighbours (vertex room ) roomGraph)

-- generateRoom :: Room -> RoomGraph -> RoomGraph
-- generateRoom currentRoom 

-- exit 1 -> room2
-- exit 2 -> ?
-- exit 3 -> ?