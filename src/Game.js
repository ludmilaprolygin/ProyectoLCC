import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import Square from './Square';
import { joinResult } from './util';
import { smallerPow2GreaterOrEqualThan } from './util';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);
  const [suma, setSuma] = useState(0);

  useEffect(() => {
    // This is executed just once, after the first render.
    PengineClient.init(onServerReady);
  }, []);

  function handleClickBooster() {
      const gridS = JSON.stringify(grid);
      const queryS = "booster(" + gridS + "," + numOfColumns + ", RGrids)";    
      pengine.query(queryS, (success, response) => {        
        if (success) {          
          animateEffect(response['RGrids'], 500); 
        } else {
          setWaiting(false);
        }        
      }); 
      
    }

    function handleClickMovidaMaxima() {
      const gridS = JSON.stringify(grid);
      const queryS = "movida_maxima(" + gridS + "," + numOfColumns + ", SumaPath, Path)";    
      pengine.query(queryS, (success, response) => {        
        if (success) { 
          if(response['Path'].length === 0)
            gameOver();
          else
          {
            setSuma(smallerPow2GreaterOrEqualThan(joinResult(response['Path'], grid, numOfColumns)));
            setPath(response['Path']);
          }
        } else {
          setWaiting(false);
        }        
      }); 
      
    }  

    function gameOver()
    {

    }

  /**
   * Called when the server was successfully initialized
   */
  function onServerReady(instance) {
    pengine = instance;
    const queryS = 'init(Grid, NumOfColumns)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setNumOfColumns(response['NumOfColumns']);
      }
    });
  }

  /**
   * Called while the user is drawing a path in the grid, each time the path changes.
   */
  function onPathChange(newPath) {
    // No effect if waiting.
    if (waiting) {
      return;
    }
    setPath(newPath);
    if(newPath.length > 1)
      setSuma(smallerPow2GreaterOrEqualThan(joinResult(newPath, grid, numOfColumns)));
    else
      setSuma(0);
  }

  /**
   * Called when the user finished drawing a path in the grid.
   */
  function onPathDone() {
    /*
    Build Prolog query, which will be like:
    join([
          64,4,64,32,16,
          64,8,16,2,32,
          2,4,64,64,2,
          2,4,32,16,4,
          16,4,16,16,16,
          16,64,2,32,32,
          64,2,64,32,64,
          32,2,64,32,4
          ], 
          5, 
          [[2, 0], [3, 0], [4, 1], [3, 1], [2, 1], [1, 1], [1, 2], [0, 3]],
          RGrids
        ).
    */
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setPath([]);
        animateEffect(response['RGrids']);
        setSuma(0); // Setea en vacio porque ya finalizó la elección del camino con un click
      } else {
        setWaiting(false);
      }
    });
  }

  

  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids) {
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {
      setTimeout(() => {
        animateEffect(restRGrids);
      }, 900);
    } else {
      setWaiting(false);
    }
  }

  if (grid === null) {
    return null;
  }
  return (
    <div className="game">
      <div className="header">
        {path.length > 1 ? 
        
            <Square
              value={suma}
            /> :
            
            <div className="score">{score}</div>
        }
        
      </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      />
      <button className="funcionalidades-button" onClick={handleClickMovidaMaxima}> camino maximo </button>
      <button className="funcionalidades-button" onClick={handleClickBooster}> 💣 Colapsar iguales 💣 </button>
    </div>
  );
}

export default Game;