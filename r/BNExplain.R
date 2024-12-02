library(gRain)
library(rjson)


BNVariable <- setRefClass("BNVariable",
                          fields = list(varID = "character",
                                        varStates = "character",
                                        varParents = "list",
                                        varProbs = "list",
                                        varType = "character",
                                        varDist = "list",
                                        varParams = "list",
                                        observed= "character"),
                          methods = list(
                            addParent = function(newPar){
                              varParents <<- append(varParents,newPar)
                            },
                            addParent_byID = function(newParID,varList){
                              for (i in 1:length(varList)) {
                                if(newParID==varList[[i]]$varID){
                                  addParent(varList[[i]])
                                }
                              }
                            },
                            addProbability = function(newProbs){
                              varProbs <<- append(varProbs,newProbs)
                            },
                            show = function(){
                              cat('\nThe variable is:',varID)
                              cat('\nThe variable type is:',varType)
                              if(.self$varType!="ContinuousInterval"){
                                cat('\nVariable states are:',varStates)
                              }
                              else{
                                cat("\nVariable distribution is: A mixture of ")
                                for (i in 1:length(.self$varDist)) {
                                  cat(.self$varDist[[i]], ' ')
                                  
                                }
                                
                              }
                               
                              
                              cat('\nVariable parents are: ')
                              if(length(varParents)>0){
                                for (i in 1:length(varParents)) {
                                  cat(varParents[[i]]$varID, ' ')
                                } 
                              }
                              else
                                cat('NO PARENTS')
                            },
                            getParentIDs = function(){
                              parList <- c()
                              if(length(varParents)>0){
                                for (i in 1:length(varParents)) {
                                  parList[i] <- varParents[[i]]$varID
                                }
                              }
                              else{
                                parList <- NULL
                              }
                              return(parList)
                            },
                            setObservation = function(stateObs){
                              observed <<- stateObs
                            },
                            getObservation = function(){
                              obsv <- .self$observed
                              return(obsv)
                            },
                            clearObservation = function(){
                              observed <<- character(0)
                            }))

BNEdge <- setRefClass("BNEdge",
                      fields = list(fromVar = "BNVariable",
                                    toVar = "BNVariable",
                                    relType = "character"),
                      methods = list(
                        show = function(){
                          cat('\nThe edge is from: ',fromVar$varID)
                          cat('\nThe edge is to: ', toVar$varID)
                        }
                      ))

createEdges <- function(inputVar){
  edges <- list()
  if(length(inputVar$varParents)>0){
    for (i in 1:length(inputVar$varParents)) {
      edges[[i]] <- BNEdge$new(fromVar = inputVar$varParents[[i]], toVar = inputVar)
    }
  }
  return(edges)
}

BayesianNetwork <- setRefClass("BayesianNetwork",
                               fields = list(modelVariables = "list",
                                             modelEdges = "list"),
                               methods = list(
                                 addVariable = function(newVar){
                                   modelVariables <<- append(modelVariables,newVar)
                                 },
                                 addEdge = function(newVar){
                                   modelEdges <<- append(modelEdges,createEdges(newVar))
                                 },
                                 initialize = function(varList=NULL){
                                   for (i in 1:length(varList)) {
                                     addVariable(varList[[i]])
                                     addEdge(varList[[i]])
                                   }
                                   names(modelVariables) <<- names(varList)
                                 },
                                 updateEdges = function(){
                                   if(length(modelEdges)==0){
                                     for (i in 1:length(modelVariables)) {
                                       modelEdges <<- append(modelEdges,createEdges(modelVariables[[i]]))
                                     }
                                   }},
                                 show = function(){
                                   cat('\nVariables in this BN:\n\t')
                                   for (i in 1:length(modelVariables)) {
                                     cat(modelVariables[[i]]$varID, ' ')
                                   }
                                   if(!is.null(getObserved())){
                                     cat('\nObservations in this BN:\n\t')
                                     for (i in 1:length(getObserved())) {
                                       cat('(',getObserved()[i],' = ',getObservations()[i],') ',sep="")
                                     }
                                   }
                                   cat('\nRelations in this BN:')
                                   for (i in 1:length(modelEdges)) {
                                     cat('\n\t(',modelEdges[[i]]$fromVar$varID,' -> ',modelEdges[[i]]$toVar$varID,') ',sep="")
                                   }
                                 },
                                 getVariables = function(){
                                   varList <- c()
                                   if(length(modelVariables)>0){
                                     for (i in 1:length(modelVariables)) {
                                       varList[i] <- modelVariables[[i]]$varID
                                     }
                                   }
                                   else
                                     varList <- NULL
                                   return(varList)
                                 },
                                 getObservations = function(){
                                   if(length(modelVariables)>0){
                                     obsStateList <- c()
                                     for (i in 1:length(modelVariables)) {
                                       if(length(modelVariables[[i]]$observed)>0)
                                         obsStateList <- append(obsStateList,modelVariables[[i]]$observed)
                                     }
                                     return(obsStateList)
                                   }
                                 },
                                 getObserved = function(){
                                   if(length(modelVariables)>0){
                                     obsVarList <- c()
                                     for (i in 1:length(modelVariables)) {
                                       if(length(modelVariables[[i]]$observed)>0)
                                         obsVarList <- append(obsVarList,modelVariables[[i]]$varID)
                                     }
                                     return(obsVarList)
                                   }
                                 },
                                 clearAllObservations = function(){
                                   for (i in 1:length(modelVariables)) {
                                     modelVariables[[i]]$clearObservation()
                                   }
                                   return(.self)
                                 }))

ExplanationFramework <- setRefClass("ExplanationFramework",
                                    fields = list(eviVars = "list",
                                                  interVars = "list",
                                                  targetVar = "BNVariable",
                                                  stateOfInt = "character"),
                                    methods = list(
                                      initialize = function(inputBN=NULL,inputPath=NULL){
                                        if(!is.null(inputPath)){
                                          jsonFram <- rjson::fromJSON(file=inputPath)
                                          inputCategories <- jsonFram$categoryList
                                          if(length(inputBN$modelVariables)==length(inputCategories) && length(inputCategories)!=0){
                                            for (i in 1:length(inputCategories)) {
                                              if(inputCategories[[i]]=="Evidence")
                                                eviVars <<- append(eviVars, inputBN$modelVariables[[i]])
                                              else if(inputCategories[[i]]=="Intermediate")
                                                interVars <<- append(interVars, inputBN$modelVariables[[i]])
                                              else if(inputCategories[[i]]=="Target")
                                                targetVar <<- inputBN$modelVariables[[i]]
                                            }
                                          }
                                          state_of_interest <- jsonFram$stateOfInt
                                          if(!is.null(state_of_interest) && state_of_interest %in% targetVar$varStates){
                                            stateOfInt <<- state_of_interest
                                          }
                                        }
                                      },
                                      show = function(){
                                        if(length(eviVars)>0){
                                          cat('\nEvidence variables:\n\t')
                                          for (i in 1:length(eviVars)) {
                                            cat(eviVars[[i]]$varID, ' ')
                                          }
                                        }
                                        if(length(interVars)>0){
                                          cat('\nIntermediate variables:\n\t')
                                          for (i in 1:length(interVars)) {
                                            cat(interVars[[i]]$varID, ' ')
                                          }
                                        }  
                                        cat('\nTarget variable:\n\t')
                                        for (i in 1:length(targetVar)) {
                                          cat(targetVar$varID, ' ')
                                          if(length(stateOfInt)>0){
                                            cat("( State of interest:",stateOfInt,")\n")
                                          }
                                          else{
                                            cat("( State of Interest: Undefined )\n")
                                          }
                                        }
                                      },
                                      getEvidence = function(){
                                        if(length(eviVars)>0){
                                          eviList <- c()
                                          for (i in 1:length(eviVars)){
                                            eviList <- append(eviList,eviVars[[i]]$varID)
                                          }
                                          return(eviList)
                                        }
                                      },
                                      getTarget = function(){
                                        tarName <- targetVar$varID
                                        return(tarName)
                                      },
                                      getIntermediate = function(){
                                        if(length(interVars)>0){
                                          interList <- c()
                                          for (i in 1:length(interVars)){
                                            interList <- append(interList,interVars[[i]]$varID)
                                          }
                                          return(interList)
                                        }
                                      }
                                    ))



#expBN <- eval(parse(text=jsonTemp$expBN))

ExplainableBN <- setRefClass("ExplainableBN",
                             fields = list(bayesnetwork = "BayesianNetwork",
                                           expframework = "ExplanationFramework"),
                             methods = list(
                               initialize = function(inputBN,inputFram=NULL){
                                 bayesnetwork <<- inputBN
                                 if(!is.null(inputFram)){
                                   expframework <<- inputFram  
                                 }
                               },
                               show = function(){
                                 cat('\nThis is an Explainable BN\n')
                                 bayesnetwork$show()
                                 expframework$show()
                               }
                             ))

createExpBN <- function(modelFile,framFile){
  BN <- from_cmpx(modelFile)
  ExpBN <- ExplainableBN$new(BN)
  ExpBN$expframework <- ExplanationFramework$new(BN,framFile)
  return(ExpBN)
}

createDiscExpBN <- function(modelFile,framFile){
  BN <- from_static_disc_cmpx(modelFile)
  ExpBN <- ExplainableBN$new(BN)
  ExpBN$expframework <- ExplanationFramework$new(BN,framFile)
  return(ExpBN)
}


copyVariableList <- function(inputList){
  outputList <- vector(mode = "list",length = length(inputList))
  for (i in 1:length(inputList)) {
    outputList[[i]] <- inputList[[i]]$copy()
  }
  names(outputList) <- names(inputList) #important to name elements of new copied list as well
  
  return(outputList)
}

createCase <- function(inputBN,observedCase=NULL,clearObservations=FALSE){
  if(clearObservations==FALSE){
    tempList <- copyVariableList(inputBN$modelVariables)
    newBN <- BayesianNetwork$new(tempList)
    
    if(!is.null(observedCase)){
      for (i in 1:length(observedCase)) {
        for (j in 1:length(inputBN$modelVariables)) {
          if(names(observedCase)[i]==inputBN$modelVariables[[j]]$varID)
            newBN$modelVariables[[j]]$setObservation(observedCase[[i]])
        }
      }
    }
  }
  else{
    tempList <- copyVariableList(inputBN$modelVariables)
    newBN <- BayesianNetwork$new(tempList)$clearAllObservations()
    
    if(!is.null(observedCase)){
      for (i in 1:length(observedCase)) {
        for (j in 1:length(inputBN$modelVariables)) {
          if(names(observedCase)[i]==inputBN$modelVariables[[j]]$varID)
            newBN$modelVariables[[j]]$setObservation(observedCase[[i]])
        }
      }
    }
  }
  return(newBN)
}

creategRainBN <- function(inputBN){
  
  variableCPTS <- list()

  for (i in 1:length(inputBN$modelVariables)) {
      variableCPTS[[i]] <- cptable(c(inputBN$modelVariables[[i]]$varID,inputBN$modelVariables[[i]]$getParentIDs()),
                                   values=inputBN$modelVariables[[i]]$varProbs,
                                   levels=inputBN$modelVariables[[i]]$varStates)
  }
  
  modelOutput <- grain(compileCPT(variableCPTS))
  return(modelOutput)
}

calculateBN <- function(inputBN,displayNodes=NULL){
  modelOutput <- creategRainBN(inputBN)
  
  checkObs <- 0
  for (i in 1:length(inputBN$modelVariables)) {
    checkObs <- checkObs+length(inputBN$modelVariables[[i]]$observed)
  }
  
  if(checkObs==0){
    if(is.null(displayNodes))
      calcModel <- querygrain(modelOutput, result="data.frame")
    else
      calcModel <- querygrain(modelOutput, nodes=displayNodes, result="data.frame")
  }
  
  else{
    eviList <- inputBN$getObserved()
    obsList <- inputBN$getObservations()
    eviSet <- setEvidence(modelOutput, nodes=eviList, states=obsList)
    if(is.null(displayNodes))
      calcModel <- querygrain(eviSet, result="data.frame")
    else
      calcModel <- querygrain(eviSet, nodes=displayNodes, result="data.frame")
  }
  
  return(calcModel)
}

getNodeProbabilities <- function(inputExpBN,obsCase,BNnode){
  
  obsBN <- createCase(inputExpBN$bayesnetwork,observedCase = obsCase)
  noObsBN <- inputExpBN$bayesnetwork
  listnames <- c("AllObserved","NoObserved",obsBN$getObserved())
  probsList <- vector(mode="list",length=length(listnames))
  names(probsList) <- listnames
  
  probsList[[1]] <- calculateBN(obsBN,BNnode)[[1]][,2]
  probsList[[2]] <- calculateBN(noObsBN,BNnode)[[1]][,2]
  
  for(i in 1:length(obsBN$getObserved())){
    for (j in 1:length(obsBN$modelVariables)) {
      if(obsBN$modelVariables[[j]]$varID==obsBN$getObserved()[i]){
        tempBN <- createCase(obsBN)
        tempBN$modelVariables[[j]]$clearObservation()
        probsList[[2+i]] <- calculateBN(tempBN,BNnode)[[1]][,2]
      }
    }
  }
  return(probsList)
}

getHellingerDistances <- function(inputExpBN,obsCase,BNnode){
  
  obsBN <- createCase(inputExpBN$bayesnetwork,observedCase = obsCase)
  varProbList <- getNodeProbabilities(inputExpBN,obsCase,BNnode)
  listnames <- c("NoObserved",obsBN$getObserved())
  distanceList <- vector(mode="list",length=length(listnames))
  names(distanceList) <- listnames
  
  for (i in 1:length(obsBN$modelVariables)) {
    if(obsBN$modelVariables[[i]]$varID==BNnode)
      stateNum <- length(obsBN$modelVariables[[i]]$varStates)
  }
  
  for (i in 2:length(varProbList)) {
    tempSum <- 0
    for (j in 1:stateNum) {
      tempSum <- tempSum + (sqrt(varProbList[[1]][j])-sqrt(varProbList[[i]][j]))^2
    }
    distanceList[[i-1]] <- sqrt(tempSum/2)
  }
  return(distanceList)
}

getSignificantEvidence <- function(inputExpBN,obsCase,BNnode){
  
  obsBN <- createCase(inputExpBN$bayesnetwork,observedCase = obsCase)
  nodeProbs <- getNodeProbabilities(inputExpBN,obsCase,BNnode)
  
  sortedTargetHellDist <- sort(unlist(getHellingerDistances(inputExpBN,obsCase,BNnode)[-1]),decreasing = TRUE)
  
  alphaList <- c(0.5,0.45,0.4,0.35, 0.3, 0.25, 0.2,0.15, 0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001 ,0.00005, 0.00001)
  
  for (i in 1:length(obsBN$modelVariables)) {
    if(obsBN$modelVariables[[i]]$varID==BNnode)
      stateNum <- length(obsBN$modelVariables[[i]]$varStates)
  }
  
  for(i in 1:length(alphaList)){
    alpha <- alphaList[i]
    probG <- nodeProbs$AllObserved-alpha*(nodeProbs$AllObserved-nodeProbs$NoObserved)
    
    tempSum <- 0
    for (j in 1:stateNum) {
      tempSum <- tempSum + (sqrt(nodeProbs$AllObserved[j])-sqrt(probG[j]))^2
    }
    theta <- sqrt(tempSum/2)
    sigEvDist <- sortedTargetHellDist[which(sortedTargetHellDist>=theta)]
    if(length(sigEvDist)>=length(sortedTargetHellDist)/2)
      break
  }
  significantEvidences <- names(sigEvDist)
  
  outputList <- list(alpha,theta,significantEvidences)
  return(outputList)
}

conflictAnalysis <- function(inputExpBN,obsCase,BNnode){
  
  obsBN <- createCase(inputExpBN$bayesnetwork,observedCase = obsCase)
  varProbs <- getNodeProbabilities(inputExpBN,obsCase,BNnode)
  hellDistances <- getHellingerDistances(inputExpBN,obsCase,BNnode)
  signifEvi <- getSignificantEvidence(inputExpBN,obsCase,BNnode)[3]
  
  #getting hellinger distances only for significant evi variables (and none observed)
  signifHellDistances <- c(hellDistances[1])
  for (i in 2:length(hellDistances)) {
    if(names(hellDistances)[i] %in% signifEvi[[1]]){
      signifHellDistances <- append(signifHellDistances,hellDistances[i])
    }
  }
  
  #getting state probabilities for significant evi variables with 
  signifVarProbs <- c(varProbs[1],varProbs[2])
  for (i in 3:length(varProbs)) {
    if(names(varProbs)[i] %in% names(signifHellDistances)){
      signifVarProbs <- append(signifVarProbs,varProbs[i])
    }
  }
  
  signifVarProbsPositive <- c(signifVarProbs[1],signifVarProbs[2])
  for (i in 2:length(signifHellDistances)) {
    if(signifHellDistances[[i]]!=0)
      signifVarProbsPositive<-append(signifVarProbsPositive,signifVarProbs[1+i])
  }
  
  for (i in 1:length(obsBN$modelVariables)) {
    if(obsBN$modelVariables[[i]]$varID==BNnode){
      stateNum <- length(obsBN$modelVariables[[i]]$varStates)
      stateNames <- obsBN$modelVariables[[i]]$varStates
    }
  }
  
  conflInfo <- vector(mode="list",length=length(signifVarProbsPositive)-2)
  names(conflInfo) <- names(signifVarProbsPositive)[-(1:2)]
  allObs <- c()
  noneObs <- c()
  eviRemObs <- c()
  for (i in 1:stateNum){
    allObs[i] <- signifVarProbsPositive[[1]][i]
    noneObs[i] <- signifVarProbsPositive[[2]][i]
    for (j in 3:length(signifVarProbsPositive)) {
      eviRemObs[i] <- signifVarProbsPositive[[j]][i]
      if((allObs[i]-eviRemObs[i]>0 && allObs[i]-noneObs[i]>0) || (allObs[i]-eviRemObs[i]<0 && allObs[i]-noneObs[i]<0) || (allObs[i]-eviRemObs[i]==0 && allObs[i]-noneObs[i]==0)){
        conflInfo[[j-2]][i] <- "consistent_state"
      }
      else{
        conflInfo[[j-2]][i] <- "conflicting_state"
      }
    }
  }
  
  consistent_evidence <- c()
  conflicting_evidence <- c()
  mixed_evidence <- c()
  
  mixed_consistent_evidence <- c()
  mixed_conflicting_evidence <- c()
  dominant_evidence <- c()
  
  for (i in 1:length(conflInfo)) {
    
    if(length(unique(conflInfo[[i]]))>1){
      mixed_evidence <- append(mixed_evidence,names(conflInfo)[i])
    }
    else{
      if(unique(conflInfo[[i]])=="consistent_state"){
        consistent_evidence <- append(consistent_evidence,names(conflInfo)[i])
      }
      if(unique(conflInfo[[i]])=="conflicting_state"){
        conflicting_evidence <- append(conflicting_evidence,names(conflInfo)[i])
      }
    }
  }
  
  
  tempCalc <- vector(mode="list",length=length(mixed_evidence))
  cnf_total <- c(length(mixed_evidence))*0
  cst_total <- c(length(mixed_evidence))*0
  
  if(length(mixed_evidence)>0){
    for (i in 1:length(mixed_evidence)) {
      probs <- signifVarProbsPositive[[which(names(signifVarProbsPositive)==mixed_evidence[i])]]
      for (j in 1:stateNum) {
        tempCalc[[i]][j] <- (sqrt(signifVarProbsPositive[[1]][j])-sqrt(probs[j]))^2
      }
    }
    for (i in 1:length(mixed_evidence)) {
      states <- conflInfo[[which(names(conflInfo)==mixed_evidence[i])]]
      for (j in 1:stateNum) {
        if(states[j]=="conflicting_state"){
          cnf_total[i] <- cnf_total[i]+tempCalc[[i]][j]
        }
        if(states[j]=="consistent_state"){
          cst_total[i] <- cst_total[i]+tempCalc[[i]][j]
        }
      }
      cnf_total[i] <- sqrt(cnf_total[i]/2)
      cst_total[i] <- sqrt(cst_total[i]/2)
      if(!is.na(cnf_total[i]) && !is.na(cst_total[i])){
        if(cnf_total[i]>cst_total[i]){
          mixed_conflicting_evidence <- append(mixed_conflicting_evidence,mixed_evidence[i])
        }else{
          mixed_consistent_evidence <- append(mixed_conflicting_evidence,mixed_evidence[i])
        }
      }
    }
  }
  
  
  if(length(consistent_evidence)>0){
    for(i in 1:length(consistent_evidence)){
      eviHell <- signifHellDistances[[which(names(signifHellDistances)==consistent_evidence[i])]]
      if(eviHell>signifHellDistances[[1]])
        dominant_evidence <- append(dominant_evidence,consistent_evidence[i])
    }
  }
  
  
  outputList <- list(dominant_evidence,consistent_evidence,mixed_consistent_evidence,
                     conflicting_evidence,mixed_conflicting_evidence)
  names(outputList) <- c("dominant_evidence","consistent_evidence","mixed_consistent_evidence",
                         "conflicting_evidence","mixed_conflicting_evidence")
  
  return(outputList)  
}



### used to be called explanationAlgorithm

explanationAlgorithm <- function(inputExpBN,obsCase){
  
  target <- inputExpBN$expframework$getTarget()
  exp_results <- conflictAnalysis(inputExpBN,obsCase,target)

  return(exp_results)
}


setDiscretisedState <- function(inputExpBN, obsCase){
  modelVars <- inputExpBN$bayesnetwork$modelVariables
  for (i in seq_along(obsCase)){
    for (j in seq_along(modelVars)){
      if (names(obsCase)[i] == modelVars[[j]]$varID){
        if (modelVars[[j]]$varType == "ContinuousInterval"){
          
          init_val <- as.double(obsCase[[i]])
          match_state <- modelVars[[j]]$varStates[length(modelVars[[j]]$varStates)]
          
          for (st in modelVars[[j]]$varStates[-length(modelVars[[j]]$varStates)]){
            this_limit <- as.double(sub('.* - ', '', st))
            if (init_val <= this_limit){
              match_state <- st
              break
            }
          }
          
          obsCase[[i]] <- match_state
        }
      }
    }
  }
  
  return(obsCase)
}



generateAbstractExplanation <- function(inputExpBN,obsCase,fileName){
  

  obsCase_d <- setDiscretisedState(inputExpBN, obsCase)
  
  target <- inputExpBN$expframework$getTarget()
  alg_res <- explanationAlgorithm(inputExpBN,obsCase_d)
  
  obsBN <- createCase(inputExpBN$bayesnetwork,observedCase=obsCase_d)
  noObsBN <- inputExpBN$bayesnetwork
  
  eviVarNames <- inputExpBN$expframework$getEvidence()
  interVarNames <- inputExpBN$expframework$getIntermediate()
  
  tar_states = c()
  for (vr in inputExpBN$bayesnetwork$modelVariables){
    if (vr$varID == target){
      tar_states = vr$varStates
    }
  }
  
  st_of_int = inputExpBN$expframework$stateOfInt
  
  t_p <- list(calculateBN(noObsBN,displayNodes = target)[[1]][[2]], calculateBN(obsBN,displayNodes = target)[[1]][[2]])
  names(t_p) = c("no_obs", "all_obs")
  
  t <- list(target, tar_states, st_of_int, t_p)
  names(t) = c("id", "states", "state_of_interest", "probabilities")
  
  iv <- vector(mode = "list", length = length(interVarNames))
  for (i in seq_along(interVarNames)){
    this_i_id = interVarNames[i]
    
    this_i_p = list(calculateBN(noObsBN,displayNodes = this_i_id)[[1]][[2]], calculateBN(obsBN,displayNodes = this_i_id)[[1]][[2]])
    names(this_i_p) = c("no_obs", "all_obs")
    
    this_i = list(this_i_id, this_i_p)
    names(this_i) = c("id", "probabilities")
    
    iv[[i]] <- this_i
  }
  
  cons_evi = unique(c(alg_res$dominant_evidence, alg_res$consistent_evidence))
  conf_evi = alg_res$conflicting_evidence
  
  ev <- list(consistent = cons_evi, conflicting = conf_evi)
  
  ob <- vector(mode = "list", length = length(obsCase))
  for (i in seq_along(obsCase)){
    this_ob_id <- names(obsCase)[i]
    this_ob_value <- obsCase[[i]]
    
    this_ob = list(id = this_ob_id, value = this_ob_value)
    ob[[i]] <- this_ob
  }
  
  outList <- list(target = t, intermediate = iv, evidence = ev, observations = ob)
  outList_j <- rjson::toJSON(outList)
  write(outList_j, file = fileName)
  return(outList_j)
  
}




from_cmpx <- function(modelPath){
  agenaModel <- rjson::fromJSON(file = modelPath)
  agenaNodes <- agenaModel$model$networks[[1]]$nodes
  agenaLinks <- agenaModel$model$networks[[1]]$links
  agenaLinks <- rev(agenaLinks)
  
  #creating a list of BNVariables
  tempVarList <- NULL
  namesList <- c()
  
  for (i in 1:length(agenaNodes)) {
    tempVarList[[i]] <- BNVariable(varID=agenaNodes[[i]]$id,
                                   varType=agenaNodes[[i]]$configuration$type)
    namesList[i] <- agenaNodes[[i]]$id
  }
  
  names(tempVarList) <- namesList #naming list elements after varIDs
  
  for (i in 1:length(tempVarList)) {
    if(tempVarList[[i]]$varType != "ContinuousInterval"){
      tempVarList[[i]]$varStates <- agenaNodes[[i]]$configuration$states
      tempVarList[[i]]$varDist <- list("Discrete")
    }
  }
  
  for (i in 1:length(tempVarList)) {
    if(tempVarList[[i]]$varType == "ContinuousInterval"){
      thisExpr <- agenaNodes[[i]]$configuration$table$expressions
      for (k in 1:length(thisExpr)) {
        tempVarList[[i]]$varDist[[k]] <- sub("\\(.*", "", thisExpr[[k]])
        tempVarList[[i]]$varParams[[k]] <- as.numeric(unlist(strsplit(sub("\\).*", "", sub(".*\\(", "", thisExpr[[k]])) ,",")))
      }
    }
  }
  
  #from the most inner parent to outer!! (json has this other way around)
  #if taken from json, it has to be reversed so it matches up with correct state probs in gRain
  
  parentsList <- vector(mode = "list",length = length(tempVarList))
  names(parentsList) <- namesList
  
  for (i in 1:length(tempVarList)) {
    for (j in 1:length(agenaLinks)) {
      if(tempVarList[[i]]$varID==agenaLinks[[j]][[2]]){
        parentsList[[i]] <- append(parentsList[[i]], agenaLinks[[j]][[1]])
      }
    }
  }
  
  for (i in 1:length(parentsList)) {
    if(length(parentsList[[i]])>0){
      for (j in 1:length(parentsList[[i]])) {
        tempVarList[[i]]$addParent_byID(parentsList[[i]][[j]],tempVarList)
      }
    }
  }
  
  agenaCondProbabilities <- vector(mode = "list",length = length(agenaNodes))
  
  for (i in 1:length(agenaNodes)) {
    if(agenaNodes[[i]]$configuration$type != "ContinuousInterval"){
      for (j in 1:length(agenaNodes[[i]]$configuration$table$probabilities[[1]])) {
        for (k in 1:length(agenaNodes[[i]]$configuration$table$probabilities)) {
          agenaCondProbabilities[[i]] <- append(agenaCondProbabilities[[i]],agenaNodes[[i]]$configuration$table$probabilities[[k]][[j]])
        }
      }
    }
  }

  
  for (i in 1:length(tempVarList)) {
    tempVarList[[i]]$addProbability(agenaCondProbabilities[[i]])
  }
  
  for (i in 1:length(tempVarList)) {
    if(tempVarList[[i]]$varType == "ContinuousInterval"){
      tempVarList[[i]]$varStates <- c("False","True")
      prep_probs <-  length(tempVarList[[i]]$varStates)
      for (j in 1:length(tempVarList[[i]]$varParents)) {
        prep_probs <-  prep_probs * length(tempVarList[[i]]$varParents[[j]]$varStates)
      }
      this_probs <- list()
      threshold <- tempVarList[[i]]$varParams[[1]][1]
      state_probs <- list()
      
      for (ix in 1:length(tempVarList[[i]]$varParams)) {
        p_prob = pdnorm(threshold,mean = tempVarList[[i]]$varParams[[ix]][1],sd=sqrt(tempVarList[[i]]$varParams[[ix]][2]))
        state_probs <- append(state_probs,c(1-p_prob,p_prob))
      }
      this_probs <- append(this_probs,state_probs)
      tempVarList[[i]]$varProbs <- this_probs
    }
    
  }
  
  
  BNObject <- BayesianNetwork$new(tempVarList)
  
  return(BNObject)
  
}



detect_markov_blanket <- function(inputBN, BNnode) {
  'A function to detect all variables in the Markov blanket of a node, returns a list of BNNode objects'
  
  node_parents <- BNnode$varParents
  node_children <- c()
  node_peers <- c()

  for (i in seq_along(inputBN$modelVariables)){
    if(BNnode$varID %in% inputBN$modelVariables[[i]]$getParentIDs()){
      node_children <- append(node_children,inputBN$modelVariables[[i]])
    }
  }
  
  for (ch in node_children) {
    node_peers <- append(node_peers, ch$varParents)
  }
  node_peers <- unique(node_peers)
  
  for (i in seq_along(node_peers)) {
    if (node_peers[[i]]$varID == BNnode$varID) {
      node_peers <- node_peers[-i]
      break
    }
  }
  
  node_MB <- c(node_parents,node_children,node_peers)
  return(node_MB)
}

from_static_disc_cmpx <- function(modelPath){
  agenaModel <- rjson::fromJSON(file = modelPath)
  agenaNodes <- agenaModel$model$networks[[1]]$nodes
  agenaLinks <- agenaModel$model$networks[[1]]$links
  agenaLinks <- rev(agenaLinks)
  
  #creating a list of BNVariables
  tempVarList <- NULL
  namesList <- c()
  
  for (i in 1:length(agenaNodes)) {
    tempVarList[[i]] <- BNVariable(varID=agenaNodes[[i]]$id,
                                   varType=agenaNodes[[i]]$configuration$type)
    namesList[i] <- agenaNodes[[i]]$id
  }
  
  names(tempVarList) <- namesList #naming list elements after varIDs
  
  
  for (i in 1:length(tempVarList)) {
    tempVarList[[i]]$varStates <- agenaNodes[[i]]$configuration$states
    tempVarList[[i]]$varDist <- list("Discrete")
  }
  
  #from the most inner parent to outer!! (json has this other way around)
  #if taken from json, it has to be reversed so it matches up with correct state probs in gRain
  
  parentsList <- vector(mode = "list",length = length(tempVarList))
  names(parentsList) <- namesList
  
  for (i in 1:length(tempVarList)) {
    for (j in 1:length(agenaLinks)) {
      if(tempVarList[[i]]$varID==agenaLinks[[j]][[2]]){
        parentsList[[i]] <- append(parentsList[[i]], agenaLinks[[j]][[1]])
      }
    }
  }
  
  for (i in 1:length(parentsList)) {
    if(length(parentsList[[i]])>0){
      for (j in 1:length(parentsList[[i]])) {
        tempVarList[[i]]$addParent_byID(parentsList[[i]][[j]],tempVarList)
      }
    }
  }
  
  agenaCondProbabilities <- vector(mode = "list",length = length(agenaNodes))
  
  for (i in 1:length(agenaNodes)) {
    for (j in 1:length(agenaNodes[[i]]$configuration$table$probabilities[[1]])) {
      for (k in 1:length(agenaNodes[[i]]$configuration$table$probabilities)) {
        agenaCondProbabilities[[i]] <- append(agenaCondProbabilities[[i]],agenaNodes[[i]]$configuration$table$probabilities[[k]][[j]])
      }
    }
  }
  
  for (i in 1:length(tempVarList)) {
    tempVarList[[i]]$addProbability(agenaCondProbabilities[[i]])
  }
  
  BNObject <- BayesianNetwork$new(tempVarList)
  
  return(BNObject)
}

