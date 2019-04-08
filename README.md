# Met-CS-810
This repository contains the source code for my Masters Thesis at Boston University Metropolitan College.

## Introduction

Recent advances in Machine Learning have demonstrated the feasibility of training models to play games with high proficiency and have yielded significant breakthroughs in the field of autonomy and decision management. However, it’s no secret that in cases where data is scares, many ML techniques are simply impractical for producing meaningful results. To address this hurdle, researchers have relied on increasingly complex simulators of game playing environments. This is especially true for DeepMind’s AlphaGo, and most recently AlphaStar. While these results are impressive, these simulation environments are seldom representative of real-world environments, where data is often noisy, and world states are only partially observable. As a result, ML techniques that work well in simulation (i.e. Reinforcement Learning) do not always translate well into the real world, and in some cases has yielded disastrous results, for example the recent fatal crash of an Uber self driving car.

To enable safer and smoother transitions from a simulation to a real-world environment without sacrificing generality, MET-CS-811 provides a simulation and learning framework that (1) enables Subject Matter Experts (SME) to encode their knowledge about the real world without requiring any ML background; (2) automatically constructs a simulator based on the encoded SME knowledge; (3) contains a library of learning frameworks and models to plug and play against the constructed simulator; (4) provides a modular modeling and analysis API for building custom models based on the technical requirements of their domain.

NOTE: This framework currently only supports simulators with finite action spaces. Will eventually extend to infinite and continuous spaces 

## Requirements 
- scala 2.12.7 (or higher) 
- sbt 0.13.8 (or higher)

## Building and Testing the project 
Navigate to root directory and type: 
> sbt clean compile \
> sbt test

## Creating your own Simulator 
- Step1: Define your Environment 
- Step2: Define your Agent 
- Step3: Define your Action Space 
- Step4: Define your transition function 




