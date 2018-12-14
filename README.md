# Met-CS-810
This repository contains the source code for my Masters Thesis at Boston University Metropolitan College.

## Introduction

Recent advances in Reinforcement Learning have made breakthroughs in teaching AI programs to proficiently play games. However, current RL approaches have steep computational bottlenecks that have only been addressed by leveraging massive computational resources, making them unusable on machines with modest computing capabiltiies. 

This project shall focus on reducing these computational bottlenecks to make it possible to train programs play games on more affordable computing platforms while incrementally increasing the richness and complexity of these games to explore the computational limits of various approaches. I will update this readme as significant developments in our approach and/or new results come in. 

## Requirements 
- scala 2.12.7 (or higher) 
- sbt 0.13.8 (or higher)

## Building and Testing the project 
Navigate to root directory and type: 
> sbt clean compile \
> sbt test
