# SIMMBA (SIMulation and Model Building for Adversaries)

## Introduction
Adversaries in the real world come in many forms, and yield many kinds of threats. Cyber Attacks that breach of sensitive databases lead to fraud as well as crucial information being exposed to foreign intelligence. Terrorist attacks on vulnerable civilian locations instill fear into populations and cause mass destruction and loss of human life. Criminal and civil offenses not only pose a threat to civilians but also puts pressure on our justice system. While modern Machine Learning techniques have the potential to predict and prevent these threats from realization, their dependence on representative adversarial data severely limits their performance. This is especially true as adversarial data is seldom available and frequently outdated, as their actions and movement patterns are constantly changing. As result, security and military personnel have relied increasingly on Subject Matter Expertise (SME) over purely data driven approaches. SME (i.e. knowledge driven) models however, are often monolithic and difficult to manage, especially in the face of an ever-changing adversary. Furthermore, the SME having to work with an ML expert to encode this knowledge only adds to the overhead. However, recent advances using simulated data has bridged the game between knowledge based and data driven solutions. Indeed, a domain expert can encode their knowledge of an adversarial scenario into a simulator, which then produces realistic and high-quality data about the scenario to train a data driven model. While these approaches have largely been limited to playing competitive games, they nevertheless yield immediate applications towards real world adversarial environments.  In this work, we first present a flexible framework for building and using simulations to train a range of models to play games competitively. Then we discuss how an SME in cybersecurity, counter-terrorism, or some other adversary-oriented field could use this framework to build simulations and train models with minimal knowledge of Machine Learning. Finally, we show how our approach avoids some of the pitfalls of simulation-based approaches that have beset our predecessors.

NOTE: This framework currently only supports simulators with finite action spaces. Will eventually extend to infinite and continuous action spaces 

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




