package org.bu.met810.model

import breeze.linalg.{DenseVector, max}
import neuroflow.application.plugin.Extensions._
import neuroflow.application.plugin.IO._
import neuroflow.application.plugin.Notation._
import neuroflow.application.processor.Image._
import neuroflow.common.~>
import neuroflow.core.Activators.Float._
import neuroflow.dsl.Convolution.autoTupler
import neuroflow.core._
import neuroflow.dsl._
import neuroflow.nets.gpu.ConvNetwork._

import scala.collection.mutable


class ConvolutionExperimental{

    //    val path = "/Users/felix/github/unversioned/cifar"
    val path = "/home/felix"
    val wps: String = path + "/waypoint"
    val lfo: String = path + "/lfo.txt"

    val classes = Seq("airplane", "automobile", "bird", "cat", "deer", "dog", "frog", "horse", "ship", "truck")
    val classVecs: Map[String, DenseVector[Float]] = classes.zipWithIndex.map { case (c, i) => c -> ~>(ζ[Float](classes.size)).io(_.update(i, 1.0f)).t }.toMap

    println("Loading data ...")

    val limits: (Int, Int) = (50000, 10000)

    val train: mutable.ArraySeq[(TensorRGB[Float], DenseVector[Float])] = new java.io.File(path + "/train").list().take(limits._1).par.map { s =>
      val c = classes.find(z => s.contains(z)).get
      loadTensorRGB(path + "/train/" + s).float -> classVecs(c)
    }.seq

    val test: mutable.ArraySeq[(TensorRGB[Float], DenseVector[Float])] =
      new java.io.File(path + "/test").list().take(limits._2).par.map { s =>
      val c = classes.find(z => s.contains(z)).get
      loadTensorRGB(path + "/test/" + s).float -> classVecs(c)
    }.seq

    classes.foreach { c =>
      println(s"|$c| = " + train.count(l => l._2 == classVecs(c)))
    }

    val f: Activator.ReLU[Float] = ReLU

    val c0 = Convolution(dimIn = (32, 32, 3),  padding = 1, field = 3, stride = 1, filters = 128, activator = f)
    val c1 = Convolution(dimIn = c0.dimOut,    padding = 1, field = 3, stride = 1, filters = 128, activator = f)
    val c2 = Convolution(dimIn = c1.dimOut,    padding = 1, field = 4, stride = 2, filters = 128, activator = f)
    val c3 = Convolution(dimIn = c2.dimOut,    padding = 1, field = 3, stride = 1, filters = 128, activator = f)
    val c4 = Convolution(dimIn = c3.dimOut,    padding = 1, field = 3, stride = 1, filters = 128, activator = f)
    val c5 = Convolution(dimIn = c4.dimOut,    padding = 1, field = 4, stride = 2, filters = 128, activator = f)
    val c6 = Convolution(dimIn = c5.dimOut,    padding = 1, field = 3, stride = 1, filters = 128, activator = f)
    val c7 = Convolution(dimIn = c6.dimOut,    padding = 0, field = 1, stride = 1, filters = 128, activator = f)
    val c8 = Convolution(dimIn = c7.dimOut,    padding = 0, field = 1, stride = 1, filters =  10, activator = f)

    val μ = 0.0

    implicit val weights: WeightBreeder[Float] = WeightBreeder[Float].normal(Map(
      0 -> (μ, 0.1),  1 -> (μ, 0.1), 2 -> (μ, 0.1),
      3 -> (μ, 0.01), 4 -> (μ, 0.1), 5 -> (μ, 0.01),
      6 -> (μ, 0.01), 7 -> (μ, 0.1), 8 -> (μ, 1.0),
      9 -> (0.01, 0.01)
    ))

    //    implicit val weights = neuroflow.application.plugin.IO.File.weightBreeder[Float](wps + "-iter-1000.nf")

    val net = Network(
      layout = c0 :: c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: Dense(10, f) :: SoftmaxLogEntropy(),
      Settings[Float](
        prettyPrint     = true,
        learningRate    = {
          case (i, _) if i < 4000 => 1E-5
          case (_, _)             => 1E-6
        },
        updateRule      = Momentum(μ = 0.8f),
        iterations      = Int.MaxValue,
        precision       = 1E-2,
        batchSize       = Some(250),
        gcThreshold     = Some(1024 * 1024 * 1024L /* 1G */),
        lossFuncOutput  = Some(LossFuncOutput(Some(lfo))),
        waypoint        = Some(Waypoint(nth = 1000, (iter, ws) => File.writeWeights(ws, wps + s"-iter-$iter.nf")))
      )
    )

    net.train(train.map(_._1), train.map(_._2))

    def eval(source: Seq[(Tensor3D[Float], DenseVector[Float])], s: String): Unit = {
      var i = 1
      val rate = source.map {
        case (x, y) =>
          val v = net(x)
          val c = v.toArray.indexOf(max(v))
          val t = y.toArray.indexOf(max(y))
          println(s"Step $i.")
          i += 1
          if (c == t) 1.0 else 0.0
      }.sum / source.size.toDouble
      println(s"$s: Recognition rate = ${rate * 100.0} %, Error rate = ${(1.0 - rate) * 100.0} %!")
    }

    eval(train, "train")
    eval(test, "test")
    eval(train ++ test, "train ++ test")

    val posWeights: Int = net.weights.foldLeft(0)((count, m) => count + m.findAll(_ > 0.0).size)
    val negWeights: Int = net.weights.foldLeft(0)((count, m) => count + m.findAll(_ < 0.0).size)

    println(s"|Weights| > 0 = $posWeights, |Weights| < 0 = $negWeights")

}
