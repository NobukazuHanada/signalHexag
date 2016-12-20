package com.nobkz.hexagonSingal.signal;

sealed abstract class Signal
case class NoSignal() extends Signal
case class Bang() extends Signal
case class ContainerSignal[T](t:T) extends Signal
