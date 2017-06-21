package edu.ucla.cs.starai.util

import java.util.concurrent.Callable

object Conversions {

	implicit def runnable(f: () => Unit): Runnable =
			new Runnable() { def run() = f() }

	implicit def callable[T](f: () => T): Callable[T] =
			new Callable[T]() { def call() = f() }

}