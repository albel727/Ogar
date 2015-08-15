package com.agariomods.ogar.modules

import com.agariomods.ogar.GameConfig
import grizzled.config.{Configuration, ValueConverter}

import scala.util.{Failure, Success}

object IniReflect {
  import grizzled.config.Configuration.Implicits._
  import scala.reflect.runtime.universe._

  implicit object FloatConverter extends ValueConverter[Float] {
    def convert(sectionName: String,
                optionName: String,
                value: String): Either[String, Float] = {
      scala.util.Try { java.lang.Float.parseFloat(value) } match {
        case Failure(e) => Left(s"Section '$sectionName', option '$optionName': '$value' is not a float.")
        case Success(i) => Right(i)
      }
    }
  }

  implicit object DoubleConverter extends ValueConverter[Double] {
    def convert(sectionName: String,
                optionName: String,
                value: String): Either[String, Double] = {
      scala.util.Try { java.lang.Double.parseDouble(value) } match {
        case Failure(e) => Left(s"Section '$sectionName', option '$optionName': '$value' is not a double.")
        case Success(i) => Right(i)
      }
    }
  }

  def listConfigKeys(gameConfig: GameConfig) = {
    getAccessors(gameConfig).map(_.name.toString)
  }

  def setFromConfiguration(gameConfig : GameConfig, configuration : Configuration, sectionName : String) : Iterable[(String, Either[String, Either[Any, Any]])] = {
    val accessors = getAccessors(gameConfig)

    val instanceMirror = scala.reflect.runtime.currentMirror.reflect(gameConfig)

    accessors.map(acc => {
      val optionName = acc.name.toString

      val setOption = (v : Either[String, Option[Any]]) => {
        v match {
          case Left(error) =>
            // Value was present in the config but failed to parse.
            Left(error)
          case Right(None) =>
            // Value not present in the config, returning current.
            val current = instanceMirror.reflectMethod(acc).apply()
            Right(Left(current))
          case Right(Some(theValue)) =>
            // Value present in the config, setting it.
            instanceMirror.reflectMethod(acc.setter.asMethod).apply(theValue)
            Right(Right(theValue))
        }
      }

      val v = new TpeWrapper(acc.returnType) match {
        case `intTpe` =>
          configuration.asEither[Int](sectionName, optionName)
        case `longTpe` =>
          configuration.asEither[Long](sectionName, optionName)
        case `floatTpe` =>
          configuration.asEither[Float](sectionName, optionName)
        case `doubleTpe` =>
          configuration.asEither[Double](sectionName, optionName)
        case `stringTpe` =>
          configuration.asEither[String](sectionName, optionName)
        case t =>
          Left(s"Possible bug! Unhandled property type: $t")
      }

      (optionName, setOption(v))
    })
  }

  def setFromKeyValue(gameConfig : GameConfig, optionName : String, value : String, sectionName : String) = {
    val rm = scala.reflect.runtime.currentMirror

    val accessors = rm.classSymbol(gameConfig.getClass).toType.member(TermName(optionName)) match {
      case m: MethodSymbol if m.isGetter && m.isPublic => Right(m)
      case _ => Left(s"Key $optionName doesn't exist")
    }
    val instanceMirror = rm.reflect(gameConfig)

    accessors.right.flatMap(acc => {
      val setOption = (v : Either[String, Any]) => {
        v.right.foreach(v => instanceMirror.reflectMethod(acc.setter.asMethod).apply(v))
        v
      }

      val v = new TpeWrapper(acc.returnType) match {
        case `intTpe` =>
          implicitly[ValueConverter[Int]].convert(sectionName, optionName, value)
        case `longTpe` =>
          implicitly[ValueConverter[Long]].convert(sectionName, optionName, value)
        case `floatTpe` =>
          implicitly[ValueConverter[Float]].convert(sectionName, optionName, value)
        case `doubleTpe` =>
          implicitly[ValueConverter[Double]].convert(sectionName, optionName, value)
        case `stringTpe` =>
          implicitly[ValueConverter[String]].convert(sectionName, optionName, value)
        case t =>
          Left(s"Possible bug! Unhandled property type: $t")
      }

      setOption(v)
    })
  }

  private def getAccessors(gameConfig : GameConfig) = {
    val rm = scala.reflect.runtime.currentMirror

    val accessors = rm.classSymbol(gameConfig.getClass).toType.members.collect {
      case m : MethodSymbol if m.isGetter && m.isPublic => m
    }

    accessors
  }

  private final class TpeWrapper(val tpe : Type) {
    override def equals(obj : scala.Any) = {
      obj match {
        case v : TpeWrapper => tpe =:= v.tpe
        case value : Type => tpe =:= value
        case _ => false
      }
    }

    override def hashCode() = 0
  }

  private val intTpe = new TpeWrapper(scala.reflect.runtime.universe.definitions.IntTpe)
  private val longTpe = new TpeWrapper(scala.reflect.runtime.universe.definitions.LongTpe)
  private val floatTpe = new TpeWrapper(scala.reflect.runtime.universe.definitions.FloatTpe)
  private val doubleTpe = new TpeWrapper(scala.reflect.runtime.universe.definitions.DoubleTpe)
  private val stringTpe = new TpeWrapper(scala.reflect.runtime.universe.definitions.StringClass.selfType)
}
