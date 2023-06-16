package machine

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object CoffeeMachine extends App {
  private val machine: CoffeeMachine = new CoffeeMachine(400, 540, 120, 9, 550)
  private var state: Boolean = true

  while (state) state = machine.mainMenuInput()
}

class CoffeeMachine(water: Int, milk: Int, beans: Int, cups: Int, money: Int) {
  private val waterCountPerEspresso: Int = 250
  private val beansCountPerEspresso: Int = 16
  private val moneyCountPerEspresso: Int = 4
  private val waterCountPerLatte: Int = 350
  private val milkCountPerLatte: Int = 75
  private val beansCountPerLatte: Int = 20
  private val moneyCountPerLatte: Int = 7
  private val waterCountPerCappuccino: Int = 200
  private val milkCountPerCappuccino: Int = 100
  private val beansCountPerCappuccino: Int = 12
  private val moneyCountPerCappuccino: Int = 6
  private var waterCount: Int = water
  private var milkCount: Int = milk
  private var beansCount: Int = beans
  private var cupsCount: Int = cups
  private var moneyCount: Int = money

  @tailrec
  private def mainMenuInput(input: String = ""): Boolean = {
    input match {
      case "buy" => buyMenuInput()
      case "fill" => fillMachine()
      case "take" => takeMoney()
      case "remaining" => printMachineState()
      case "exit" => false
      case _ => mainMenuInput(askForInput("Write action (buy, fill, take, remaining, exit): "))
    }
  }

  @tailrec
  private def buyMenuInput(input: String = ""): Boolean = {
    input match {
      case "1" => buyCoffee(1)
      case "2" => buyCoffee(2)
      case "3" => buyCoffee(3)
      case "back" => mainMenuInput()
      case _ => buyMenuInput(askForInput("What do you want to buy? 1 - espresso, 2 - latte, 3 - cappuccino, back - to main menu: "))
    }
  }

  private def askForInput(question: String): String = {
    print(question)
    readLine()
  }

  private def buyCoffee(coffeeType: Int): Boolean = {
    coffeeType match {
      case 1 => prepareCoffee(waterCountPerEspresso, 0, beansCountPerEspresso, moneyCountPerEspresso)
      case 2 => prepareCoffee(waterCountPerLatte, milkCountPerLatte, beansCountPerLatte, moneyCountPerLatte)
      case 3 => prepareCoffee(waterCountPerCappuccino, milkCountPerCappuccino, beansCountPerCappuccino, moneyCountPerCappuccino)
    }
    true
  }

  private def prepareCoffee(waterRequired: Int, milkRequired: Int, beansRequired: Int, cost: Int): Unit = {
    if (waterRequired > waterCount) println("Sorry, not enough water!")
    else if (milkRequired > milkCount) println("Sorry, not enough milk!")
    else if (beansRequired > beansCount) println("Sorry, not enough coffee beans!")
    else if (1 > cupsCount) println("Sorry, not enough cups!")
    else {
      println("I have enough resources, making you a coffee!")
      waterCount -= waterRequired
      milkCount -= milkRequired
      beansCount -= beansRequired
      cupsCount -= 1
      moneyCount += cost
    }
  }

  private def fillMachine(): Boolean = {
    waterCount += askForInput("Write how many ml of water you want to add: ").toInt
    milkCount += askForInput("Write how many ml of milk you want to add: ").toInt
    beansCount += askForInput("Write how many grams of coffee beans you want to add: ").toInt
    cupsCount += askForInput("Write how many disposable cups you want to add: ").toInt
    true
  }

  private def takeMoney(): Boolean = {
    println(s"I gave you $$$moneyCount")
    moneyCount = 0
    true
  }

  private def printMachineState(): Boolean = {
    println("The coffee machine has:")
    println(s"$waterCount ml of water")
    println(s"$milkCount ml of milk")
    println(s"$beansCount g of coffee beans")
    println(s"$cupsCount disposable cups")
    println(s"$$$moneyCount of money")
    true
  }
}