package livechart

import com.raquo.laminar.api.L._
import org.scalajs.dom.document
import scala.util.Random

final class DataItemID

case class DataItem(id: DataItemID, label: String, price: Double, count: Int):
  def fullPrice: Double = price * count

object DataItem:
  def apply(): DataItem =
    DataItem(DataItemID(), "?", Random.nextDouble(), Random.nextInt(5) + 1)
end DataItem

type DataList = List[DataItem]

//Since we want our chart to be editable, we will need to change the table data over time. For that purpose, we put the entire DataList in a Var, which we encapsulate in a Model class, as follows:
final class Model:
  val dataVar: Var[DataList] = Var(List(DataItem(DataItemID(), "One", 1.0, 1)))
  val dataSignal = dataVar.signal

  //we also define two functions that will add a new random item, and remove a specific item (given its ID):
  def addDataItem(item: DataItem): Unit =
    dataVar.update(data => data :+ item)

  def removeDataItem(id: DataItemID): Unit =
    dataVar.update(data => data.filter(_.id != id))
end Model


object Main:
  val model = new Model
  import model.*

  def appElement(): Element =
    div(
      h1("Live Chart"),
      renderDataTable(),
    )
  end appElement

  def renderDataTable(): Element =
    table(
      thead(tr(th("label", th("Price"), th("Count"), th("Full Price"), th("Action"))),
        tbody(
          // here what is happening is that the table is creating a new list everytime an item is added or deleted and its not creating the items in real time which is okay for text data but to overcome this performance issue we have to change the dataItem using split. lets comment the below line and add a new line
//          children <-- dataSignal.map(data => data.map { item =>
//            renderDataItem(item.id, item)
//          }),
            children <-- dataSignal.split(_.id) { (id, initial, itemSignal) =>
              renderDataItem(id, itemSignal)
            }
        ),
        tfoot(tr(
          td(button("➕", onClick --> (_ => addDataItem(DataItem())))),
          td(),
          td(),
          td(child.text <-- dataSignal.map(data => "%.2f".format(data.map(_.fullPrice).sum))),
        )),
      )
    )
  end renderDataTable

//  The id is the only field taken into account to reuse an output element. Therefore, if a future input contains a different DataItem with the same id, it will return the same output element. That DataItem will then become the new value of the time-varying itemSignal. This is why we now pass itemSignal to renderDataItem, which we amend to accept a Signal[DataItem]:
  def renderDataItem(id: DataItemID, itemSignal: Signal[DataItem]): Element =
    tr(
//      td(item.label),
//      td(item.price),
//      td(item.count),
//      td("%.2f".format(item.fullPrice)),
      td(
        child.text <-- itemSignal.map(item => "%.2f".format(item.fullPrice))
      ),
      td(button("🗑️", onClick --> (_ => removeDataItem(id)))),
    )
  end renderDataItem

end Main










