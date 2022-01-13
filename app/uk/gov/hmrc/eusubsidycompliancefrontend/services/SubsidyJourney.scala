/*
 * Copyright 2022 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.eusubsidycompliancefrontend.services

import java.time.LocalDate

import play.api.libs.json._
import shapeless.syntax.std.tuple._
import shapeless.syntax.typeable._
import uk.gov.hmrc.eusubsidycompliancefrontend.models.types.{EORI, TraderRef}

case class SubsidyJourney(
  reportPayment: FormPage[Boolean] = FormPage("claims"),
//  claimDate: FormPage[LocalDate] = FormPage("add-claim-date"),
//  claimAmount: FormPage[BigDecimal] = FormPage("add-claim-amount"),
  addClaimEori: FormPage[Option[EORI]] = FormPage("add-claim-eori"),
//  publicAuthority: FormPage[String] = FormPage("add-claim-public-authority"),
//  traderRef: FormPage[Option[TraderRef]] = FormPage("add-claim-reference"),
//  cya: FormPage[Boolean] = FormPage("check-your-answers")
) extends Journey {

  override def steps: List[Option[FormPage[_]]] =
    SubsidyJourney.
      unapply(this)
      .map(_.toList)
      .fold(List.empty[Any])(identity)
      .map(_.cast[FormPage[_]])


}

object SubsidyJourney {
  import Journey._ // N.B. don't let intellij delete this
  import play.api.libs.functional.syntax._

  implicit val formPageClaimDateFormat: OFormat[FormPage[LocalDate]] =
    Json.format[FormPage[LocalDate]]

  implicit val formPageAddClaimEoriFormat: Format[FormPage[Option[EORI]]] = new Format[FormPage[Option[EORI]]] {

    override def writes(o: FormPage[Option[EORI]]): JsValue = {
      val eoriOpt: JsValue = o.value.flatten match {
        case Some(eori) => JsString(eori)
        case _ => JsNull
      }
      Json.obj(
        "claimEori" -> eoriOpt
      )
    }

    override def reads(json: JsValue): JsResult[FormPage[Option[EORI]]] = {
      val foo: Option[EORI] = (json \ "claimEori").asOpt[EORI]
      val bar = FormPage[Option[EORI]]("add-claim-eori", Some(foo))
      JsSuccess(bar)
    }
  }

  implicit val formPageTraderRefFormat: OFormat[FormPage[TraderRef]] =
    Json.format[FormPage[TraderRef]]

  implicit val format: Format[SubsidyJourney] = Json.format[SubsidyJourney]
}
