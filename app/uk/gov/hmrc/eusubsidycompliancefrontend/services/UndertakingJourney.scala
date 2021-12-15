/*
 * Copyright 2021 HM Revenue & Customs
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

import play.api.libs.json.{Format, Json, OFormat}
import shapeless.syntax.std.tuple._
import shapeless.syntax.typeable._
import uk.gov.hmrc.eusubsidycompliancefrontend.models.ContactDetails
import uk.gov.hmrc.eusubsidycompliancefrontend.models.types.Sector.Sector

case class UndertakingJourney(
  name: FormPage[String] = FormPage("undertaking-name"),
  sector: FormPage[Sector] = FormPage("sector"),
  contact: FormPage[ContactDetails] = FormPage("contact"),
  cya: FormPage[Boolean] = FormPage("check-your-answers")
) extends Journey {

  override def steps: List[Option[FormPage[_]]] =
    UndertakingJourney.
      unapply(this)
      .map(_.toList)
      .fold(List.empty[Any])(identity)
      .map(_.cast[FormPage[_]])

}

object UndertakingJourney {

  import Journey._
  import uk.gov.hmrc.eusubsidycompliancefrontend.models.types.Sector.format
  implicit val formPageSectorFormat: OFormat[FormPage[Sector]] =
    Json.format[FormPage[Sector]]
  implicit val formatContactDetails: OFormat[ContactDetails] =
    Json.format[ContactDetails]
  implicit val formPageContactFormat: OFormat[FormPage[ContactDetails]] =
    Json.format[FormPage[ContactDetails]]

  implicit val format: Format[UndertakingJourney] = Json.format[UndertakingJourney]
}