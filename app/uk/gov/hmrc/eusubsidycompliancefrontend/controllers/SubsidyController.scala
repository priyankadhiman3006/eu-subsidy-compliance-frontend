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

package uk.gov.hmrc.eusubsidycompliancefrontend.controllers

import javax.inject.{Inject, Singleton}
import play.api.data.{Form, Mapping}
import play.api.data.Forms.{bigDecimal, date, mapping, optional, single, text, tuple}
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.eusubsidycompliancefrontend.actions.EscActionBuilders
import uk.gov.hmrc.eusubsidycompliancefrontend.config.AppConfig
import uk.gov.hmrc.eusubsidycompliancefrontend.connectors.EscConnector
import uk.gov.hmrc.eusubsidycompliancefrontend.models.DateFormValues
import uk.gov.hmrc.eusubsidycompliancefrontend.models.types.EORI
import uk.gov.hmrc.eusubsidycompliancefrontend.services.{EligibilityJourney, Store, SubsidyJourney, UndertakingJourney}
import uk.gov.hmrc.eusubsidycompliancefrontend.views.html._

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZoneId}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@Singleton
class SubsidyController @Inject()(
                                   mcc: MessagesControllerComponents,
                                   escActionBuilders: EscActionBuilders,
                                   store: Store,
                                   connector: EscConnector,
                                   reportPaymentPage: ReportPaymentPage,
                                   addClaimEoriPage: AddClaimEoriPage,
                                   addClaimAmountPage: AddClaimAmountPage,
                                   addClaimDatePage: AddClaimDatePage
)(
  implicit val appConfig: AppConfig,
  executionContext: ExecutionContext
) extends
  BaseController(mcc) {

  import escActionBuilders._

  def getReportPayment: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    store.get[SubsidyJourney].flatMap {
      case Some(journey) =>
        journey
          .reportPayment
          .value
          .fold(
            Future.successful(
              Ok(reportPaymentPage()) // TODO populate subsidy list
            )
          ){x =>
            Future.successful(
              Ok(reportPaymentPage())
            )
          }
      case None => // initialise the empty Journey model
        store.put(SubsidyJourney()).map { _ =>
          Ok(reportPaymentPage())
        }
    }
  }

  def postReportPayment: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    reportPaymentForm.bindFromRequest().fold(
      _ => throw new IllegalStateException("value hard-coded, form hacking?"),
      form => {
        store.update[SubsidyJourney]({ x =>
          x.map { y =>
            y.copy(reportPayment = y.reportPayment.copy(value = Some(form.value.toBoolean)))
          }
        }).flatMap(_.next)
      }
    )
  }

  def getClaimAmount: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    store.get[SubsidyJourney].flatMap {
      case Some(journey) =>
        journey
          .claimAmount
          .value
          .fold(
            Future.successful(
              Ok(addClaimAmountPage(claimAmountForm))
            )
          ) { x =>
            Future.successful(
              Ok(addClaimAmountPage(claimAmountForm.fill(x)))
            )
          }
    }
  }

  def postAddClaimAmount: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    claimAmountForm.bindFromRequest().fold(
      _ => throw new IllegalStateException("value hard-coded, form hacking?"),
      form => {
        store.update[SubsidyJourney]({ x =>
          x.map { y =>
            y.copy(claimAmount = y.claimAmount.copy(value = Some(form)))
          }
        }).flatMap(_.next)
      }
    )
  }

  def getClaimDate: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    store.get[SubsidyJourney].flatMap {
      case Some(journey) =>
        journey
          .claimDate
          .value
          .fold(
            Future.successful(
              Ok(addClaimDatePage(claimDateForm))
            )
          ) { x =>
            Future.successful(
              Ok(addClaimDatePage(claimDateForm.fill(x)))
            )
          }
    }
  }

  def postClaimDate: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    claimDateForm.bindFromRequest().fold(
      formWithErrors => Future(BadRequest(addClaimDatePage(formWithErrors))),
      form => {
        store.update[SubsidyJourney]({ x =>
          x.map { y =>
            y.copy(claimDate = y.claimDate.copy(value = Some(form)))
          }
        }).flatMap(_.next)
      }
    )
  }

  def getAddClaimEori: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    store.get[SubsidyJourney].flatMap {
      case Some(journey) =>
        journey
          .addClaimEori
          .value
          .fold(
            Future.successful(
              Ok(addClaimEoriPage(claimEoriForm))
            )
          ) { x =>
            val a = x.fold("false")(_ => "true")
            Future.successful(
              Ok(addClaimEoriPage(claimEoriForm.fill(OptionalEORI(a,x))))
            )
          }
    }
  }

  def postAddClaimEori: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    getPrevious[SubsidyJourney](store).flatMap { previous =>
      claimEoriForm.bindFromRequest().fold(
        errors => Future.successful(BadRequest(addClaimEoriPage(errors))),
        form => {
          store.update[SubsidyJourney]({ x =>
            x.map { y =>
              y.copy(addClaimEori = y.addClaimEori.copy(value = Some(form.value.map(EORI(_)))))
            }
          }).flatMap(_.next)
        }
      )
    }
  }

  lazy val reportPaymentForm: Form[FormValues] = Form(
    mapping("reportPayment" -> mandatory("reportPayment"))(FormValues.apply)(FormValues.unapply))

  // TODO validate the EORI matches regex
  val claimEoriForm: Form[OptionalEORI] = Form(
    mapping(
      "should-claim-eori" -> mandatory("should-claim-eori"),
      "claim-eori" -> optional(text)
    )(OptionalEORI.apply)(OptionalEORI.unapply).transform[OptionalEORI](
      a => if (a.setValue == "false") a.copy(value = None) else a,
      b => b
    )
  )

  lazy val claimAmountForm : Form[BigDecimal] = Form( mapping("claim-amount" -> bigDecimal)(identity)(Some(_)))

  lazy val claimDateForm : Form[DateFormValues] = Form(
    DateFormValues.vatRegDateMapping
      .verifying("error.date.invalid", a =>  a.isValidDate)
  )

}
