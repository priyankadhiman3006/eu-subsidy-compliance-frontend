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
import uk.gov.hmrc.eusubsidycompliancefrontend.models.types.{EORI, TraderRef}
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
                                   addClaimDatePage: AddClaimDatePage,
                                   addPublicAuthorityPage: AddPublicAuthorityPage,
                                   addTraderReferencePage: AddTraderReferencePage,
                                   cyaPage: ClaimCheckYourAnswerPage
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
      formWithErrors => Future.successful(BadRequest(addClaimAmountPage(formWithErrors))),
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
        formWithErrors => Future.successful(BadRequest(addClaimEoriPage(formWithErrors))),
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

  def getAddClaimPublicAuthority: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    store.get[SubsidyJourney].flatMap {
      case Some(journey) =>
        journey
          .publicAuthority
          .value
          .fold(
            Future.successful(
              Ok(addPublicAuthorityPage(claimPublicAuthorityForm))
            )
          ) { x =>
            Future.successful(
              Ok(addPublicAuthorityPage(claimPublicAuthorityForm.fill(x)))
            )
          }
    }
  }

  def postAddClaimPublicAuthority: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    getPrevious[SubsidyJourney](store).flatMap { previous =>
      claimPublicAuthorityForm.bindFromRequest().fold(
        errors => Future.successful(BadRequest(addPublicAuthorityPage(errors))),
        form => {
          store.update[SubsidyJourney]({ x =>
            x.map { y =>
              y.copy(publicAuthority = y.publicAuthority.copy(value = Some(form)))
            }
          }).flatMap(_.next)
        }
      )
    }
  }

  def getAddClaimReference: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    store.get[SubsidyJourney].flatMap {
      case Some(journey) =>
        journey
          .traderRef
          .value
          .fold(
            Future.successful(
              Ok(addTraderReferencePage(claimTraderRefForm))
            )
          ) { x =>
            val a = x.fold("false")(_ => "true")
            Future.successful(
              Ok(addTraderReferencePage(claimTraderRefForm.fill(OptionalTraderRef(a,x))))
            )
          }
    }
  }

  def postAddClaimReference: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    getPrevious[SubsidyJourney](store).flatMap { previous =>
      claimTraderRefForm.bindFromRequest().fold(
        errors => Future.successful(BadRequest(addTraderReferencePage(errors))),
        form => {
          store.update[SubsidyJourney]({ x =>
            x.map { y =>
              y.copy(traderRef = y.traderRef.copy(value = Some(form.value.map(TraderRef(_)))))
            }
          }).flatMap(_.next)
        }
      )
    }
  }

  def getCheckAnswers: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    store.get[SubsidyJourney].flatMap {
      case Some(journey) =>
        Future.successful(
          Ok(
            cyaPage(
              journey.claimDate.value.getOrElse(throw new IllegalStateException("Claim date should be defined")),
              journey.claimAmount.value.getOrElse(throw new IllegalStateException("Claim amount payment should be defined")),
              journey.addClaimEori.value.getOrElse(throw new IllegalStateException("Claim EORI payment should be defined")),
              journey.publicAuthority.value.getOrElse(throw new IllegalStateException("Public Authority payment should be defined")),
              journey.traderRef.value.getOrElse(throw new IllegalStateException("Trader Reference payment should be defined"))
            )
          )
        )
    }
  }
  def postCheckAnswers: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    cyaForm.bindFromRequest().fold(
      _ => throw new IllegalStateException("value hard-coded, form hacking?"),
      form => {
        store.update[SubsidyJourney]({ x =>
          x.map { y =>
            y.copy(cya = y.cya.copy(value = Some(form.value.toBoolean)))
          }
        })
          .flatMap { journey: SubsidyJourney =>
            for {
              a <- Future.successful(4)//stub for connector stuff
            } yield {
              Ok("Dun")
            }
          }
      }
    )
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

  val claimTraderRefForm: Form[OptionalTraderRef] = Form(
    mapping(
      "should-store-trader-ref" -> mandatory("should-claim-eori"),
      "claim-trader-ref" -> optional(text)
    )(OptionalTraderRef.apply)(OptionalTraderRef.unapply).transform[OptionalTraderRef](
      a => if (a.setValue == "false") a.copy(value = None) else a,
      b => b
    )
  )

  lazy val claimPublicAuthorityForm: Form[String] = Form(
    "claim-public-authority" -> mandatory("claim-public-authority")
  )

  lazy val claimAmountForm : Form[BigDecimal] = Form( mapping("claim-amount" -> bigDecimal)(identity)(Some(_)))

  lazy val claimDateForm : Form[DateFormValues] = Form(
    DateFormValues.vatRegDateMapping
      .verifying("error.date.invalid", a =>  a.isValidDate)
  )

  lazy val cyaForm: Form[FormValues] = Form(
    mapping("cya" -> mandatory("cya"))(FormValues.apply)(FormValues.unapply))

}