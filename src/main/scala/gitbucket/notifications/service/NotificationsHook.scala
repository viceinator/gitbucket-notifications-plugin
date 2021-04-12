package gitbucket.notifications.service

import gitbucket.core.controller.Context
import gitbucket.core.model.{Account, Issue}
import gitbucket.core.service._
import RepositoryService.RepositoryInfo
import gitbucket.core
import gitbucket.core.model
import gitbucket.core.model.Profile.Priorities
import gitbucket.core.util.Directory._
import gitbucket.core.util.JGitUtil.{BranchInfo, getBranchesOfCommit}
import gitbucket.core.util.{JGitUtil, LDAPUtil, Mailer}
import gitbucket.core.view.Markdown
import gitbucket.notifications.model.Profile._
import org.eclipse.jgit.transport.{ReceiveCommand, ReceivePack}
import org.slf4j.LoggerFactory
import profile.blockingApi._
import org.eclipse.jgit.api.Git

import scala.util.Using
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success}

class AccountHook extends gitbucket.core.plugin.AccountHook
{
  override def deleted(userName: String)(implicit session: Session): Unit = {
    IssueNotifications.filter(_.notificationUserName === userName.bind).delete
    Watches.filter(_.notificationUserName === userName.bind).delete
  }
}

class RepositoryHook extends gitbucket.core.plugin.RepositoryHook
{
  override def deleted(owner: String, repository: String)(implicit session: Session): Unit =  {
    IssueNotifications.filter(t => t.userName === owner.bind && t.repositoryName === repository.bind).delete
    Watches.filter(t => t.userName === owner.bind && t.repositoryName === repository.bind).delete
  }

  override def renamed(owner: String, repository: String, newRepository: String)(implicit session: Session): Unit = {
    rename(owner, repository, owner, newRepository)
  }

  override def transferred(owner: String, newOwner: String, repository: String)(implicit session: Session): Unit = {
    rename(owner, repository, newOwner, repository)
  }

  // TODO select - insert
  private def rename(owner: String, repository: String, newOwner: String, newRepository: String)(implicit session: Session) = {
    val n = IssueNotifications.filter(t => t.userName === owner.bind && t.repositoryName === repository.bind).list
    val w = Watches.filter(t => t.userName === owner.bind && t.repositoryName === repository.bind).list

    deleted(owner, repository)

    IssueNotifications.insertAll(n.map(_.copy(userName = newOwner, repositoryName = newRepository)) :_*)
    Watches.insertAll(w.map(_.copy(userName = newOwner, repositoryName = newRepository)) :_*)
  }

}

class IssueHook extends gitbucket.core.plugin.IssueHook
  with NotificationsService
  with RepositoryService
  with AccountService
  with IssuesService
  with LabelsService
  with PrioritiesService
  with MilestonesService
  with SystemSettingsService
{
  private val logger = LoggerFactory.getLogger(classOf[IssueHook])

  override def created(issue: Issue, r: RepositoryInfo)(implicit session: Session, context: Context): Unit = {
    val baseUrl = context.baseUrl

    val repoData = s"[${r.owner}/${issue.repositoryName}](${s"${baseUrl}/${r.owner}/${r.name}"})"
    val issueData = s"[${issue.issueId}](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"})"
    val ownerData = s"[${issue.userName}](${s"${baseUrl}/${issue.userName}"})"
    val creatorData = s"[${issue.openedUserName}](${s"${baseUrl}/${issue.openedUserName}"})"
    var assigneeData = ""
    var priorityData = ""
    var milestoneData = ""
    val titleData = s"${issue.title}"

    if(issue.priorityId.isDefined) {
      val priority = Priorities.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.priorityId.get)).firstOption.get

      priorityData = priority.priorityName
    }

    if(issue.milestoneId.isDefined) {
      val milestone = Milestones.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.milestoneId.get)).firstOption.get

      milestoneData = s"[${milestone.title}](${baseUrl}/${r.owner}/${r.name}/milestone/${milestone.milestoneId})"
    }

    if(issue.assignedUserName.getOrElse("") != "") {
      assigneeData = s"[${issue.assignedUserName getOrElse ""}](${s"${baseUrl}/${issue.assignedUserName getOrElse ""}"})"
    }

    val editorData = s"[${context.loginAccount.get.userName}](${s"${baseUrl}/${context.loginAccount.get.userName}"})"

    val markdown =
      s"""|${s"| Info | Data |"}
          |${s"|------|------|"}
          |${s"| Repository: | ${repoData} |"}
          |${s"| Issue: | ${issueData} |"}
          |${s"| Owner: | ${ownerData} |"}
          |${s"| Creator: | ${creatorData} |"}
          |${s"| Assignee: | ${assigneeData} |"}
          |${s"| Priority: | ${priorityData} |"}
          |${s"| Milestone: | ${milestoneData} |"}
          |${s"| Title: | ${titleData} |"}
          |
          |${s"---"}
          |${s"### New issue by ${editorData}"}
          |${s"#### Title"}
          |${titleData}
          |
          |${s"#### Description"}
          |${issue.content getOrElse ""}
          |
          |${s"----"}
          |${s"[View it on GitBucket](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"})"}
          |""".stripMargin

    sendAsync(issue, r, subject(issue, r), markdown)
  }

  override def addedComment(commentId: Int, content: String, issue: Issue, r: RepositoryInfo)
                           (implicit session: Session, context: Context): Unit = {
    val baseUrl = context.baseUrl

    val repoData = s"[${r.owner}/${issue.repositoryName}](${s"${baseUrl}/${r.owner}/${r.name}"})"
    val issueData = s"[${issue.issueId}](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"})"
    val ownerData = s"[${issue.userName}](${s"${baseUrl}/${issue.userName}"})"
    val creatorData = s"[${issue.openedUserName}](${s"${baseUrl}/${issue.openedUserName}"})"
    var assigneeData = ""
    var priorityData = ""
    var milestoneData = ""
    val titleData = s"${issue.title}"

    if(issue.priorityId.isDefined) {
      val priority = Priorities.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.priorityId.get)).firstOption.get

      priorityData = priority.priorityName
    }

    if(issue.milestoneId.isDefined) {
      val milestone = Milestones.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.milestoneId.get)).firstOption.get

      milestoneData = s"[${milestone.title}](${baseUrl}/${r.owner}/${r.name}/milestone/${milestone.milestoneId})"
    }

    if(issue.assignedUserName.getOrElse("") != "") {
      assigneeData = s"[${issue.assignedUserName getOrElse ""}](${s"${baseUrl}/${issue.assignedUserName getOrElse ""}"})"
    }

    val editorData = s"[${context.loginAccount.get.userName}](${s"${baseUrl}/${context.loginAccount.get.userName}"})"

    val markdown =
      s"""|${s"| Info | Data |"}
          |${s"|------|------|"}
          |${s"| Repository: | ${repoData} |"}
          |${s"| Issue: | ${issueData} |"}
          |${s"| Owner: | ${ownerData} |"}
          |${s"| Creator: | ${creatorData} |"}
          |${s"| Assignee: | ${assigneeData} |"}
          |${s"| Priority: | ${priorityData} |"}
          |${s"| Milestone: | ${milestoneData} |"}
          |${s"| Title: | ${titleData} |"}
          |
          |${s"---"}
          |${s"### New comment by ${editorData}"}
          |${content}
          |
          |${s"----"}
          |${s"[View it on GitBucket](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}#comment-$commentId"})"}
          |""".stripMargin

    sendAsync(issue, r, subject(issue, r), markdown)
  }

  override def updatedComment(commentId: Int, content: String, issue: Issue, r: RepositoryInfo)
                             (implicit session: Session, context: Context): Unit = {
    val baseUrl = context.baseUrl

    val repoData = s"[${r.owner}/${issue.repositoryName}](${s"${baseUrl}/${r.owner}/${r.name}"})"
    val issueData = s"[${issue.issueId}](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"})"
    val ownerData = s"[${issue.userName}](${s"${baseUrl}/${issue.userName}"})"
    val creatorData = s"[${issue.openedUserName}](${s"${baseUrl}/${issue.openedUserName}"})"
    var assigneeData = ""
    var priorityData = ""
    var milestoneData = ""
    val titleData = s"${issue.title}"

    if(issue.priorityId.isDefined) {
      val priority = Priorities.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.priorityId.get)).firstOption.get

      priorityData = priority.priorityName
    }

    if(issue.milestoneId.isDefined) {
      val milestone = Milestones.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.milestoneId.get)).firstOption.get

      milestoneData = s"[${milestone.title}](${baseUrl}/${r.owner}/${r.name}/milestone/${milestone.milestoneId})"
    }

    if(issue.assignedUserName.getOrElse("") != "") {
      assigneeData = s"[${issue.assignedUserName getOrElse ""}](${s"${baseUrl}/${issue.assignedUserName getOrElse ""}"})"
    }

    val editorData = s"[${context.loginAccount.get.userName}](${s"${baseUrl}/${context.loginAccount.get.userName}"})"

    val markdown =
      s"""|${s"| Info | Data |"}
          |${s"|------|------|"}
          |${s"| Repository: | ${repoData} |"}
          |${s"| Issue: | ${issueData} |"}
          |${s"| Owner: | ${ownerData} |"}
          |${s"| Creator: | ${creatorData} |"}
          |${s"| Assignee: | ${assigneeData} |"}
          |${s"| Priority: | ${priorityData} |"}
          |${s"| Milestone: | ${milestoneData} |"}
          |${s"| Title: | ${titleData} |"}
          |
          |${s"---"}
          |${s"### Updated comment by ${editorData}"}
          |${content}
          |
          |${s"----"}
          |${s"[View it on GitBucket](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}#comment-$commentId"})"}
          |""".stripMargin

    sendAsync(issue, r, subject(issue, r), markdown)
  }

  override def closed(issue: Issue, r: RepositoryInfo)(implicit session: Session, context: Context): Unit = {
    val baseUrl = context.baseUrl

    val repoData = s"[${r.owner}/${issue.repositoryName}](${s"${baseUrl}/${r.owner}/${r.name}"})"
    val issueData = s"[${issue.issueId}](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"})"
    val ownerData = s"[${issue.userName}](${s"${baseUrl}/${issue.userName}"})"
    val creatorData = s"[${issue.openedUserName}](${s"${baseUrl}/${issue.openedUserName}"})"
    var assigneeData = ""
    var priorityData = ""
    var milestoneData = ""
    val titleData = s"${issue.title}"

    if(issue.priorityId.isDefined) {
      val priority = Priorities.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.priorityId.get)).firstOption.get

      priorityData = priority.priorityName
    }

    if(issue.milestoneId.isDefined) {
      val milestone = Milestones.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.milestoneId.get)).firstOption.get

      milestoneData = s"[${milestone.title}](${baseUrl}/${r.owner}/${r.name}/milestone/${milestone.milestoneId})"
    }

    if(issue.assignedUserName.getOrElse("") != "") {
      assigneeData = s"[${issue.assignedUserName getOrElse ""}](${s"${baseUrl}/${issue.assignedUserName getOrElse ""}"})"
    }

    val editorData = s"[${context.loginAccount.get.userName}](${s"${baseUrl}/${context.loginAccount.get.userName}"})"

    val markdown =
      s"""|${s"| Info | Data |"}
          |${s"|------|------|"}
          |${s"| Repository: | ${repoData} |"}
          |${s"| Issue: | ${issueData} |"}
          |${s"| Owner: | ${ownerData} |"}
          |${s"| Creator: | ${creatorData} |"}
          |${s"| Assignee: | ${assigneeData} |"}
          |${s"| Priority: | ${priorityData} |"}
          |${s"| Milestone: | ${milestoneData} |"}
          |${s"| Title: | ${titleData} |"}
          |
          |${s"---"}
          |${s"### Issue closed by ${editorData}"}
          |
          |${s"----"}
          |${s"[View it on GitBucket](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"})"}
          |""".stripMargin

    sendAsync(issue, r, subject(issue, r), markdown)
  }

  override def reopened(issue: Issue, r: RepositoryInfo)(implicit session: Session, context: Context): Unit = {
    val baseUrl = context.baseUrl

    val repoData = s"[${r.owner}/${issue.repositoryName}](${s"${baseUrl}/${r.owner}/${r.name}"})"
    val issueData = s"[${issue.issueId}](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"})"
    val ownerData = s"[${issue.userName}](${s"${baseUrl}/${issue.userName}"})"
    val creatorData = s"[${issue.openedUserName}](${s"${baseUrl}/${issue.openedUserName}"})"
    var assigneeData = ""
    var priorityData = ""
    var milestoneData = ""
    val titleData = s"${issue.title}"

    if(issue.priorityId.isDefined) {
      val priority = Priorities.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.priorityId.get)).firstOption.get

      priorityData = priority.priorityName
    }

    if(issue.milestoneId.isDefined) {
      val milestone = Milestones.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.milestoneId.get)).firstOption.get

      milestoneData = s"[${milestone.title}](${baseUrl}/${r.owner}/${r.name}/milestone/${milestone.milestoneId})"
    }

    if(issue.assignedUserName.getOrElse("") != "") {
      assigneeData = s"[${issue.assignedUserName getOrElse ""}](${s"${baseUrl}/${issue.assignedUserName getOrElse ""}"})"
    }

    val editorData = s"[${context.loginAccount.get.userName}](${s"${baseUrl}/${context.loginAccount.get.userName}"})"

    val markdown =
      s"""|${s"| Info | Data |"}
          |${s"|------|------|"}
          |${s"| Repository: | ${repoData} |"}
          |${s"| Issue: | ${issueData} |"}
          |${s"| Owner: | ${ownerData} |"}
          |${s"| Creator: | ${creatorData} |"}
          |${s"| Assignee: | ${assigneeData} |"}
          |${s"| Priority: | ${priorityData} |"}
          |${s"| Milestone: | ${milestoneData} |"}
          |${s"| Title: | ${titleData} |"}
          |
          |${s"---"}
          |${s"### Issue reopened by ${editorData}"}
          |
          |----
          |[View it on GitBucket](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"})
          |""".stripMargin

    sendAsync(issue, r, subject(issue, r), markdown)
  }

  override def assigned(issue: Issue, r: RepositoryInfo, assigner: Option[String], assigned: Option[String], oldAssigned: Option[String])(implicit session: model.Profile.profile.api.Session, context: Context): Unit = {
    val baseUrl = context.baseUrl

    val assignerMessage = assigner.flatMap(getAccountByUserName(_)).map(a => s"${a.fullName}([${a.userName}](${baseUrl}/${a.userName}))").getOrElse("unknown user")
    val assignedMessage = assigned.flatMap(getAccountByUserName(_)).map(a => s"${a.fullName}([${a.userName}](${baseUrl}/${a.userName}))").getOrElse("not assigned")
    val oldAssignedMessage = oldAssigned.flatMap(getAccountByUserName(_, true)).map(a => s"${a.fullName}([${a.userName}](${baseUrl}/${a.userName}))").getOrElse("not assigned")

    val repoData = s"[${r.owner}/${issue.repositoryName}](${s"${baseUrl}/${r.owner}/${r.name}"})"
    val issueData = s"[${issue.issueId}](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"})"
    val ownerData = s"[${issue.userName}](${s"${baseUrl}/${issue.userName}"})"
    val creatorData = s"[${issue.openedUserName}](${s"${baseUrl}/${issue.openedUserName}"})"
    var assigneeData = ""
    var priorityData = ""
    var milestoneData = ""
    val titleData = s"${issue.title}"

    if(issue.priorityId.isDefined) {
      val priority = Priorities.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.priorityId.get)).firstOption.get

      priorityData = priority.priorityName
    }

    if(issue.milestoneId.isDefined) {
      val milestone = Milestones.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.milestoneId.get)).firstOption.get

      milestoneData = s"[${milestone.title}](${baseUrl}/${r.owner}/${r.name}/milestone/${milestone.milestoneId})"
    }

    if(issue.assignedUserName.getOrElse("") != "") {
      assigneeData = s"[${issue.assignedUserName getOrElse ""}](${s"${baseUrl}/${issue.assignedUserName getOrElse ""}"})"
    }

    val markdown =
      s"""|${s"| Info | Data |"}
          |${s"|------|------|"}
          |${s"| Repository: | ${repoData} |"}
          |${s"| Issue: | ${issueData} |"}
          |${s"| Owner: | ${ownerData} |"}
          |${s"| Creator: | ${creatorData} |"}
          |${s"| Assignee: | ${assigneeData} |"}
          |${s"| Priority: | ${priorityData} |"}
          |${s"| Milestone: | ${milestoneData} |"}
          |${s"| Title: | ${titleData} |"}
          |
          |${s"---"}
          |${s"### Issue assigned from ${oldAssignedMessage} to ${assignedMessage} by ${assignerMessage}"}
          |
          |${s"----"}
          |${s"[View it on GitBucket](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"})"}
          |""".stripMargin

    sendAsync(issue, r, subject(issue, r), markdown)
  }

  override def closedByCommitComment(issue: Issue, r: RepositoryInfo, commitMessage: String, pusher: Account)(implicit session: core.model.Profile.profile.api.Session): Unit = {
    val settings = loadSystemSettings()
    val baseUrl = settings.baseUrl.get

    val repoData = s"[${r.owner}/${issue.repositoryName}](${s"${baseUrl}/${r.owner}/${r.name}"})"
    val issueData = s"[${issue.issueId}](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"})"
    val ownerData = s"[${issue.userName}](${s"${baseUrl}/${issue.userName}"})"
    val creatorData = s"[${issue.openedUserName}](${s"${baseUrl}/${issue.openedUserName}"})"
    var assigneeData = ""
    var priorityData = ""
    var milestoneData = ""
    val titleData = s"${issue.title}"

    val repoHtmlLink = s"""|<a href="${baseUrl}/${r.owner}/${r.name}">${r.owner}/${issue.repositoryName}</a>""".stripMargin
    val issueHtmlLink = s"""<a href="${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"}">${issue.issueId}</a>""".stripMargin
    val ownerHtmlLink = s"""<a href="${s"${baseUrl}/${issue.userName}"}">${issue.userName}</a>""".stripMargin
    val creatorHtmlLink = s"""<a href="${s"${baseUrl}/${issue.openedUserName}"}">${issue.openedUserName}</a>""".stripMargin
    var assigneeHtmlLink = ""
    var milestoneHtmlLink = ""
    val pusherHtmlLink = s"""<a href="${s"${baseUrl}/${pusher.userName}"}">${pusher.userName}</a>""".stripMargin

    val viewOnGitBucketHtmlLink =
      s"""<a href="${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"}">View it on GitBucket</a>""".stripMargin

    if(issue.priorityId.isDefined) {
      val priority = Priorities.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.priorityId.get)).firstOption.get

      priorityData = priority.priorityName
    }

    if(issue.milestoneId.isDefined) {
      val milestone = Milestones.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.milestoneId.get)).firstOption.get

      milestoneData = s"[${milestone.title}](${baseUrl}/${r.owner}/${r.name}/milestone/${milestone.milestoneId})"
      milestoneHtmlLink = s"""<a href="${baseUrl}/${r.owner}/${r.name}/milestone/${milestone.milestoneId}">${milestone.title}</a>""".stripMargin
    }

    if(issue.assignedUserName.getOrElse("") != "") {
      assigneeData = s"[${issue.assignedUserName getOrElse ""}](${s"${baseUrl}/${issue.assignedUserName getOrElse ""}"})"
      assigneeHtmlLink = s"""<a href="${s"${baseUrl}/${issue.assignedUserName getOrElse ""}"}">${issue.assignedUserName getOrElse ""}</a>""".stripMargin
    }

    val markDownMessage =
      s"""|${s"| Info | Data |"}
          |${s"|------|------|"}
          |${s"| Repository: | ${repoData} |"}
          |${s"| Issue: | ${issueData} |"}
          |${s"| Owner: | ${ownerData} |"}
          |${s"| Creator: | ${creatorData} |"}
          |${s"| Assignee: | ${assigneeData} |"}
          |${s"| Priority: | ${priorityData} |"}
          |${s"| Milestone: | ${milestoneData} |"}
          |${s"| Title: | ${titleData} |"}
          |
          |${s"---"}
          |${s"### Issue closed with commit by [${pusher.userName}](${s"${baseUrl}/${pusher.userName}"})"}
          |${commitMessage}
          |
          |${s"----"}
          |${s"[View it on GitBucket](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"})"}
          |""".stripMargin

    val htmlMessage =
      s"""|<table>
          | <tr>
          |  <th>Info:</td>
          |  <th>Data:</td>
          | </tr>
          | <tr>
          |  <td>Repository:</td>
          |  <td>${repoHtmlLink}</td>
          | </tr>
          | <tr>
          |  <td>Issue:</td>
          |  <td>${issueHtmlLink}</td>
          | </tr>
          | <tr>
          |  <td>Owner:</td>
          |  <td>${ownerHtmlLink}</td>
          | </tr>
          | <tr>
          |  <td>Creator:</td>
          |  <td>${creatorHtmlLink}</td>
          | </tr>
          | <tr>
          |  <td>Assignee:</td>
          |  <td>${assigneeHtmlLink}</td>
          | </tr>
          | <tr>
          |  <td>Priority:</td>
          |  <td>${priorityData}</td>
          | </tr>
          | <tr>
          |  <td>Milestone:</td>
          |  <td>${milestoneHtmlLink}</td>
          | </tr>
          | <tr>
          |  <td>Title:</td>
          |  <td>${titleData}</td>
          | </tr>
          |</table>
          |
          |<hr>
          |<div style=""><h3>Issue closed with commit by ${pusherHtmlLink}</h3></div>
          |<div>${commitMessage}</div>
          |
          |<hr>
          |<div>${viewOnGitBucketHtmlLink}</div>
          |""".stripMargin

    println(markDownMessage)
    sendAsyncHtml(issue, r, subject(issue, r), markDownMessage, Some(htmlMessage), pusher, settings)
  }

  protected def subject(issue: Issue, r: RepositoryInfo): String = {
    s"[${r.owner}/${r.name}] ${issue.title} (#${issue.issueId})"
  }

  protected def toHtml(markdown: String, r: RepositoryInfo)(implicit context: Context): String =
    Markdown.toHtml(
      markdown         = markdown,
      repository       = r,
      branch           = r.repository.defaultBranch,
      enableWikiLink   = false,
      enableRefsLink   = true,
      enableAnchor     = false,
      enableLineBreaks = false
    )

  protected def sendAsyncHtml(issue: Issue,
                              repository: RepositoryInfo,
                              subject: String,
                              textMessage: String,
                              htmlMessage: Option[String],
                              senderAccount: Account,
                              settings: SystemSettingsService.SystemSettings)
                             (implicit session:Session): Unit =
  {
    val recipients = getRecipients(issue, senderAccount)
    val mailer = new Mailer(settings)
    val f = Future {
      recipients.foreach { address =>
        mailer.send(address, subject, textMessage, htmlMessage, Some(senderAccount))
      }
      "Notifications Successful."
    }
    f.onComplete {
      case Success(s) => logger.debug(s)
      case Failure(t) => logger.error("Notifications Failed.", t)
    }
  }

  protected def sendAsyncTextOnly(issue: Issue,
                                  repository: RepositoryInfo,
                                  subject: String,
                                  message: String,
                                  senderAccount: Account,
                                  settings: SystemSettingsService.SystemSettings)
                                 (implicit session:Session): Unit =
  {
    val recipients = getRecipients(issue, senderAccount)
    val mailer = new Mailer(settings)
    val f = Future {
      recipients.foreach { address =>
        mailer.send(address, subject, message, None, Some(senderAccount))
      }
      "Notifications Successful."
    }
    f.onComplete {
      case Success(s) => logger.debug(s)
      case Failure(t) => logger.error("Notifications Failed.", t)
    }
  }

  protected def sendAsync(issue: Issue, repository: RepositoryInfo, subject: String, markdown: String)
                         (implicit session: Session, context: Context): Unit = {
    val recipients = getRecipients(issue, context.loginAccount.get)
    val mailer = new Mailer(context.settings)
    val html = toHtml(markdown, repository)
    val f = Future {
      recipients.foreach { address =>
        mailer.send(address, subject, markdown, Some(html), context.loginAccount)
      }
      "Notifications Successful."
    }
    f.onComplete {
      case Success(s) => logger.debug(s)
      case Failure(t) => logger.error("Notifications Failed.", t)
    }
  }

  protected def getRecipients(issue: Issue, loginAccount: Account)(implicit session: Session): Seq[String] = {
    getNotificationUsers(issue)
      //.withFilter ( _ != loginAccount.userName )  // the operation in person is excluded
      .flatMap (
      getAccountByUserName(_)
        .filterNot (_.isGroupAccount)
        .filterNot (LDAPUtil.isDummyMailAddress)
        .filterNot (isDisableEmailNotification)
        .map (account =>
          account.mailAddress :: getAccountExtraMailAddresses(account.userName)
        )
      )
      .flatten
      .distinct
  }

}

class PullRequestHook extends IssueHook
  with gitbucket.core.plugin.PullRequestHook
{
  override def created(issue: Issue, r: RepositoryInfo)(implicit session: Session, context: Context): Unit = {
    val baseUrl = context.baseUrl

    val repoData = s"[${r.owner}/${issue.repositoryName}](${s"${baseUrl}/${r.owner}/${r.name}"})"
    val issueData = s"[${issue.issueId}](${s"${baseUrl}/${r.owner}/${r.name}/pull/${issue.issueId}"})"
    val ownerData = s"[${issue.userName}](${s"${baseUrl}/${issue.userName}"})"
    val creatorData = s"[${issue.openedUserName}](${s"${baseUrl}/${issue.openedUserName}"})"
    var assigneeData = ""
    var priorityData = ""
    var milestoneData = ""
    val titleData = s"${issue.title}"

    if(issue.priorityId.isDefined) {
      val priority = Priorities.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.priorityId.get)).firstOption.get

      priorityData = priority.priorityName
    }

    if(issue.milestoneId.isDefined) {
      val milestone = Milestones.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.milestoneId.get)).firstOption.get

      milestoneData = s"[${milestone.title}](${baseUrl}/${r.owner}/${r.name}/milestone/${milestone.milestoneId})"
    }

    if(issue.assignedUserName.getOrElse("") != "") {
      assigneeData = s"[${issue.assignedUserName getOrElse ""}](${s"${baseUrl}/${issue.assignedUserName getOrElse ""}"})"
    }

    val editorData = s"[${context.loginAccount.get.userName}](${s"${baseUrl}/${context.loginAccount.get.userName}"})"

    val markdown =
      s"""|${s"| Info | Data |"}
          |${s"|------|------|"}
          |${s"| Repository: | ${repoData} |"}
          |${s"| Pull-Request: | ${issueData} |"}
          |${s"| Owner: | ${ownerData} |"}
          |${s"| Creator: | ${creatorData} |"}
          |${s"| Assignee: | ${assigneeData} |"}
          |${s"| Priority: | ${priorityData} |"}
          |${s"| Milestone: | ${milestoneData} |"}
          |${s"| Title: | ${titleData} |"}
          |
          |${s"---"}
          |${s"### New Pull-Request by ${editorData}"}
          |${s"#### Title"}
          |${titleData}
          |
          |${s"#### Description"}
          | ${issue.content getOrElse ""}
          |
          |${s"----"}
          |${s"[View it on GitBucket](${s"${baseUrl}/${r.owner}/${r.name}/pull/${issue.issueId}"})"}
          |""".stripMargin

    sendAsync(issue, r, subject(issue, r), markdown)
  }

  override def addedComment(commentId: Int, content: String, issue: Issue, r: RepositoryInfo)
                           (implicit session: Session, context: Context): Unit = {
    val baseUrl = context.baseUrl

    val repoData = s"[${r.owner}/${issue.repositoryName}](${s"${baseUrl}/${r.owner}/${r.name}"})"
    val issueData = s"[${issue.issueId}](${s"${baseUrl}/${r.owner}/${r.name}/pull/${issue.issueId}"})"
    val ownerData = s"[${issue.userName}](${s"${baseUrl}/${issue.userName}"})"
    val creatorData = s"[${issue.openedUserName}](${s"${baseUrl}/${issue.openedUserName}"})"
    var assigneeData = ""
    var priorityData = ""
    var milestoneData = ""
    val titleData = s"${issue.title}"

    if(issue.priorityId.isDefined) {
      val priority = Priorities.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.priorityId.get)).firstOption.get

      priorityData = priority.priorityName
    }

    if(issue.milestoneId.isDefined) {
      val milestone = Milestones.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.milestoneId.get)).firstOption.get

      milestoneData = s"[${milestone.title}](${baseUrl}/${r.owner}/${r.name}/milestone/${milestone.milestoneId})"
    }

    if(issue.assignedUserName.getOrElse("") != "") {
      assigneeData = s"[${issue.assignedUserName getOrElse ""}](${s"${baseUrl}/${issue.assignedUserName getOrElse ""}"})"
    }

    val editorData = s"[${context.loginAccount.get.userName}](${s"${baseUrl}/${context.loginAccount.get.userName}"})"

    val markdown =
      s"""|${s"| Info | Data |"}
          |${s"|------|------|"}
          |${s"| Repository: | ${repoData} |"}
          |${s"| Pull-Request: | ${issueData} |"}
          |${s"| Owner: | ${ownerData} |"}
          |${s"| Creator: | ${creatorData} |"}
          |${s"| Assignee: | ${assigneeData} |"}
          |${s"| Priority: | ${priorityData} |"}
          |${s"| Milestone: | ${milestoneData} |"}
          |${s"| Title: | ${titleData} |"}
          |
          |${s"---"}
          |${s"### New comment by ${editorData}"}
          |$content
          |
          |${s"----"}
          |${s"[View it on GitBucket](${s"${baseUrl}/${r.owner}/${r.name}/pull/${issue.issueId}#comment-$commentId"})"}
          |""".stripMargin

    sendAsync(issue, r, subject(issue, r), markdown)
  }

  override def updatedComment(commentId: Int, content: String, issue: Issue, r: RepositoryInfo)
                             (implicit session: Session, context: Context): Unit = {
    val baseUrl = context.baseUrl

    val repoData = s"[${r.owner}/${issue.repositoryName}](${s"${baseUrl}/${r.owner}/${r.name}"})"
    val issueData = s"[${issue.issueId}](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"})"
    val ownerData = s"[${issue.userName}](${s"${baseUrl}/${issue.userName}"})"
    val creatorData = s"[${issue.openedUserName}](${s"${baseUrl}/${issue.openedUserName}"})"
    var assigneeData = ""
    var priorityData = ""
    var milestoneData = ""
    val titleData = s"${issue.title}"

    if(issue.priorityId.isDefined) {
      val priority = Priorities.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.priorityId.get)).firstOption.get

      priorityData = priority.priorityName
    }

    if(issue.milestoneId.isDefined) {
      val milestone = Milestones.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.milestoneId.get)).firstOption.get

      milestoneData = s"[${milestone.title}](${baseUrl}/${r.owner}/${r.name}/milestone/${milestone.milestoneId})"
    }

    if(issue.assignedUserName.getOrElse("") != "") {
      assigneeData = s"[${issue.assignedUserName getOrElse ""}](${s"${baseUrl}/${issue.assignedUserName getOrElse ""}"})"
    }

    val editorData = s"[${context.loginAccount.get.userName}](${s"${baseUrl}/${context.loginAccount.get.userName}"})"

    val markdown =
      s"""|${s"| Info | Data |"}
          |${s"|------|------|"}
          |${s"| Repository: | ${repoData} |"}
          |${s"| Pull-Request: | ${issueData} |"}
          |${s"| Owner: | ${ownerData} |"}
          |${s"| Creator: | ${creatorData} |"}
          |${s"| Assignee: | ${assigneeData} |"}
          |${s"| Priority: | ${priorityData} |"}
          |${s"| Milestone: | ${milestoneData} |"}
          |${s"| Title: | ${titleData} |"}
          |
          |${s"---"}
          |${s"### Updated comment by ${editorData}"}
          |${content}
          |
          |${s"----"}
          |${s"[View it on GitBucket](${s"${baseUrl}/${r.owner}/${r.name}/pull/${issue.issueId}#comment-$commentId"})"}
          |""".stripMargin

    sendAsync(issue, r, subject(issue, r), markdown)
  }

  override def assigned(issue: Issue, r: RepositoryInfo, assigner: Option[String], assigned: Option[String], oldAssigned: Option[String])
                       (implicit session: model.Profile.profile.api.Session, context: Context): Unit =
  {
    val baseUrl = context.baseUrl

    val assignerMessage = assigner.flatMap(getAccountByUserName(_)).map(a => s"${a.fullName}([${a.userName}](${baseUrl}/${a.userName}))").getOrElse("unknown user")
    val assignedMessage = assigned.flatMap(getAccountByUserName(_)).map(a => s"${a.fullName}([${a.userName}](${baseUrl}/${a.userName}))").getOrElse("not assigned")
    val oldAssignedMessage = oldAssigned.flatMap(getAccountByUserName(_, true)).map(a => s"${a.fullName}([${a.userName}](${baseUrl}/${a.userName}))").getOrElse("not assigned")

    val repoData = s"[${r.owner}/${issue.repositoryName}](${s"${baseUrl}/${r.owner}/${r.name}"})"
    val issueData = s"[${issue.issueId}](${s"${baseUrl}/${r.owner}/${r.name}/issues/${issue.issueId}"})"
    val ownerData = s"[${issue.userName}](${s"${baseUrl}/${issue.userName}"})"
    val creatorData = s"[${issue.openedUserName}](${s"${baseUrl}/${issue.openedUserName}"})"
    var assigneeData = ""
    var priorityData = ""
    var milestoneData = ""
    val titleData = s"${issue.title}"

    if(issue.priorityId.isDefined) {
      val priority = Priorities.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.priorityId.get)).firstOption.get

      priorityData = priority.priorityName
    }

    if(issue.milestoneId.isDefined) {
      val milestone = Milestones.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.milestoneId.get)).firstOption.get

      milestoneData = s"[${milestone.title}](${baseUrl}/${r.owner}/${r.name}/milestone/${milestone.milestoneId})"
    }

    if(issue.assignedUserName.getOrElse("") != "") {
      assigneeData = s"[${issue.assignedUserName getOrElse ""}](${s"${baseUrl}/${issue.assignedUserName getOrElse ""}"})"
    }

    val markdown =
      s"""|${s"| Info | Data |"}
          |${s"|------|------|"}
          |${s"| Repository: | ${repoData} |"}
          |${s"| Pull-Request: | ${issueData} |"}
          |${s"| Owner: | ${ownerData} |"}
          |${s"| Creator: | ${creatorData} |"}
          |${s"| Assignee: | ${assigneeData} |"}
          |${s"| Priority: | ${priorityData} |"}
          |${s"| Milestone: | ${milestoneData} |"}
          |${s"| Title: | ${titleData} |"}
          |
          |${s"---"}
          |${s"### Pull-Request assigned from ${oldAssignedMessage} to ${assignedMessage} by ${assignerMessage}"}
          |
          |${s"----"}
          |${s"[View it on GitBucket](${s"${baseUrl}/${r.owner}/${r.name}/pull/${issue.issueId}"})"}
          |""".stripMargin

    sendAsync(issue, r, subject(issue, r), markdown)
  }

  override def merged(issue: Issue, r: RepositoryInfo)(implicit session: Session, context: Context): Unit = {
    val baseUrl = context.baseUrl

    val repoData = s"[${r.owner}/${issue.repositoryName}](${s"${baseUrl}/${r.owner}/${r.name}"})"
    val issueData = s"[${issue.issueId}](${s"${baseUrl}/${r.owner}/${r.name}/pull/${issue.issueId}"})"
    val ownerData = s"[${issue.userName}](${s"${baseUrl}/${issue.userName}"})"
    val creatorData = s"[${issue.openedUserName}](${s"${baseUrl}/${issue.openedUserName}"})"
    var assigneeData = ""
    var priorityData = ""
    var milestoneData = ""
    val titleData = s"${issue.title}"

    if(issue.priorityId.isDefined) {
      val priority = Priorities.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.priorityId.get)).firstOption.get

      priorityData = priority.priorityName
    }

    if(issue.milestoneId.isDefined) {
      val milestone = Milestones.filter(_.byPrimaryKey(r.owner, issue.repositoryName, issue.milestoneId.get)).firstOption.get

      milestoneData = s"[${milestone.title}](${baseUrl}/${r.owner}/${r.name}/milestone/${milestone.milestoneId})"
    }

    if(issue.assignedUserName.getOrElse("") != "") {
      assigneeData = s"[${issue.assignedUserName getOrElse ""}](${s"${baseUrl}/${issue.assignedUserName getOrElse ""}"})"
    }

    val editorData = s"[${context.loginAccount.get.userName}](${s"${baseUrl}/${context.loginAccount.get.userName}"})"

    val markdown =
      s"""|${s"| Info | Data |"}
          |${s"|------|------|"}
          |${s"| Repository: | ${repoData} |"}
          |${s"| Pull-Request: | ${issueData} |"}
          |${s"| Owner: | ${ownerData} |"}
          |${s"| Creator: | ${creatorData} |"}
          |${s"| Assignee: | ${assigneeData} |"}
          |${s"| Priority: | ${priorityData} |"}
          |${s"| Milestone: | ${milestoneData} |"}
          |${s"| Title: | ${titleData} |"}
          |
          |${s"---"}
          |${s"### Pull-Request merged by ${editorData}"}
          |
          |${s"----"}
          |${s"[View it on GitBucket](${s"${baseUrl}/${r.owner}/${r.name}/pull/${issue.issueId}"})"}
          |""".stripMargin

    sendAsync(issue, r, subject(issue, r), markdown)
  }
}

class ReceiveHook extends gitbucket.core.plugin.ReceiveHook
  with NotificationsService
  with RepositoryService
  with AccountService
  with IssuesService
  with LabelsService
  with PrioritiesService
  with MilestonesService
  with SystemSettingsService
{
  private val logger = LoggerFactory.getLogger(classOf[ReceiveHook])

  override def postReceive(owner: String,
                           repository: String,
                           receivePack: ReceivePack,
                           command: ReceiveCommand,
                           pusher: String,
                           mergePullRequest: Boolean
                          )(
                          implicit session: Session
  ): Unit = {
    try {
      val settings = loadSystemSettings()

      if(settings.baseUrl != None) {
        val baseUrl = settings.baseUrl.get

        val repo = getRepository(owner, repository).get
        val pusherAccount = getAccountByUserName(pusher, true)

        val refName = command.getRefName().split("/")
        val branchName = refName.drop(2).mkString("/")

        val repoData = s"[${repo.owner}/${repo.name}](${s"${baseUrl}/${repo.owner}/${repo.name}"})"
        val repoHtmlLink = s"""|<a href="${baseUrl}/${repo.owner}/${repo.name}">${repo.owner}/${repo.name}</a>""".stripMargin

        val branchNameData = s"[${branchName}](${s"${baseUrl}/${repo.owner}/${repo.name}/tree/${branchName}"})"
        val branchNameHtmlLink = s"""|<a href="${baseUrl}/${repo.owner}/${repo.name}/tree/${branchName}">${branchName}</a>""".stripMargin

        val pusherData = s"[${pusherAccount.get.userName}](${s"${baseUrl}/${pusherAccount.get.userName}"})"
        val pusherHtmlLink = s"""<a href="${s"${baseUrl}/${pusherAccount.get.userName}"}">${pusherAccount.get.userName}</a>""".stripMargin

        val oldCommitRefData =
          s"[${command.getOldId().name()}](${s"${baseUrl}/${repo.owner}/${repo.name}/commit/${command.getOldId().name()}"})"
        val oldCommitRefHtmlLink =
          s"""<a href="${s"${baseUrl}/${repo.owner}/${repo.name}/commit/${command.getOldId().name()}"}">${command.getOldId().name()}</a>""".stripMargin

        val newCommitRefData =
          s"[${command.getNewId().name()}](${s"${baseUrl}/${repo.owner}/${repo.name}/commit/${command.getNewId().name()}"})"
        val newCommitRefHtmlLink =
          s"""<a href="${s"${baseUrl}/${repo.owner}/${repo.name}/commit/${command.getNewId().name()}"}">${command.getNewId().name()}</a>""".stripMargin

        var commitsStrData = s""
        var commitsStrHtml = s""

        Using.resource(Git.open(getRepositoryDir(repo.owner, repo.name))) { git =>
          val commits = git.log
            .addRange(command.getOldId(), command.getNewId())
            .call
            .iterator
            .asScala
            .map(c => new JGitUtil.CommitInfo(c))
            .toList

          commitsStrData = commits
            .take(5)
            .map(commit => s"[${commit.id}](${s"${baseUrl}/${repo.owner}/${repo.name}/commit/${commit.id}"}) : ${commit.shortMessage}")
            .mkString("\n")

          commitsStrHtml = commits
            .take(5)
            .map(commit => s"""<a href="${s"${baseUrl}/${repo.owner}/${repo.name}/commit/${commit.id}"}">${commit.id}</a> : ${commit.shortMessage}""")
            .mkString("<br/>")
        }

        val viewOnGitBucketHtmlLink =
          s"""<a href="${s"${baseUrl}/${repo.owner}/${repo.name}"}">View it on GitBucket</a>""".stripMargin

        val markDownMessage =
          s"""|${s"| Info | Data |"}
              |${s"|------|------|"}
              |${s"| Repository: | ${repoData} |"}
              |${s"| Branch: | ${branchNameData} |"}
              |${s"| Old Ref: | ${oldCommitRefData} |"}
              |${s"| New Ref: | ${newCommitRefData} |"}
              |${s"| Pusher: | ${pusherData} |"}
              |
              |${s"---"}
              |${s"### ${pusherData} pushed to ${branchNameData} at ${repoData}"}
              |${commitsStrData}
              |
              |${s"----"}
              |${s"[View it on GitBucket](${s"${baseUrl}/${repo.owner}/${repo.name}"})"}
              |""".stripMargin

        val htmlMessage =
          s"""|<table>
              | <tr>
              |  <th>Info:</td>
              |  <th>Data:</td>
              | </tr>
              | <tr>
              |  <td>Repository:</td>
              |  <td>${repoHtmlLink}</td>
              | </tr>
              | <tr>
              |  <td>Branch:</td>
              |  <td>${branchNameHtmlLink}</td>
              | </tr>
              | <tr>
              |  <td>Old Ref:</td>
              |  <td>${oldCommitRefHtmlLink}</td>
              | </tr>
              | <tr>
              |  <td>New Ref:</td>
              |  <td>${newCommitRefHtmlLink}</td>
              | </tr>
              | <tr>
              |  <td>Pusher:</td>
              |  <td>${pusherHtmlLink}</td>
              | </tr>
              |</table>
              |
              |<hr>
              |<div style=""><h3>${pusherHtmlLink} pushed to ${branchNameHtmlLink} at ${repoHtmlLink}</h3></div>
              |${commitsStrHtml}
              |<br/>
              |
              |<hr>
              |<div>${viewOnGitBucketHtmlLink}</div>
              |""".stripMargin

        val mailer = new Mailer(settings)
        val recipients = getRecipients(repo, pusherAccount.get)
        val subject = s"[${repo.owner}/${repo.name}] ${pusherAccount.get.fullName} pushed commit/s"

        val f = Future {
          recipients.foreach { address =>
            mailer.send(address, subject, markDownMessage, Some(htmlMessage), pusherAccount)
          }
          "Notifications Successful."
        }
        f.onComplete {
          case Success(s) => logger.debug(s)
          case Failure(t) => logger.error("Notifications Failed.", t)
        }
      }
    } catch {
      case ex: Exception => {

      }
    }
  }

  protected def getRecipients(repository: RepositoryInfo, loginAccount: Account)(implicit session: Session): Seq[String] = {
    getNotificationUsers(repository)
      //.withFilter ( _ != loginAccount.userName )  // the operation in person is excluded
      .flatMap (
        getAccountByUserName(_)
          .filterNot (_.isGroupAccount)
          .filterNot (LDAPUtil.isDummyMailAddress)
          .filterNot (isDisableEmailNotification)
          .map (account =>
            account.mailAddress :: getAccountExtraMailAddresses(account.userName)
          )
      )
      .flatten
      .distinct
  }
}
