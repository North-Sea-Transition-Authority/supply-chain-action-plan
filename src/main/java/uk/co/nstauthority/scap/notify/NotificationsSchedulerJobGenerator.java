package uk.co.nstauthority.scap.notify;

import static org.quartz.JobBuilder.newJob;
import static org.quartz.JobKey.jobKey;
import static org.quartz.TriggerKey.triggerKey;

import org.quartz.CalendarIntervalScheduleBuilder;
import org.quartz.JobKey;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.TriggerBuilder;
import org.quartz.TriggerKey;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

@Service
class NotificationsSchedulerJobGenerator {
  private static final Logger LOGGER = LoggerFactory.getLogger(NotificationsSchedulerJobGenerator.class);
  private static final JobKey NOTIFICATIONS_SCHEDULER_JOB_KEY = jobKey("NOTIFICATIONS_SCHEDULER_JOB", "SCAP_NOTIFICATIONS");
  private static final TriggerKey NOTIFICATIONS_SCHEDULER_TRIGGER_KEY =
      triggerKey("NOTIFICATIONS_SCHEDULER_TRIGGER", "SCAP_NOTIFICATION_TRIGGERS");

  private final Scheduler scheduler;

  @Autowired
  public NotificationsSchedulerJobGenerator(Scheduler scheduler) {
    this.scheduler = scheduler;
  }

  @EventListener(classes = ApplicationReadyEvent.class)
  public void registerJob() throws SchedulerException {
    if (scheduler.getJobDetail(NOTIFICATIONS_SCHEDULER_JOB_KEY) != null) {
      LOGGER.info("Notifications scheduler job found");
    } else {
      LOGGER.info("Notifications scheduler job does not exist. Creating...");
      var jobDetail = newJob(NotificationsSchedulerBean.class)
          .withIdentity(NOTIFICATIONS_SCHEDULER_JOB_KEY)
          .requestRecovery()
          .storeDurably()
          .build();

      var trigger = TriggerBuilder
          .newTrigger()
          .withIdentity(NOTIFICATIONS_SCHEDULER_TRIGGER_KEY)
          .startNow()
          .withSchedule(CalendarIntervalScheduleBuilder.calendarIntervalSchedule().withIntervalInHours(1))
          .build();

      scheduler.scheduleJob(jobDetail, trigger);

      LOGGER.info("Notifications scheduler job created");
    }
  }
}
