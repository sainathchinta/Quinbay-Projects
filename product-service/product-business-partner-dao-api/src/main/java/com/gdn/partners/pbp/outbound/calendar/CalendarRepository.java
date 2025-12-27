package com.gdn.partners.pbp.outbound.calendar;

import java.util.Date;

/**
 * Created by Vishal on 15/05/18.
 */
public interface CalendarRepository {

  /**
   * get due date by working week by giving duration in minutes
   *
   * @param requestId request Id
   * @param calendarName name of calendar Optional
   * @param startDateTime start date of calendar must not null
   * @param durationInMinutes duration in minutes must not null
   * @param username user name
   * @return
   * @throws Exception
   */
  Date getDueDateWithMinutesDurationByWorkingWeek(String requestId,
      String calendarName, Date startDateTime, Long durationInMinutes, String username)
      throws Exception;
}
