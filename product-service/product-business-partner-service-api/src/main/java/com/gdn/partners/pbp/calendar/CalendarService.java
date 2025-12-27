package com.gdn.partners.pbp.calendar;

import java.util.Date;

/**
 * Created by Vishal on 15/05/18.
 */
public interface CalendarService {

  /**
   * get expected activation date by category code and submissionDate
   *
   * @param categoryCode category code must not blank
   * @param submissionDate submission date must not null
   * @return
   */
  Date getExpectedActivationDateByCategoryCode(String categoryCode, Date submissionDate);
}
