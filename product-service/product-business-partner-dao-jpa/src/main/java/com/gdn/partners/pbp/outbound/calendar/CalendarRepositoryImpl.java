package com.gdn.partners.pbp.outbound.calendar;

import java.util.Date;

import com.gdn.common.util.GdnDateHelper;
import com.gdn.x.calendar.dto.DueDate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;

/**
 * Created by Vishal on 15/05/18.
 */
@Repository
public class CalendarRepositoryImpl implements CalendarRepository{

  private static final Logger LOGGER = LoggerFactory.getLogger(CalendarRepositoryImpl.class);

  @Autowired
  private CalendarFeign calendarFeign;

  @Override
  public Date getDueDateWithMinutesDurationByWorkingWeek(String requestId, String calendarName, Date startDateTime,
      Long durationInMinutes, String username) throws Exception {
    GdnRestSingleResponse<DueDate> dateGdnRestSingleResponse =
        calendarFeign.getDueDateWithMinutesDurationByWorkingWeek(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, requestId, username, calendarName,
            GdnDateHelper.getFormattedDateTime(startDateTime), durationInMinutes);
    if (!dateGdnRestSingleResponse.isSuccess()) {
      LOGGER.error("failed to fetch detail from calendar api, errorMsg : {}",
          dateGdnRestSingleResponse.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          dateGdnRestSingleResponse.getErrorMessage());
    }
    return dateGdnRestSingleResponse.getValue().getEndDate();
  }
}
