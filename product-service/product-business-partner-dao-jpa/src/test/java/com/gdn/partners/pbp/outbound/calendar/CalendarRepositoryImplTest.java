package com.gdn.partners.pbp.outbound.calendar;

import java.util.Date;

import com.gdn.common.util.GdnDateHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.calendar.dto.DueDate;

/**
 * Created by Vishal on 15/05/18.
 */
public class CalendarRepositoryImplTest {

  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String DEFAULT_USERNAME = "user";
  private static final String CALENDAR_NAME = "calendarName";

  @InjectMocks
  private CalendarRepositoryImpl calendarRepository;

  @Mock
  private CalendarFeign calendarFeign;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(calendarFeign);
  }

  @Test
  public void getDueDateWithMinutesDurationByWorkingWeek() throws Exception {
    Date expectationDate = new Date();
    DueDate dueDate = new DueDate(expectationDate, expectationDate);
    GdnRestSingleResponse<DueDate> dateGdnRestSingleResponse = new GdnRestSingleResponse<>(dueDate, DEFAULT_REQUEST_ID);
    Mockito.when(calendarFeign.getDueDateWithMinutesDurationByWorkingWeek(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME, CALENDAR_NAME,
        GdnDateHelper.getFormattedDateTime(expectationDate), 600L)).thenReturn(dateGdnRestSingleResponse);
    Date result = calendarRepository.getDueDateWithMinutesDurationByWorkingWeek(DEFAULT_REQUEST_ID, CALENDAR_NAME,
        expectationDate, 600L, DEFAULT_USERNAME);
    Mockito.verify(calendarFeign)
        .getDueDateWithMinutesDurationByWorkingWeek(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            CALENDAR_NAME, GdnDateHelper.getFormattedDateTime(expectationDate), 600L);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(result, expectationDate);
  }

  @Test
  public void getDueDateWithMinutesDurationByWorkingWeek_withSuccessFalse() throws Exception {
    Date expectationDate = new Date();
    DueDate dueDate = new DueDate(expectationDate, expectationDate);
    GdnRestSingleResponse<DueDate> dateGdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, false, null, DEFAULT_REQUEST_ID);
    Mockito.when(calendarFeign.getDueDateWithMinutesDurationByWorkingWeek(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME, CALENDAR_NAME,
        GdnDateHelper.getFormattedDateTime(expectationDate), 600L)).thenReturn(dateGdnRestSingleResponse);
    try {
      Date result = calendarRepository.getDueDateWithMinutesDurationByWorkingWeek(DEFAULT_REQUEST_ID, CALENDAR_NAME,
          expectationDate, 600L, DEFAULT_USERNAME);
    } catch (ApplicationRuntimeException e) {
      Mockito.verify(calendarFeign)
          .getDueDateWithMinutesDurationByWorkingWeek(GdnMandatoryRequestParameterUtil.getStoreId(),
              Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
              CALENDAR_NAME, GdnDateHelper.getFormattedDateTime(expectationDate), 600L);
    }

  }

}
