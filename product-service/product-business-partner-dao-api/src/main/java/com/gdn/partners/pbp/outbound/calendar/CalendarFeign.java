package com.gdn.partners.pbp.outbound.calendar;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.calendar.dto.DueDate;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface CalendarFeign {

  @RequestLine(
      "GET /api/holiday/getDueDateWithMinutesDurationWorkingWeek?storeId={storeId}&channelId={channelId}&clientId"
          + "={clientId}&requestId={requestId}&username={username}&calendarName={calendarName}&startDateTime={startDateTime}&durationInMinutes={durationInMinutes}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<DueDate> getDueDateWithMinutesDurationByWorkingWeek(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("calendarName") String calendarName,
      @Param("startDateTime") String startDateTime, @Param("durationInMinutes") Long durationInMinutes);
}
