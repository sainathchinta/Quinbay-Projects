package com.gdn.partners.pbp.calendar;

import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.partners.pbp.outbound.calendar.CalendarRepository;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

/**
 * Created by Vishal on 15/05/18.
 */

@Service
public class CalendarServiceImpl implements CalendarService {

  public static final Logger LOGGER = LoggerFactory.getLogger(CalendarServiceImpl.class);
  private static final int HOURS_IN_A_DAY = 24;
  private static final int MINUTES_IN_HOUR = 60;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private CalendarRepository calendarRepository;

  @Value("${default.activation.interval:72}")
  private int defaultActivationInterval;

  @Value("${calendar.day.working.hour:10}")
  private int dayWorkingHours;

  @Override
  public Date getExpectedActivationDateByCategoryCode(String categoryCode, Date submissionDate) {
    Date expectedActivationDate = null;
    try {
      String requestId = MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER);
      String username = MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
      Long defaultInternalActivationPeriod =
          Long.valueOf((defaultActivationInterval / HOURS_IN_A_DAY) * dayWorkingHours * MINUTES_IN_HOUR);
      Integer internalActivationInterval = null;
      if (StringUtils.isNotBlank(categoryCode)) {
        List<CategoryResponse> categoryResponses =
            productRepository.filterCategoryHierarchyByCategoryCode(requestId, username, categoryCode).getContent();
        internalActivationInterval =
            categoryResponses.stream().filter(categoryResponse -> categoryResponse.getParentCategoryId() == null)
                .findFirst().orElse(new CategoryResponse()).getInternalActivationInterval();
      }
      Long internalActivationPeriodInMinutes = internalActivationInterval == null ?
          defaultInternalActivationPeriod :
          Long.valueOf((internalActivationInterval / HOURS_IN_A_DAY) * dayWorkingHours * MINUTES_IN_HOUR);
      expectedActivationDate = calendarRepository
          .getDueDateWithMinutesDurationByWorkingWeek(requestId, null, submissionDate,
              internalActivationPeriodInMinutes, username);
    } catch (Exception e) {
      LOGGER.error("failed to fetch activation date for cateogoryCode : {}", categoryCode, e);
    }
    return expectedActivationDate;
  }
}
