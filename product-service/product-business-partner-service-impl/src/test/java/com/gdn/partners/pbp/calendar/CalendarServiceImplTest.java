package com.gdn.partners.pbp.calendar;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import org.apache.log4j.MDC;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.partners.pbp.outbound.calendar.CalendarRepository;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

/**
 * Created by Vishal on 15/05/18.
 */
public class CalendarServiceImplTest {

  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String DEFAULT_USERNAME = "com.gdn.mta.developer";
  private static final String DEFAULT_ID = "1";
  private static final String DEFAULT_CATEGORY_CODE = "categoryCode";

  @Mock
  private ProductRepository productRepository;

  @Mock
  private CalendarRepository calendarRepository;

  @InjectMocks
  private CalendarServiceImpl calendarService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(calendarService, "defaultActivationInterval", 72);
    ReflectionTestUtils.setField(calendarService, "dayWorkingHours", 10);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(calendarRepository, productRepository);
  }

  @Test
  public void getExpectedActivationDateByCategoryCode() throws Exception {
    Date submissionDate = new Date();
    List<CategoryResponse> content = getCategoryResponses();
    GdnRestListResponse<CategoryResponse> gdnRestListCategoryReponse =
        new GdnRestListResponse<>(content, new PageMetaData(10, 0, 2), DEFAULT_REQUEST_ID);
    Mockito.when(calendarRepository
        .getDueDateWithMinutesDurationByWorkingWeek(DEFAULT_REQUEST_ID, null, submissionDate, 600L,
            DEFAULT_USERNAME)).thenReturn(submissionDate);
    Mockito.when(productRepository
        .filterCategoryHierarchyByCategoryCode(DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            DEFAULT_CATEGORY_CODE)).thenReturn(gdnRestListCategoryReponse);
    Date result = calendarService
        .getExpectedActivationDateByCategoryCode(DEFAULT_CATEGORY_CODE, submissionDate);
    Mockito.verify(productRepository)
        .filterCategoryHierarchyByCategoryCode(DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            DEFAULT_CATEGORY_CODE);
    Mockito.verify(calendarRepository)
        .getDueDateWithMinutesDurationByWorkingWeek(DEFAULT_REQUEST_ID, null, submissionDate, 600L,
            DEFAULT_USERNAME);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(submissionDate, result);
  }

  @Test
  public void getExpectedActivationDateByCategoryCodeNoParentCategory() throws Exception {
    Date submissionDate = new Date();
    List<CategoryResponse> content = getCategoryResponses();
    content.get(0).setParentCategoryId(DEFAULT_ID);
    GdnRestListResponse<CategoryResponse> gdnRestListCategoryReponse =
        new GdnRestListResponse<>(content, new PageMetaData(10, 0, 2), DEFAULT_REQUEST_ID);
    Mockito.when(calendarRepository
        .getDueDateWithMinutesDurationByWorkingWeek(DEFAULT_REQUEST_ID, null, submissionDate, 1800L,
            DEFAULT_USERNAME)).thenReturn(submissionDate);
    Mockito.when(productRepository
        .filterCategoryHierarchyByCategoryCode(DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            DEFAULT_CATEGORY_CODE)).thenReturn(gdnRestListCategoryReponse);
    Date result = calendarService
        .getExpectedActivationDateByCategoryCode(DEFAULT_CATEGORY_CODE, submissionDate);
    Mockito.verify(productRepository)
        .filterCategoryHierarchyByCategoryCode(DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            DEFAULT_CATEGORY_CODE);
    Mockito.verify(calendarRepository)
        .getDueDateWithMinutesDurationByWorkingWeek(DEFAULT_REQUEST_ID, null, submissionDate, 1800L,
            DEFAULT_USERNAME);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(submissionDate, result);
  }

  @Test
  public void getExpectedActivationDateByCategoryCodeNull() throws Exception {
    Date submissionDate = new Date();
    List<CategoryResponse> content = getCategoryResponses();
    GdnRestListResponse<CategoryResponse> gdnRestListCategoryReponse =
        new GdnRestListResponse<>(content, new PageMetaData(10, 0, 2), DEFAULT_REQUEST_ID);
    Mockito.when(calendarRepository
        .getDueDateWithMinutesDurationByWorkingWeek(DEFAULT_REQUEST_ID, null, submissionDate, 1800L, DEFAULT_USERNAME))
        .thenReturn(submissionDate);
    Date result = calendarService.getExpectedActivationDateByCategoryCode(null, submissionDate);
    Mockito.verify(calendarRepository)
        .getDueDateWithMinutesDurationByWorkingWeek(DEFAULT_REQUEST_ID, null, submissionDate, 1800L, DEFAULT_USERNAME);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(submissionDate, result);
  }

  @Test
  public void getExpectedActivationDateByCategoryCode_WithNullIntervalActivation()
      throws Exception {
    Date submissionDate = new Date();
    List<CategoryResponse> content = getCategoryResponsesWithNullActivationInterval();
    GdnRestListResponse<CategoryResponse> gdnRestListCategoryReponse =
        new GdnRestListResponse<>(content, new PageMetaData(10, 0, 2), DEFAULT_REQUEST_ID);
    Mockito.when(calendarRepository
        .getDueDateWithMinutesDurationByWorkingWeek(DEFAULT_REQUEST_ID, null, submissionDate, 1800L,
            DEFAULT_USERNAME)).thenReturn(submissionDate);
    Mockito.when(productRepository
        .filterCategoryHierarchyByCategoryCode(DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            DEFAULT_CATEGORY_CODE)).thenReturn(gdnRestListCategoryReponse);
    Date result = calendarService
        .getExpectedActivationDateByCategoryCode(DEFAULT_CATEGORY_CODE, submissionDate);
    Mockito.verify(productRepository)
        .filterCategoryHierarchyByCategoryCode(DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            DEFAULT_CATEGORY_CODE);
    Mockito.verify(calendarRepository)
        .getDueDateWithMinutesDurationByWorkingWeek(DEFAULT_REQUEST_ID, null, submissionDate, 1800L,
            DEFAULT_USERNAME);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(submissionDate, result);
  }

  @Test
  public void getExpectedActivationDateByCategoryCode_ExceptionToFetchDueDate() throws Exception {
    Date submissionDate = new Date();
    List<CategoryResponse> content = getCategoryResponsesWithNullActivationInterval();
    GdnRestListResponse<CategoryResponse> gdnRestListCategoryReponse =
        new GdnRestListResponse<>(content, new PageMetaData(10, 0, 2), DEFAULT_REQUEST_ID);
    Mockito.when(calendarRepository
        .getDueDateWithMinutesDurationByWorkingWeek(DEFAULT_REQUEST_ID, null, submissionDate, 1800L,
            DEFAULT_USERNAME)).thenThrow(new Exception());
    Mockito.when(productRepository
        .filterCategoryHierarchyByCategoryCode(DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            DEFAULT_CATEGORY_CODE)).thenReturn(gdnRestListCategoryReponse);
    Date result = calendarService
        .getExpectedActivationDateByCategoryCode(DEFAULT_CATEGORY_CODE, submissionDate);
    Mockito.verify(productRepository)
        .filterCategoryHierarchyByCategoryCode(DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            DEFAULT_CATEGORY_CODE);
    Mockito.verify(calendarRepository)
        .getDueDateWithMinutesDurationByWorkingWeek(DEFAULT_REQUEST_ID, null, submissionDate, 1800L,
            DEFAULT_USERNAME);
    Assertions.assertNull(result);
  }

  @Test
  public void getExpectedActivationDateByCategoryCode_ExceptionToFetchCategoryHierarchy()
      throws Exception {
    Mockito.when(productRepository
        .filterCategoryHierarchyByCategoryCode(DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            DEFAULT_CATEGORY_CODE)).thenThrow(new Exception());
    Date result =
        calendarService.getExpectedActivationDateByCategoryCode(DEFAULT_CATEGORY_CODE, new Date());
    Mockito.verify(productRepository)
        .filterCategoryHierarchyByCategoryCode(DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            DEFAULT_CATEGORY_CODE);
    Assertions.assertNull(result);
  }

  private List<CategoryResponse> getCategoryResponses() {
    CategoryResponse categoryResponse1 = new CategoryResponse();
    categoryResponse1.setParentCategoryId(null);
    categoryResponse1.setInternalActivationInterval(24);
    CategoryResponse categoryResponse2 = new CategoryResponse();
    categoryResponse2.setParentCategoryId(DEFAULT_ID);
    return Arrays.asList(categoryResponse1, categoryResponse2);
  }

  private List<CategoryResponse> getCategoryResponsesWithNullActivationInterval() {
    CategoryResponse categoryResponse1 = new CategoryResponse();
    categoryResponse1.setParentCategoryId(null);
    CategoryResponse categoryResponse2 = new CategoryResponse();
    categoryResponse2.setParentCategoryId(DEFAULT_ID);
    return Arrays.asList(categoryResponse1, categoryResponse2);
  }

}
