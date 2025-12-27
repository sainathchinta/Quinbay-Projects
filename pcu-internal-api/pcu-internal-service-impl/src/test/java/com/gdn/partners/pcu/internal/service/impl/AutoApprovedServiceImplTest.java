package com.gdn.partners.pcu.internal.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.internal.client.feign.ProductAnalyticsFeign;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedWebRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedUserFeedbackRequest;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedListWebResponse;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedUserFeedbackResponse;
import com.gdn.partners.pcu.internal.client.model.response.ProductAssigneeChangeResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.VendorService;
import com.gdn.partners.pcu.internal.service.impl.exception.InvalidStateException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class AutoApprovedServiceImplTest {

  private static final int PAGE = 0;
  private static final int SIZE = 10;

  private static final String REQUEST_ID = "request-id";
  private static final String PRODUCT_CODE = "product-code";
  private static final String USER_NAME = "username";
  private static final String USER_NAME_1 = "username_1";

  @InjectMocks
  private AutoApprovedServiceImpl autoApprovedService;

  @Mock
  private ProductAnalyticsFeign productAnalyticsFeign;

  @Mock
  private VendorService vendorService;

  @BeforeEach
  public void init() {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(productAnalyticsFeign);
    Mockito.verifyNoMoreInteractions(vendorService);
  }

  @Test
  public void getAutoApprovedProductsListTest() {
    AutoApprovedWebRequest request = new AutoApprovedWebRequest();
    AutoApprovedRequest autoApprovedRequest = new AutoApprovedRequest();
    GdnRestListResponse<AutoApprovedListWebResponse> serviceResponse = new GdnRestListResponse<>(
        new ArrayList<>(), new PageMetaData(SIZE, PAGE, SIZE), Constants.REQUEST_ID);
    Mockito.when(productAnalyticsFeign.fetchAutoApprovedProductsList(PAGE, SIZE, autoApprovedRequest))
        .thenReturn(serviceResponse);
    GdnRestListResponse<AutoApprovedListWebResponse> result =
        autoApprovedService.getAutoApprovedProductsList(PAGE, SIZE, request);
    verify(productAnalyticsFeign, times(1)).fetchAutoApprovedProductsList(PAGE, SIZE, autoApprovedRequest);
    assertEquals(serviceResponse, result);
  }

  @Test
  public void getAutoApprovedProductsListB2BNullTest() {
    AutoApprovedWebRequest request = new AutoApprovedWebRequest();
    request.setB2bActivated(true);
    request.setB2cActivated(true);
    AutoApprovedRequest autoApprovedRequest = new AutoApprovedRequest();
    GdnRestListResponse<AutoApprovedListWebResponse> serviceResponse = new GdnRestListResponse<>(
      new ArrayList<>(), new PageMetaData(SIZE, PAGE, SIZE), Constants.REQUEST_ID);
    Mockito.when(productAnalyticsFeign.fetchAutoApprovedProductsList(PAGE, SIZE, autoApprovedRequest))
      .thenReturn(serviceResponse);
    GdnRestListResponse<AutoApprovedListWebResponse> result =
      autoApprovedService.getAutoApprovedProductsList(PAGE, SIZE, request);
    verify(productAnalyticsFeign, times(1)).fetchAutoApprovedProductsList(PAGE, SIZE, autoApprovedRequest);
    assertEquals(serviceResponse, result);
  }

  @Test
  public void getAutoApprovedProductsListB2BTrueTest() {
    AutoApprovedWebRequest request = new AutoApprovedWebRequest();
    request.setB2bActivated(true);
    AutoApprovedRequest autoApprovedRequest = new AutoApprovedRequest();
    autoApprovedRequest.setB2bActivated(true);
    GdnRestListResponse<AutoApprovedListWebResponse> serviceResponse = new GdnRestListResponse<>(
      new ArrayList<>(), new PageMetaData(SIZE, PAGE, SIZE), Constants.REQUEST_ID);
    Mockito.when(productAnalyticsFeign.fetchAutoApprovedProductsList(PAGE, SIZE, autoApprovedRequest))
      .thenReturn(serviceResponse);
    GdnRestListResponse<AutoApprovedListWebResponse> result =
      autoApprovedService.getAutoApprovedProductsList(PAGE, SIZE, request);
    verify(productAnalyticsFeign, times(1)).fetchAutoApprovedProductsList(PAGE, SIZE, autoApprovedRequest);
    assertEquals(serviceResponse, result);
  }

  @Test
  public void getAutoApprovedProductsListB2BFalseTest() {
    AutoApprovedWebRequest request = new AutoApprovedWebRequest();
    request.setB2cActivated(true);
    AutoApprovedRequest autoApprovedRequest = new AutoApprovedRequest();
    autoApprovedRequest.setB2bActivated(false);
    GdnRestListResponse<AutoApprovedListWebResponse> serviceResponse = new GdnRestListResponse<>(
      new ArrayList<>(), new PageMetaData(SIZE, PAGE, SIZE), Constants.REQUEST_ID);
    Mockito.when(productAnalyticsFeign.fetchAutoApprovedProductsList(PAGE, SIZE, autoApprovedRequest))
      .thenReturn(serviceResponse);
    GdnRestListResponse<AutoApprovedListWebResponse> result =
      autoApprovedService.getAutoApprovedProductsList(PAGE, SIZE, request);
    verify(productAnalyticsFeign, times(1)).fetchAutoApprovedProductsList(PAGE, SIZE, autoApprovedRequest);
    assertEquals(serviceResponse, result);
  }

  @Test
  public void updateAssigneeTest() throws Exception {
    GdnRestListResponse<ProductAssigneeChangeResponse> serviceResponse = new GdnRestListResponse<>(
      new ArrayList<>(), null, Constants.REQUEST_ID);
    AutoApprovedAssigneeRequest assigneeRequest = new AutoApprovedAssigneeRequest();
    assigneeRequest.setAssigneeTo(USER_NAME);
    Mockito.when(vendorService.getProductReviewers()).thenReturn(List.of(USER_NAME));
    Mockito.when(productAnalyticsFeign.updateAssignedTo(assigneeRequest))
      .thenReturn(serviceResponse);
    GdnRestListResponse<ProductAssigneeChangeResponse> response = autoApprovedService.updateAssignee(assigneeRequest);
    Mockito.verify(productAnalyticsFeign).updateAssignedTo(assigneeRequest);
    Mockito.verify(vendorService).getProductReviewers();
    assertEquals(serviceResponse, response);
  }

  @Test
  public void updateUnAssigneeTest() throws Exception {
    GdnRestListResponse<ProductAssigneeChangeResponse> serviceResponse = new GdnRestListResponse<>(
      new ArrayList<>(), null, Constants.REQUEST_ID);
    AutoApprovedAssigneeRequest assigneeRequest = new AutoApprovedAssigneeRequest();
    Mockito.when(productAnalyticsFeign.updateAssignedTo(assigneeRequest))
      .thenReturn(serviceResponse);
    GdnRestListResponse<ProductAssigneeChangeResponse> response = autoApprovedService.updateAssignee(assigneeRequest);
    Mockito.verify(productAnalyticsFeign).updateAssignedTo(assigneeRequest);
    assertEquals(serviceResponse, response);
  }

  @Test
  public void updateAssigneeNoValidUserTest() throws Exception {
    GdnBaseRestResponse serviceResponse = new GdnBaseRestResponse(null, null, true, REQUEST_ID);
    AutoApprovedAssigneeRequest assigneeRequest = new AutoApprovedAssigneeRequest();
    assigneeRequest.setAssigneeTo(USER_NAME_1);
    Mockito.when(vendorService.getProductReviewers()).thenReturn(List.of(USER_NAME));
    try {
      autoApprovedService.updateAssignee(assigneeRequest);
    }
    catch (InvalidStateException ex){
      Assertions.assertNotNull(ex);
    }
    finally {
      Mockito.verify(vendorService).getProductReviewers();
    }
  }

  @Test
  public void updateUserFeedbackTest() {
    GdnBaseRestResponse serviceResponse = new GdnBaseRestResponse(null, null, true, REQUEST_ID);
    AutoApprovedUserFeedbackRequest autoApprovedUserFeedbackRequest = new AutoApprovedUserFeedbackRequest();
    Mockito.when(productAnalyticsFeign.updateUserFeedback(PRODUCT_CODE, autoApprovedUserFeedbackRequest))
        .thenReturn(serviceResponse);
    GdnBaseRestResponse response = autoApprovedService.updateUserFeedback(PRODUCT_CODE, autoApprovedUserFeedbackRequest);
    Mockito.verify(productAnalyticsFeign).updateUserFeedback(PRODUCT_CODE, autoApprovedUserFeedbackRequest);
    assertEquals(serviceResponse, response);
  }

  @Test
  public void fetchUserFeedbackTest() {
    AutoApprovedUserFeedbackResponse autoApprovedUserFeedbackResponse = new AutoApprovedUserFeedbackResponse();
    GdnRestSingleResponse<AutoApprovedUserFeedbackResponse> serviceResponse =
        new GdnRestSingleResponse<>(autoApprovedUserFeedbackResponse, REQUEST_ID);
    Mockito.when(productAnalyticsFeign.fetchUserFeedback(PRODUCT_CODE)).thenReturn(serviceResponse);
    GdnRestSingleResponse<AutoApprovedUserFeedbackResponse> response = autoApprovedService.fetchUserFeedback(PRODUCT_CODE);
    Mockito.verify(productAnalyticsFeign).fetchUserFeedback(PRODUCT_CODE);
    assertEquals(serviceResponse, response);
  }
}