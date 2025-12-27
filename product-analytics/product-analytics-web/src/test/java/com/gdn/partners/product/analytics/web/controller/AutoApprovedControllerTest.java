package com.gdn.partners.product.analytics.web.controller;

import com.fasterxml.jackson.databind.ObjectMapper;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.gdn.partners.product.analytics.model.AutoApprovedApiPath;
import com.gdn.partners.product.analytics.service.AutoApprovedService;
import com.gdn.partners.product.analytics.service.UserFeedbackService;
import com.gdn.partners.product.analytics.web.model.AutoApprovedListWebResponse;
import com.gdn.partners.product.analytics.web.model.UserFeedbackResponse;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedAssigneeRequest;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedSelectedDownloadRequest;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedWebRequest;
import com.gdn.partners.product.analytics.web.model.request.UserFeedbackRequest;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class AutoApprovedControllerTest {
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String USER_NAME = "userName";
  private static final String STORE_ID = "storeId";
  private static final String REQUEST_ID = "requestId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String PRODUCT_CODE = "product-code";
  private MockMvc mockMvc;
  private ObjectMapper objectMapper;

  @InjectMocks
  private AutoApprovedController autoApprovedController;

  @Mock
  private AutoApprovedService autoApprovedService;

  @Mock
  private UserFeedbackService userFeedbackService;


  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = MockMvcBuilders.standaloneSetup(this.autoApprovedController).build();
    objectMapper = new ObjectMapper();
  }
  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(autoApprovedService);
  }

  @Test
  public void testFetchAutoApprovedProductsList() throws Exception {
    AutoApprovedWebRequest request = new AutoApprovedWebRequest();
    AutoApprovedListWebResponse response = new AutoApprovedListWebResponse();
    Pageable pageable = PageRequest.of(PAGE, SIZE);
    Page<AutoApprovedListWebResponse> pageResponse =
      new PageImpl<>(List.of(response), pageable, SIZE);
    Mockito.when(
      autoApprovedService.fetchListOfAutoApprovedProducts(Mockito.any(), Mockito.anyInt(),
        Mockito.anyInt())).thenReturn(pageResponse);
    this.mockMvc.perform(post(AutoApprovedApiPath.BASE_PATH
        + AutoApprovedApiPath.AUTO_APPROVED_PRODUCTS_LIST_API).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request)).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USER_NAME).param("page", String.valueOf(PAGE))
        .param("size", String.valueOf(SIZE))).andExpect(status().isOk())
      .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(true));
    Mockito.verify(autoApprovedService).fetchListOfAutoApprovedProducts(request, PAGE, SIZE);
  }

  @Test
  public void updateUserFeedbackTest() throws Exception {
    UserFeedbackRequest userFeedbackRequest = new UserFeedbackRequest();
    this.mockMvc.perform(
        post(AutoApprovedApiPath.BASE_PATH + AutoApprovedApiPath.UPSERT_USER_FEEDBACK,
          PRODUCT_CODE).contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(userFeedbackRequest)).param("storeId", STORE_ID)
          .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
          .param("username", USER_NAME)).andExpect(status().isOk())
      .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(true));
    Mockito.verify(userFeedbackService)
      .updateUserFeedbackForAutoApprovedProduct(PRODUCT_CODE, userFeedbackRequest);
  }

  @Test
  public void updateAssignedToTest() throws Exception {
    AutoApprovedAssigneeRequest assigneeRequest = new AutoApprovedAssigneeRequest();
    assigneeRequest.setProductCode(Collections.singletonList(PRODUCT_CODE));
    assigneeRequest.setAssigneeTo(USER_NAME);
    this.mockMvc.perform(
      post(AutoApprovedApiPath.BASE_PATH + AutoApprovedApiPath.UPDATE_ASSIGNEE).contentType(
          MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(assigneeRequest))
        .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USER_NAME)).andExpect(status().isOk());
    Mockito.verify(autoApprovedService).updateAssignee(assigneeRequest);
  }

  @Test
  public void fetchUserFeedbackTest() throws Exception {
    Mockito.when(userFeedbackService.fetchUserFeedbackResponse(PRODUCT_CODE)).thenReturn(new UserFeedbackResponse());
    this.mockMvc.perform(
            get(AutoApprovedApiPath.BASE_PATH + AutoApprovedApiPath.GET_USER_FEEDBACK_FOR_AUTO_APPROVED_PRODUCTS,
                PRODUCT_CODE).param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USER_NAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue())).andExpect(jsonPath("$.errorCode", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(true))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(userFeedbackService).fetchUserFeedbackResponse(PRODUCT_CODE);
  }

  @Test
  public void testFetchAutoApprovedSelectedProductsList() throws Exception {
    AutoApprovedSelectedDownloadRequest request = new AutoApprovedSelectedDownloadRequest();
    AutoApprovedListWebResponse response = new AutoApprovedListWebResponse();
    List<AutoApprovedListWebResponse> responseList =
      new ArrayList<>(Collections.singletonList(response));
    Mockito.when(autoApprovedService.fetchSelectedItemsOfAutoApprovedProducts(Mockito.any()))
      .thenReturn(responseList);
    this.mockMvc.perform(post(AutoApprovedApiPath.BASE_PATH
        + AutoApprovedApiPath.AUTO_APPROVED_PRODUCTS_SELECTED_DOWNLOAD).contentType(
          MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(request))
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USER_NAME)).andExpect(status().isOk())
      .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(true));
    Mockito.verify(autoApprovedService).fetchSelectedItemsOfAutoApprovedProducts(request);
  }
}
