package com.gdn.x.productcategorybase.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.http.client.utils.URIBuilder;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.RestrictedKeywordApiPath;
import com.gdn.x.productcategorybase.dto.RestrictedKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsSearchRequest;
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsListingResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.UiValidationRestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.helper.RestrictedKeywordServiceHelper;
import com.gdn.x.productcategorybase.service.RestrictedKeywordHistoryService;
import com.gdn.x.productcategorybase.service.RestrictedKeywordService;
import com.gdn.x.productcategorybase.service.RestrictedKeywordServiceWrapper;

public class RestrictedKeywordControllerTest {

  private static final String STORE_ID = "10001";
  private static final String KEYWORD = "keyword";
  private static final String KEYWORD_ID = "keywordId";
  private static final String NAME = "name";
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String SEARCH_RESTRICTED_KEYWORDS =
    "/api/restricted-keywords/search";
  private static final String UPDATE_RESTRICTED_KEYWORDS =
          "/api/restricted-keywords/update-restricted-keywords";
  private static final String REQUEST_ID = "REQ-001";
  private static final Boolean VALIDATE_ON_UI = true;
  private static final Boolean VALIDATE_BY_DS = false;

  private static final String ACTIVITY = "activity";
  private static final String OLD_VALUE = "oldValue";
  private static final String NEW_VALUE = "newValue";
  private static final String CREATED_BY = "createdBy";
  private static final PageRequest pageRequest = PageRequest.of(0, 10);
  private static RestrictedKeywordHistoryResponse restrictedKeywordHistoryResponse;
  private static RestrictedKeywordsListingResponse restrictedKeywordsListingResponse;

  @InjectMocks
  private RestrictedKeywordController controller;

  @Mock
  private RestrictedKeywordHistoryService service;

  @Mock
  private RestrictedKeywordServiceWrapper restrictedKeywordServiceWrapper;

  @Mock
  private RestrictedKeywordService restrictedKeywordService;

  private ObjectMapper objectMapper;

  private MockMvc mockMvc;

  private final Pageable pageable = PageRequest.of(0, 10);
  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private Page<RestrictedKeywordsResponse> restrictedKeywordsResponsePage;
  private List<RestrictedKeywordsResponse> restrictedKeywordsResponseList;
  private RestrictedKeywordsResponse restrictedKeywordsResponse;
  private RestrictedKeywordsUpdateRequest restrictedKeywordsUpdateRequest;

  @Mock
  private RestrictedKeywordServiceHelper restrictedKeywordServiceHelper;

  @InjectMocks
  private RestrictedKeywordController restrictedKeywordController;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    restrictedKeywordsResponse = new RestrictedKeywordsResponse();
    restrictedKeywordsResponse.setSelected(Boolean.TRUE);
    restrictedKeywordsResponse.setKeywordId(KEYWORD_ID);
    restrictedKeywordsResponse.setKeyword(KEYWORD);
    restrictedKeywordsResponseList = new ArrayList<>();
    restrictedKeywordsResponseList.add(restrictedKeywordsResponse);
    restrictedKeywordsResponsePage =
      new PageImpl<>(restrictedKeywordsResponseList, this.pageable, 1000L);
    this.mockMvc = standaloneSetup(this.restrictedKeywordController).build();
    this.mockMvc = standaloneSetup(this.controller).build();
    restrictedKeywordHistoryResponse = new RestrictedKeywordHistoryResponse();
    restrictedKeywordHistoryResponse.setActivity(ACTIVITY);
    restrictedKeywordHistoryResponse.setOldValue(OLD_VALUE);
    restrictedKeywordHistoryResponse.setNewValue(NEW_VALUE);
    restrictedKeywordHistoryResponse.setCreatedBy(CREATED_BY);

    restrictedKeywordsUpdateRequest = new RestrictedKeywordsUpdateRequest();
    restrictedKeywordsUpdateRequest.setKeywordId(KEYWORD_ID);
    restrictedKeywordsUpdateRequest.setKeyword(KEYWORD);
    restrictedKeywordsUpdateRequest.setValidateOnUi(VALIDATE_ON_UI);
    restrictedKeywordsUpdateRequest.setValidateByDs(VALIDATE_BY_DS);

    restrictedKeywordsListingResponse = new RestrictedKeywordsListingResponse();
    restrictedKeywordsListingResponse.setKeywordId(KEYWORD_ID);
    restrictedKeywordsListingResponse.setKeyword(KEYWORD);
    restrictedKeywordsListingResponse.setValidateOnUi(true);
    restrictedKeywordsListingResponse.setValidateByDs(true);
    objectMapper = new ObjectMapper();
  }


  @Test
  public void getRestrictedKeywordsTest() throws Exception {
    RestrictedKeywordsSearchRequest request = new RestrictedKeywordsSearchRequest();
    request.setKeyword(KEYWORD);
    String requestBody = this.objectMapper.writeValueAsString(request);
    Mockito.when(restrictedKeywordServiceHelper.getRestrictedKeywordsResponses(STORE_ID, KEYWORD, pageable))
        .thenReturn(restrictedKeywordsResponsePage);
    URI uri = new URIBuilder().setPath(
            RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.SEARCH_RESTRICTED_KEYWORDS)
        .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID).addParameter("clientId", CLIENT_ID)
        .addParameter("requestId", REQUEST_ID).addParameter("username", DEFAULT_USERNAME).addParameter("page", "0")
        .addParameter("size", "10").build();
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(restrictedKeywordServiceHelper)
        .getRestrictedKeywordsResponses(eq(STORE_ID), anyString(), any(Pageable.class));
  }

  @Test
  public void getRestrictedKeywordsExceptionTest() throws Exception {
    RestrictedKeywordsSearchRequest request = new RestrictedKeywordsSearchRequest();
    request.setKeyword(KEYWORD);
    String requestBody = this.objectMapper.writeValueAsString(request);
    Mockito.when(
        restrictedKeywordServiceHelper.getRestrictedKeywordsResponses(STORE_ID, KEYWORD, pageable))
      .thenThrow(RuntimeException.class);
    URI uri = new URIBuilder().setPath(
            RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.SEARCH_RESTRICTED_KEYWORDS)
        .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID).addParameter("clientId", CLIENT_ID)
        .addParameter("requestId", REQUEST_ID).addParameter("username", DEFAULT_USERNAME).addParameter("page", "0")
        .addParameter("size", "10").build();
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(restrictedKeywordServiceHelper)
      .getRestrictedKeywordsResponses(eq(STORE_ID), anyString(), any(Pageable.class));
  }

  @Test
  public void getRestrictedKeywordsApplicationExceptionTest() throws Exception {
    RestrictedKeywordsSearchRequest request = new RestrictedKeywordsSearchRequest();
    request.setKeyword(KEYWORD);
    String requestBody = this.objectMapper.writeValueAsString(request);
    Mockito.when(
        restrictedKeywordServiceHelper.getRestrictedKeywordsResponses(STORE_ID, KEYWORD, pageable))
      .thenThrow(ApplicationRuntimeException.class);
    URI uri = new URIBuilder().setPath(
            RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.SEARCH_RESTRICTED_KEYWORDS)
        .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID).addParameter("clientId", CLIENT_ID)
        .addParameter("requestId", REQUEST_ID).addParameter("username", DEFAULT_USERNAME).addParameter("page", "0")
        .addParameter("size", "10").build();
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(restrictedKeywordServiceHelper)
      .getRestrictedKeywordsResponses(eq(STORE_ID), anyString(), any(Pageable.class));
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.restrictedKeywordServiceWrapper);
    verifyNoMoreInteractions(this.restrictedKeywordServiceHelper);
    verifyNoMoreInteractions(this.restrictedKeywordService);
    verifyNoMoreInteractions(this.service);
  }

  @Test
  public void getRestrictedKeywordHistoryTest() throws Exception {
    Page<RestrictedKeywordHistoryResponse> restrictedKeywordHistoryResponsePage =
      new PageImpl<>(Arrays.asList(restrictedKeywordHistoryResponse), pageRequest, 1);
    Mockito.when(service.getRestrictedKeywordHistory(STORE_ID, KEYWORD_ID, pageRequest))
      .thenReturn(restrictedKeywordHistoryResponsePage);
    this.mockMvc.perform(
        get(RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.HISTORY, KEYWORD_ID).accept(
            MediaType.APPLICATION_JSON).param("storeId", RestrictedKeywordControllerTest.STORE_ID)
          .param("requestId", RestrictedKeywordControllerTest.REQUEST_ID)
          .param("username", RestrictedKeywordControllerTest.DEFAULT_USERNAME))
      .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)))
      .andExpect(jsonPath("$.content[0].activity", equalTo(ACTIVITY)))
      .andExpect(jsonPath("$.content[0].oldValue", equalTo(OLD_VALUE)))
      .andExpect(jsonPath("$.content[0].newValue", equalTo(NEW_VALUE)))
      .andExpect(jsonPath("$.content[0].createdBy", equalTo(CREATED_BY)))
      .andExpect(jsonPath("$.requestId", equalTo(RestrictedKeywordControllerTest.REQUEST_ID)))
      .andReturn();
    Mockito.verify(service).getRestrictedKeywordHistory(STORE_ID, KEYWORD_ID, pageRequest);
  }

  @Test
  public void getRestrictedKeywordHistoryApplicationRuntimeExceptionTest() throws Exception {
    Mockito.when(service.getRestrictedKeywordHistory(STORE_ID, KEYWORD_ID, pageRequest))
        .thenThrow(new ApplicationRuntimeException());
    this.mockMvc.perform(get(RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.HISTORY, KEYWORD_ID).accept(
                MediaType.APPLICATION_JSON).param("storeId", RestrictedKeywordControllerTest.STORE_ID)
            .param("requestId", RestrictedKeywordControllerTest.REQUEST_ID)
            .param("username", RestrictedKeywordControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)))
        .andExpect(jsonPath("$.requestId", equalTo(RestrictedKeywordControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(service).getRestrictedKeywordHistory(STORE_ID, KEYWORD_ID, pageRequest);
  }

  @Test
  public void getRestrictedKeywordHistoryExceptionTest() throws Exception {
    Mockito.when(service.getRestrictedKeywordHistory(STORE_ID, KEYWORD_ID, pageRequest))
        .thenThrow(new NullPointerException());
    this.mockMvc.perform(get(RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.HISTORY, KEYWORD_ID).accept(
                MediaType.APPLICATION_JSON).param("storeId", RestrictedKeywordControllerTest.STORE_ID)
            .param("requestId", RestrictedKeywordControllerTest.REQUEST_ID)
            .param("username", RestrictedKeywordControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)))
        .andExpect(jsonPath("$.requestId", equalTo(RestrictedKeywordControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(service).getRestrictedKeywordHistory(STORE_ID, KEYWORD_ID, pageRequest);
  }

  @Test
  public void getRestrictedKeywordForListingTest() throws Exception {
    Page<RestrictedKeywordsListingResponse> restrictedKeywordsListingResponses =
        new PageImpl<>(Arrays.asList(restrictedKeywordsListingResponse), pageRequest, 1);
    Mockito.when(this.restrictedKeywordService.getRestrictedKeywordForListing(STORE_ID, KEYWORD, pageable))
        .thenReturn(restrictedKeywordsListingResponses);
    this.mockMvc.perform(
            get(RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.LISTING).accept(MediaType.APPLICATION_JSON)
                .param("storeId", RestrictedKeywordControllerTest.STORE_ID)
                .param("requestId", RestrictedKeywordControllerTest.REQUEST_ID)
                .param("username", RestrictedKeywordControllerTest.DEFAULT_USERNAME).param("keyword", KEYWORD))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)))
        .andExpect(jsonPath("$.content[0].keywordId", equalTo(KEYWORD_ID)))
        .andExpect(jsonPath("$.content[0].keyword", equalTo(KEYWORD)))
        .andExpect(jsonPath("$.content[0].validateOnUi", equalTo(Boolean.TRUE)))
        .andExpect(jsonPath("$.content[0].validateByDs", equalTo(Boolean.TRUE)))
        .andExpect(jsonPath("$.requestId", equalTo(RestrictedKeywordControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(this.restrictedKeywordService).getRestrictedKeywordForListing(STORE_ID, KEYWORD, pageable);
  }

  @Test
  public void getRestrictedKeywordForListingApplicationRuntimeExceptionTest() throws Exception {
    Page<RestrictedKeywordsListingResponse> restrictedKeywordsListingResponses =
        new PageImpl<>(Arrays.asList(restrictedKeywordsListingResponse), pageRequest, 1);
    Mockito.when(this.restrictedKeywordService.getRestrictedKeywordForListing(STORE_ID, KEYWORD, pageable))
        .thenThrow(new ApplicationRuntimeException());
    this.mockMvc.perform(
            get(RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.LISTING).accept(MediaType.APPLICATION_JSON)
                .param("storeId", RestrictedKeywordControllerTest.STORE_ID)
                .param("requestId", RestrictedKeywordControllerTest.REQUEST_ID)
                .param("username", RestrictedKeywordControllerTest.DEFAULT_USERNAME).param("keyword", KEYWORD))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)))
        .andExpect(jsonPath("$.requestId", equalTo(RestrictedKeywordControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(this.restrictedKeywordService).getRestrictedKeywordForListing(STORE_ID, KEYWORD, pageable);
  }

  @Test
  public void getRestrictedKeywordForListingExceptionTest() throws Exception {
    Page<RestrictedKeywordsListingResponse> restrictedKeywordsListingResponses =
        new PageImpl<>(Arrays.asList(restrictedKeywordsListingResponse), pageRequest, 1);
    Mockito.when(this.restrictedKeywordService.getRestrictedKeywordForListing(STORE_ID, KEYWORD, pageable))
        .thenThrow(new Exception());
    this.mockMvc.perform(
            get(RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.LISTING).accept(MediaType.APPLICATION_JSON)
                .param("storeId", RestrictedKeywordControllerTest.STORE_ID)
                .param("requestId", RestrictedKeywordControllerTest.REQUEST_ID)
                .param("username", RestrictedKeywordControllerTest.DEFAULT_USERNAME).param("keyword", KEYWORD))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)))
        .andExpect(jsonPath("$.requestId", equalTo(RestrictedKeywordControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(this.restrictedKeywordService).getRestrictedKeywordForListing(STORE_ID, KEYWORD, pageable);
  }

  @Test
  public void updateRestrictedKeywordTest() throws Exception {
    String request = RestrictedKeywordControllerTest.OBJECT_MAPPER.writeValueAsString(restrictedKeywordsUpdateRequest);
    doNothing().when(restrictedKeywordServiceWrapper)
        .updateRestrictedKeyword(any(RestrictedKeywordsUpdateDTO.class), anyString());
    this.mockMvc.perform(
            put(RestrictedKeywordControllerTest.UPDATE_RESTRICTED_KEYWORDS).contentType(MediaType.APPLICATION_JSON_VALUE)
                .param("storeId", RestrictedKeywordControllerTest.STORE_ID)
                .param("requestId", RestrictedKeywordControllerTest.REQUEST_ID)
                .param("username", RestrictedKeywordControllerTest.DEFAULT_USERNAME).content(request))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", CoreMatchers.equalTo(true)));

    Mockito.verify(restrictedKeywordServiceWrapper)
        .updateRestrictedKeyword(any(RestrictedKeywordsUpdateDTO.class), anyString());
  }

  @Test
  public void updateRestrictedKeywordExceptionTest() throws Exception {
    String request = RestrictedKeywordControllerTest.OBJECT_MAPPER.writeValueAsString(restrictedKeywordsUpdateRequest);
    doThrow(new Exception()).when(restrictedKeywordServiceWrapper)
        .updateRestrictedKeyword(any(RestrictedKeywordsUpdateDTO.class), anyString());
    this.mockMvc.perform(
            put(RestrictedKeywordControllerTest.UPDATE_RESTRICTED_KEYWORDS).contentType(MediaType.APPLICATION_JSON_VALUE)
                .param("storeId", RestrictedKeywordControllerTest.STORE_ID)
                .param("requestId", RestrictedKeywordControllerTest.REQUEST_ID)
                .param("username", RestrictedKeywordControllerTest.DEFAULT_USERNAME).content(request))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)));
    Mockito.verify(restrictedKeywordServiceWrapper)
        .updateRestrictedKeyword(any(RestrictedKeywordsUpdateDTO.class), anyString());
  }

  @Test
  public void updateRestrictedKeywordApplicationExceptionTest() throws Exception {
    String request = RestrictedKeywordControllerTest.OBJECT_MAPPER.writeValueAsString(restrictedKeywordsUpdateRequest);
    doThrow(new ApplicationRuntimeException()).when(restrictedKeywordServiceWrapper)
        .updateRestrictedKeyword(any(RestrictedKeywordsUpdateDTO.class), anyString());
    this.mockMvc.perform(
            put(RestrictedKeywordControllerTest.UPDATE_RESTRICTED_KEYWORDS).contentType(MediaType.APPLICATION_JSON_VALUE)
                .param("storeId", RestrictedKeywordControllerTest.STORE_ID)
                .param("requestId", RestrictedKeywordControllerTest.REQUEST_ID)
                .param("username", RestrictedKeywordControllerTest.DEFAULT_USERNAME).content(request))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)));
    Mockito.verify(restrictedKeywordServiceWrapper)
        .updateRestrictedKeyword(any(RestrictedKeywordsUpdateDTO.class), anyString());
  }

  @Test
  public void getRestrictedKeywordForUiValidationListing() throws Exception {
    List<UiValidationRestrictedKeywordsResponse> restrictedKeywordsResponses = new ArrayList<>();
    UiValidationRestrictedKeywordsResponse uiValidationRestrictedKeywordsResponse = new UiValidationRestrictedKeywordsResponse();
    uiValidationRestrictedKeywordsResponse.setKeyword(KEYWORD);
    uiValidationRestrictedKeywordsResponse.setKeywordId(KEYWORD_ID);
    restrictedKeywordsResponses.add(uiValidationRestrictedKeywordsResponse);
    Mockito.when(this.restrictedKeywordService.getListOfRestrictedKeywordsForUiValidation(STORE_ID))
        .thenReturn(restrictedKeywordsResponses);
    this.mockMvc.perform(get(RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.UI_VALIDATION_LIST).accept(
                MediaType.APPLICATION_JSON).param("storeId", RestrictedKeywordControllerTest.STORE_ID)
            .param("requestId", RestrictedKeywordControllerTest.REQUEST_ID)
            .param("username", RestrictedKeywordControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)))
        .andExpect(jsonPath("$.requestId", equalTo(RestrictedKeywordControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(this.restrictedKeywordService).getListOfRestrictedKeywordsForUiValidation(STORE_ID);
  }

  @Test
  public void getRestrictedKeywordForUiValidationListingApplicationRuntimeExceptionTest() throws Exception {
    Mockito.when(this.restrictedKeywordService.getListOfRestrictedKeywordsForUiValidation(STORE_ID))
        .thenThrow(new ApplicationRuntimeException());
    this.mockMvc.perform(
            get(RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.UI_VALIDATION_LIST).accept(MediaType.APPLICATION_JSON)
                .param("storeId", RestrictedKeywordControllerTest.STORE_ID)
                .param("requestId", RestrictedKeywordControllerTest.REQUEST_ID)
                .param("username", RestrictedKeywordControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)))
        .andExpect(jsonPath("$.requestId", equalTo(RestrictedKeywordControllerTest.REQUEST_ID))).andReturn();
    Mockito.verify(this.restrictedKeywordService).getListOfRestrictedKeywordsForUiValidation(STORE_ID);
  }

}
