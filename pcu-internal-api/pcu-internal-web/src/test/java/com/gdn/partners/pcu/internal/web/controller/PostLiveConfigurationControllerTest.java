package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.PostLiveConfigurationControllerPath;
import com.gdn.partners.pcu.internal.service.PostLiveConfigurationService;
import com.gdn.partners.pcu.internal.web.TestApplication;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.CategoryConfigurationWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ConfigurationFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ConfigurationWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.CategoryConfigurationFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.CategoryConfigurationHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantConfigurationFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantConfigurationHistoryWebResponse;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.response.ConfigurationCountResponse;


@AutoConfigureMockMvc
public class PostLiveConfigurationControllerTest extends TestHelper {

  private final static String KEYWORD = "keyword";
  private static final String CATEGORY_CODE = "CAT-01";
  private static final String SORT_ORDER = "desc";
  private static final int PAGE = 0;
  private static final int SIZE = 30;
  private static final String REVIEW_CONFIG = "reviewConfig";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BL-00001";
  private static final String FILE = "/filestore/originalFilename.xls";
  private static final String PATH = "path";
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";
  private static final String USER_NAME = "username";
  private static final String TYPE_PARAMETER = "type";
  private static final int HISTORY_SIZE = 25;
  private static final long TOTAL_RECORDS = 0;

  private byte[] fileContent;
  private CategoryConfigurationRequest categoryConfigurationRequest;
  private static final Long MERCHANT_CONFIGURATION_COUNT = Long.valueOf(10);
  private static final Long CATEGORY_CONFIGURATION_COUNT = Long.valueOf(20);

  private MockMultipartFile multipartFile;
  private CategoryConfigurationWebRequest categoryConfigurationWebRequest;
  private List<CategoryConfigurationWebRequest> categoryConfigurationWebRequestList;
  private ConfigurationWebRequest configurationWebRequest;

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private ConfigurationFilterWebRequest configurationFilterWebRequest =
      new ConfigurationFilterWebRequest();
  private Pageable pageable;
  private List<MerchantConfigurationHistoryWebResponse> merchantConfigurationHistoryWebResponseList = new ArrayList<>();
  private List<MerchantConfigurationFilterWebResponse> merchantConfigurationFilterWebResponseList = new ArrayList<>();
  private List<CategoryConfigurationHistoryWebResponse> categoryConfigurationHistoryWebResponseList = new ArrayList<>();
  private List<CategoryConfigurationFilterWebResponse> categoryConfigurationFilterWebResponseList = new ArrayList<>();

  @Mock
  private PostLiveConfigurationService postLiveConfigurationService;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @InjectMocks
  private PostLiveConfigurationController postLiveConfigurationController;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(this.postLiveConfigurationController).build();
    configurationFilterWebRequest.setCategoryCode(CATEGORY_CODE);
    configurationFilterWebRequest.setReviewConfig(Constants.PRE_LIVE_STATUS);
    configurationFilterWebRequest.setSearchKey(KEYWORD);
    configurationFilterWebRequest.setSortOrder(SORT_ORDER);

    categoryConfigurationRequest = new CategoryConfigurationRequest();
    categoryConfigurationRequest.setCategoryCode(CATEGORY_CODE);
    categoryConfigurationRequest.setReviewConfig(REVIEW_CONFIG);

    categoryConfigurationWebRequest = new CategoryConfigurationWebRequest();
    categoryConfigurationWebRequest.setCategoryCode(CATEGORY_CODE);
    categoryConfigurationWebRequest.setReviewConfig(REVIEW_CONFIG);
    categoryConfigurationWebRequestList = new ArrayList<>();
    categoryConfigurationWebRequestList.add(categoryConfigurationWebRequest);
    configurationWebRequest =
        ConfigurationWebRequest.builder().merchantCode(DEFAULT_BUSINESS_PARTNER_CODE).categoryCode(CATEGORY_CODE)
            .build();

    fileContent = new byte[]{-1, -40, -20, -10};
    pageable = PageRequest.of(PAGE, SIZE);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(postLiveConfigurationService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }


  @Test
  public void getMerchantsBySearchKeywordTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService.getMerchantsBySearchKeyword(KEYWORD, PAGE, SIZE))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(PAGE, SIZE), SIZE));
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.MERCHANT_SEARCH,
            KEYWORD);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService).getMerchantsBySearchKeyword(KEYWORD, PAGE, SIZE);
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void addCategoryConfigurationTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doNothing().when(this.postLiveConfigurationService)
        .addCategoryConfiguration(Collections.singletonList(new CategoryConfigurationRequest()));
    MockHttpServletRequestBuilder requestBuilder = post(
        PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.ADD_CATEGORY_CONFIGURATION)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(Collections.singletonList(new CategoryConfigurationWebRequest())));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService).addCategoryConfiguration(Collections.singletonList(new CategoryConfigurationRequest()));
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void updateCategoryConfigurationTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doNothing().when(this.postLiveConfigurationService)
        .updateCategoryConfiguration(CATEGORY_CODE, REVIEW_CONFIG);
    MockHttpServletRequestBuilder requestBuilder = put(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.UPDATE_CATEGORY_CONFIGURATION, CATEGORY_CODE)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(categoryConfigurationWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService).updateCategoryConfiguration(CATEGORY_CODE, REVIEW_CONFIG);
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void deleteCategoryConfigurationTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doNothing().when(this.postLiveConfigurationService).deleteCategoryConfiguration(CATEGORY_CODE);
    MockHttpServletRequestBuilder requestBuilder = delete(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.DELETE_CATEGORY_CONFIGURATION, CATEGORY_CODE)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService).deleteCategoryConfiguration(CATEGORY_CODE);
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void addMerchantConfigurationTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doNothing().when(this.postLiveConfigurationService)
        .addMerchantConfiguration(Collections.singletonList(new MerchantConfigurationRequest()));
    MockHttpServletRequestBuilder requestBuilder = post(
        PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.ADD_MERCHANT_CONFIGURATION)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(Collections.singletonList(new CategoryConfigurationWebRequest())));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService)
        .addMerchantConfiguration(Collections.singletonList(new MerchantConfigurationRequest()));
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void updateMerchantConfigurationTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doNothing().when(this.postLiveConfigurationService)
        .updateMerchantConfiguration(DEFAULT_BUSINESS_PARTNER_CODE, REVIEW_CONFIG);
    MockHttpServletRequestBuilder requestBuilder = put(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.UPDATE_MERCHANT_CONFIGURATION, DEFAULT_BUSINESS_PARTNER_CODE)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(categoryConfigurationWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService)
        .updateMerchantConfiguration(DEFAULT_BUSINESS_PARTNER_CODE, REVIEW_CONFIG);
    Mockito.verify(this.clientParameterHelper).getRequestId(); }

  @Test
  public void deleteMerchantConfigurationTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doNothing().when(this.postLiveConfigurationService)
        .deleteMerchantConfiguration(DEFAULT_BUSINESS_PARTNER_CODE);
    MockHttpServletRequestBuilder requestBuilder = delete(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.DELETE_MERCHANT_CONFIGURATION, DEFAULT_BUSINESS_PARTNER_CODE)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService).deleteMerchantConfiguration(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.clientParameterHelper).getRequestId(); }

  @Test
  public void getConfigurationStatusTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(
        this.postLiveConfigurationService.getConfigurationsStatus(Collections.singletonList(configurationWebRequest)))
        .thenReturn(new ArrayList<>());
    MockHttpServletRequestBuilder requestBuilder =
        post(PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.CONFIGURATIONS_STATUS)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(Collections.singletonList(configurationWebRequest)));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService)
        .getConfigurationsStatus(Collections.singletonList(configurationWebRequest));
    Mockito.verify(this.clientParameterHelper).getRequestId(); }
  @Test
  public void getConfigurationCountsTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService.getConfigurationCounts())
        .thenReturn(new ConfigurationCountResponse(CATEGORY_CONFIGURATION_COUNT, MERCHANT_CONFIGURATION_COUNT));
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.CONFIGURATION_COUNT);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService).getConfigurationCounts();
    Mockito.verify(this.clientParameterHelper).getRequestId(); }
  @Test
  public void fetchCategoryConfigurationListTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService
        .filterCategoryConfiguration(configurationFilterWebRequest, PAGE, SIZE))
        .thenReturn(new PageImpl<>(categoryConfigurationFilterWebResponseList, pageable, TOTAL_RECORDS));
    String requestString = PostLiveConfigurationControllerTest.OBJECT_MAPPER
        .writeValueAsString(configurationFilterWebRequest);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.CATEGORY_LISTING)
            .contentType(MediaType.APPLICATION_JSON).content(requestString).param("page", toJson(PAGE))
            .param("size", toJson(SIZE));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService)
        .filterCategoryConfiguration(configurationFilterWebRequest, PAGE, SIZE);
    Mockito.verify(this.clientParameterHelper).getRequestId(); }
  @Test
  public void fetchMerchantConfigurationListTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService
        .filterMerchantConfiguration(configurationFilterWebRequest, PAGE, SIZE))
        .thenReturn(new PageImpl<>(merchantConfigurationFilterWebResponseList, pageable, TOTAL_RECORDS));
    String requestString = PostLiveConfigurationControllerTest.OBJECT_MAPPER
        .writeValueAsString(configurationFilterWebRequest);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.MERCHANT_LISTING)
            .contentType(MediaType.APPLICATION_JSON).content(requestString).param("page", toJson(PAGE))
            .param("size", toJson(SIZE));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService)
        .filterMerchantConfiguration(configurationFilterWebRequest, PAGE, SIZE);
    Mockito.verify(this.clientParameterHelper).getRequestId(); }
  @Test
  public void fetchCategoryConfigurationHistoryListTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService.getCategoryConfigurationHistory(CATEGORY_CODE, PAGE, SIZE))
        .thenReturn(new PageImpl<>(categoryConfigurationHistoryWebResponseList, pageable, TOTAL_RECORDS));
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder = get(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.CATEGORY_CONFIGURATION_HISTORY, CATEGORY_CODE).param("page", toJson(PAGE))
        .param("size", toJson(SIZE));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService)
        .getCategoryConfigurationHistory(CATEGORY_CODE, PAGE, SIZE);
    Mockito.verify(this.clientParameterHelper).getRequestId(); }
  @Test
  public void bulkConfigurationUpdateTest() throws Exception {
    String ACTION_TYPE = "Merchant";
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(
        PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.BULK_CONFIGURATION_UPLOAD)
        .file(multipartFile).accept(MediaType.APPLICATION_JSON).param(TYPE_PARAMETER, ACTION_TYPE))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(postLiveConfigurationService)
        .uploadBulkConfiguration(multipartFile, ACTION_TYPE, Constants.REQUEST_ID, Constants.STORE_ID, USER_NAME);
    Mockito.verify(this.clientParameterHelper).getRequestId();
    Mockito.verify(this.clientParameterHelper).getStoreId();
    Mockito.verify(this.clientParameterHelper).getUsername();
  }

  @Test
  public void bulkConfigurationUpdate_IOExceptionTest() throws Exception {
    String ACTION_TYPE = "Merchant";
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    Mockito.doThrow(IOException.class).when(postLiveConfigurationService)
        .uploadBulkConfiguration(multipartFile, ACTION_TYPE, Constants.REQUEST_ID, Constants.STORE_ID, USER_NAME);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(
        PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.BULK_CONFIGURATION_UPLOAD)
        .file(multipartFile).accept(MediaType.APPLICATION_JSON).param(TYPE_PARAMETER, ACTION_TYPE))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    Mockito.verify(postLiveConfigurationService)
        .uploadBulkConfiguration(multipartFile, ACTION_TYPE, Constants.REQUEST_ID, Constants.STORE_ID, USER_NAME);
    Mockito.verify(this.clientParameterHelper).getRequestId();
    Mockito.verify(this.clientParameterHelper).getStoreId();
    Mockito.verify(this.clientParameterHelper).getUsername();  }

  @Test
  public void bulkConfigurationUpdate_ExceptionTest() throws Exception {
    String ACTION_TYPE = "RaNdOm";
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.multipart(
          PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.BULK_CONFIGURATION_UPLOAD)
          .file(multipartFile).accept(MediaType.APPLICATION_JSON).param(TYPE_PARAMETER, ACTION_TYPE))
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    } catch (Exception e) {
      Mockito.verify(this.clientParameterHelper).getRequestId();
    }
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @Test
  public void getMerchantConfigurationHistoryTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService
        .getMerchantConfigurationHistory(DEFAULT_BUSINESS_PARTNER_CODE, PAGE, HISTORY_SIZE))
        .thenReturn(new PageImpl<>(merchantConfigurationHistoryWebResponseList, pageable, TOTAL_RECORDS));
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.MERCHANT_HISTORY,
            DEFAULT_BUSINESS_PARTNER_CODE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService)
        .getMerchantConfigurationHistory(DEFAULT_BUSINESS_PARTNER_CODE, PAGE, HISTORY_SIZE);
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void downloadBulkConfigurationTest() throws Exception {
    String ACTION_TYPE = "Merchant";
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.doNothing().when(postLiveConfigurationService)
        .downloadBulkConfiguration(configurationFilterWebRequest, ACTION_TYPE, Constants.STORE_ID, Constants.USER_NAME);
    MockHttpServletRequestBuilder requestBuilder = post(
        PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.BULK_CONFIGURATION_DOWNLOAD)
        .param(TYPE_PARAMETER, ACTION_TYPE).content(toJson(configurationFilterWebRequest))
        .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService)
        .downloadBulkConfiguration(configurationFilterWebRequest, ACTION_TYPE, Constants.STORE_ID, Constants.USER_NAME);
    Mockito.verify(this.clientParameterHelper).getRequestId();
    Mockito.verify(this.clientParameterHelper).getStoreId();
    Mockito.verify(this.clientParameterHelper).getUsername();
  }

  @Test
  public void downloadBulkConfigurationExecptionTest() throws Exception {
    String ACTION_TYPE = "RaNdOm";
    MockHttpServletRequestBuilder requestBuilder = post(
        PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.BULK_CONFIGURATION_DOWNLOAD)
        .param(TYPE_PARAMETER, ACTION_TYPE).content(toJson(configurationFilterWebRequest))
        .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    try {
      mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    } catch (Exception e) {
    }
  }
}