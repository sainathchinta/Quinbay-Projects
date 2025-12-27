package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.integration.json.SimpleJsonSerializer.toJson;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;

import com.gdn.partners.pcu.internal.service.CategoryHistoryService;
import com.gdn.partners.pcu.internal.web.model.response.CategoryHistoryWebResponse;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.CategoryControllerPath;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.CategoryService;

@AutoConfigureMockMvc
public class CategoryControllerTest {

  private static final ClassLoader CLASS_LOADER = ClassLoader.getSystemClassLoader();
  private static final String BASE_DIRECTORY = CLASS_LOADER.getResource(StringUtils.EMPTY).getPath();
  private static final String PATH = BASE_DIRECTORY + "path";
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";
  private static final String FILE = "/filestore/originalFilename.xls";
  private static final String RESTRICTED_KEYWORD_UPSERT = "RESTRICTED_KEYWORD_UPSERT";
  private static final String CATEGORY_CODE = "CAT-001";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private byte[] fileContent;
  private MockMultipartFile multipartFile;
  private CategoryHistoryWebResponse categoryHistoryWebResponse;

  private MockMvc mockMvc;

  @Mock
  private CategoryService categoryService;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private CategoryHistoryService categoryHistoryService;

  @InjectMocks
  private CategoryController categoryController;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(this.categoryController).build();
    fileContent = new byte[]{-1, -40, -20, -10};
    categoryHistoryWebResponse = new CategoryHistoryWebResponse();
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.categoryService);
    Mockito.verifyNoMoreInteractions(this.clientParameterHelper);
    Mockito.verifyNoMoreInteractions(this.categoryHistoryService);
  }



  @Test
  public void fetchCategoryTreeWithReviewConfigTest() throws Exception {
    Mockito.when(this.categoryService.fetchCategoryTreeWithReviewConfig()).thenReturn(new ArrayList<>());
    MockHttpServletRequestBuilder requestBuilder =
        get(CategoryControllerPath.BASE_PATH + CategoryControllerPath.FETCH_CATEGORY_TREE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.categoryService).fetchCategoryTreeWithReviewConfig();
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @Test
  public void bulkRestrictedKeywordUploadTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(
            CategoryControllerPath.BASE_PATH + CategoryControllerPath.RESTRICTED_KEYWORD_BULK_UPLOAD,
            RESTRICTED_KEYWORD_UPSERT).file(multipartFile).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(categoryService)
        .saveRestrictedKeywordFile(multipartFile, RESTRICTED_KEYWORD_UPSERT, Constants.REQUEST_ID, Constants.STORE_ID,
            Constants.USER_NAME);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
  }

  @Test
  public void bulkRestrictedKeywordUpload_ExceptionTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    Mockito.doThrow(IOException.class).when(categoryService)
        .saveRestrictedKeywordFile(multipartFile, RESTRICTED_KEYWORD_UPSERT, Constants.REQUEST_ID, Constants.STORE_ID,
            Constants.USER_NAME);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(
            CategoryControllerPath.BASE_PATH + CategoryControllerPath.RESTRICTED_KEYWORD_BULK_UPLOAD,
            RESTRICTED_KEYWORD_UPSERT).file(multipartFile).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    Mockito.verify(categoryService)
        .saveRestrictedKeywordFile(multipartFile, RESTRICTED_KEYWORD_UPSERT, Constants.REQUEST_ID, Constants.STORE_ID,
            Constants.USER_NAME);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(clientParameterHelper).getStoreId();
  }

  @Test
  public void getCategoryHistoryTest() throws Exception {
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(
            this.categoryHistoryService.categoryHistory(Constants.STORE_ID, CATEGORY_CODE, PAGE,
                SIZE))
        .thenReturn(new PageImpl<>(Collections.singletonList(categoryHistoryWebResponse),
            PageRequest.of(PAGE, SIZE), SIZE));
    this.mockMvc.perform(
        get(CategoryControllerPath.BASE_PATH + CategoryControllerPath.HISTORY).contentType(
                MediaType.APPLICATION_JSON).param("categoryCode", CATEGORY_CODE)
            .param("page", toJson(PAGE)).param("size", toJson(SIZE))).andExpect(status().isOk());
    Mockito.verify(this.categoryHistoryService)
        .categoryHistory(Constants.STORE_ID, CATEGORY_CODE, PAGE, SIZE);
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getRequestId();
  }
}
