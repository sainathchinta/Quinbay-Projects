package com.gdn.partners.pcu.internal.service.impl;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.BulkRestrictedKeywordUploadModel;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.pcu.internal.client.constants.ClientParameter;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.x.productcategorybase.dto.response.CategoryParentResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeNodeResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;

/**
 * Created by govind on 19/12/2018 AD.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class CategoryServiceImplTest {

  private static final String REQUEST_ID = "requestId";
  private static final String CATEGORY_CODE = "category-code";
  private static final String CATEGORY_ID = "category-id";
  private static final String PARENT_CATEGORY_ID = "parent_category-id";
  private static final String FINAL_CATEGORY_ID = "final-category-id";
  private static final String CATEGORY_NAME = "cat_name";
  private static final int PAGE = 0;
  private static final int SIZE = 30;
  private static final Long TOTAL_RECORD = Long.valueOf(1);
  private static final ClassLoader CLASS_LOADER = ClassLoader.getSystemClassLoader();
  private static final String BASE_DIRECTORY = CLASS_LOADER.getResource(StringUtils.EMPTY).getPath();
  private static final String PATH = BASE_DIRECTORY + "path";
  private static final String FILE = "/filestore/originalFilename.xls";
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";
  private static final String TYPE = "RESTRICTED_KEYWORD_UPSERT";
  private static final String STORE_ID = "STORE_ID";
  private static final String USERNAME = "username";
  private byte[] fileContent;
  private MockMultipartFile multipartFile;

  private CategoryTreeNodeResponse categoryTreeNode = new CategoryTreeNodeResponse();
  private List<CategoryTreeNodeResponse> categoryTreeNodeList = new ArrayList<>();
  private GdnRestListResponse<CategoryTreeNodeResponse> categoryTreeNodeGdnRestListResponse;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private FileStorageServiceImpl fileStorageService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Captor
  private ArgumentCaptor<BulkRestrictedKeywordUploadModel> bulkRestrictedKeywordUploadModelArgumentCaptor;

  @Mock
  private ClientParameter clientParameter;

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private ContextRefreshedEvent contextRefreshedEvent;

  @InjectMocks
  private CategoryServiceImpl categoryService;

  private GdnRestListResponse<CategoryResponse> gdnRestListResponse;

  private CategoryResponse categoryResponse;
  private HashMap<String, String> categoryFinalMap;

  @BeforeEach
  public void setUp() throws Exception {
    gdnRestListResponse = new GdnRestListResponse<CategoryResponse>();
    gdnRestListResponse.setSuccess(Boolean.TRUE);
    List<CategoryResponse> categoryResponseList = new ArrayList<>();
    categoryResponse = new CategoryResponse();
    categoryResponseList.add(categoryResponse);
    categoryResponse.setInternalActivationInterval(24);
    gdnRestListResponse.setContent(categoryResponseList);
    ReflectionTestUtils.setField(categoryService, "internalActivationPeriod", 72);
    categoryFinalMap = new HashMap<>();
    categoryFinalMap.put(CATEGORY_ID, CATEGORY_CODE);
    ReflectionTestUtils.setField(categoryService, "categoryToFinalParentMap", categoryFinalMap);

    categoryTreeNode.setName(CATEGORY_NAME);
    categoryTreeNode.setCode(CATEGORY_CODE);
    categoryTreeNode.setReview(false);
    categoryTreeNodeList.add(categoryTreeNode);
    categoryTreeNodeGdnRestListResponse =
        new GdnRestListResponse<>(null, null, true, categoryTreeNodeList, new PageMetaData(PAGE, SIZE, TOTAL_RECORD),
            REQUEST_ID);
    fileContent = new byte[] {-1, -40, -20, -10};
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(pcbFeign);
  }


  @Test
  public void findInternalActivationIntervalInDaysByCategoryCodeTest() throws Exception {
    when(pcbFeign.filterCategoryHierarchyByCategoryCode(CATEGORY_CODE)).thenReturn(
        gdnRestListResponse);
    String value = categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
    Mockito.verify(pcbFeign).filterCategoryHierarchyByCategoryCode(CATEGORY_CODE);
    Assertions.assertNotNull(value);
    Assertions.assertEquals("1", value);
  }

  @Test
  public void findInternalActivationIntervalInDaysByCategoryCodeTest_withDefault() throws Exception {
    categoryResponse.setInternalActivationInterval(null);
    when(pcbFeign.filterCategoryHierarchyByCategoryCode(CATEGORY_CODE)).thenReturn(
        gdnRestListResponse);
    String value = categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
    Mockito.verify(pcbFeign).filterCategoryHierarchyByCategoryCode(Mockito.eq(CATEGORY_CODE));
    Assertions.assertNotNull(value);
    Assertions.assertEquals("3", value);
  }

  @Test
  public void findInternalActivationIntervalInDaysByCategoryCode_whenResponseNullTest()
      throws Exception {
    when(pcbFeign.filterCategoryHierarchyByCategoryCode(CATEGORY_CODE)).thenReturn(null);
    try {
      categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
    } catch (ClientException ex) {
      Mockito.verify(pcbFeign).filterCategoryHierarchyByCategoryCode(Mockito.eq(CATEGORY_CODE));
    }
  }

  @Test
  public void contextRefreshedTest() {
    when(contextRefreshedEvent.getApplicationContext()).thenReturn(applicationContext);
    when(applicationContext.getBean(ClientParameter.class)).thenReturn(clientParameter);
    when(applicationContext.getBean(PCBFeign.class)).thenReturn(pcbFeign);
    when(pcbFeign.getCategoriesAndFinalCategoryMapping()).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(new CategoryParentResponse(CATEGORY_ID, PARENT_CATEGORY_ID)),
        new PageMetaData(), REQUEST_ID));
    categoryService.onApplicationEvent();
    verify(pcbFeign).getCategoriesAndFinalCategoryMapping();
  }

  @Test
  public void contextRefreshedClientExceptionTest() {
    when(contextRefreshedEvent.getApplicationContext()).thenReturn(applicationContext);
    when(applicationContext.getBean(PCBFeign.class)).thenReturn(pcbFeign);
    when(pcbFeign.getCategoriesAndFinalCategoryMapping()).thenReturn(null);
    try {
      categoryService.onApplicationEvent();
    }
    catch (ClientException e) {}finally {
      verify(pcbFeign).getCategoriesAndFinalCategoryMapping();
    }
  }

  @Test
  public void getCategoryToFinalParentMapTest_mapIsNullPostDeployment() {
    ReflectionTestUtils.setField(categoryService, "categoryToFinalParentMap", null);
    when(pcbFeign.getCategoriesAndFinalCategoryMapping()).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(new CategoryParentResponse(CATEGORY_ID, PARENT_CATEGORY_ID)),
            new PageMetaData(), REQUEST_ID));
    Map<String, String> response = categoryService.getCategoryToFinalParentMap();
    verify(pcbFeign).getCategoriesAndFinalCategoryMapping();
    Assertions.assertEquals(PARENT_CATEGORY_ID, response.get(CATEGORY_ID));
  }

  @Test
  public void getCategoryToFinalParentMapTest_mapIsNotNullPostDeployment() {
    Map<String, String> response = categoryService.getCategoryToFinalParentMap();
    Assertions.assertEquals(CATEGORY_CODE, response.get(CATEGORY_ID));
  }

  @Test
  public void getFinalParentCategoryAndUpdateMapTest() {
    when(pcbFeign.getFinalParentCategory(CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(new SingleObjectResponse(FINAL_CATEGORY_ID), REQUEST_ID));
    String response = categoryService.getFinalParentCategoryAndUpdateMap(CATEGORY_ID);
    verify(pcbFeign).getFinalParentCategory(CATEGORY_ID);
    Assertions.assertEquals(FINAL_CATEGORY_ID, response);
  }

  @Test
  public void getFinalParentCategoryAndUpdateMapClientExceptionTest() {
    when(pcbFeign.getFinalParentCategory(CATEGORY_ID)).thenReturn(null);
    try {
      categoryService.getFinalParentCategoryAndUpdateMap(CATEGORY_ID);
    } catch (ClientException e) {
    } finally {
      verify(pcbFeign).getFinalParentCategory(CATEGORY_ID);
    }
  }

  @Test
  public void fetchCategoryTreeWithReviewConfigTest() {
    Mockito.when(this.pcbFeign.getCategoryTreeWithReviewConfig()).thenReturn(categoryTreeNodeGdnRestListResponse);
    List<CategoryTreeNodeResponse> response = this.categoryService.fetchCategoryTreeWithReviewConfig();
    Mockito.verify(this.pcbFeign).getCategoryTreeWithReviewConfig();
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getCode());
    Assertions.assertEquals(CATEGORY_NAME, response.get(0).getName());
    Assertions.assertFalse(response.get(0).isReview());
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @Test
  public void saveRestrictedKeywordFileTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    Mockito.when(fileStorageService.uploadFilePath(Mockito.any(), anyString(), anyString()))
        .thenReturn(PATH);
    categoryService.saveRestrictedKeywordFile(multipartFile, TYPE, REQUEST_ID, STORE_ID, USERNAME);
    Mockito.verify(fileStorageService).uploadFilePath(Mockito.any(), anyString(), anyString());
    verify(kafkaProducer).send(eq(DomainEventName.BULK_RESTRICTED_KEYWORD_UPLOAD_EVENT), anyString(),
        bulkRestrictedKeywordUploadModelArgumentCaptor.capture());
    Assertions.assertNotNull(new File(PATH + REQUEST_ID + ORIGINAL_FILENAME));
    Assertions.assertNotNull(bulkRestrictedKeywordUploadModelArgumentCaptor.getValue());
    Assertions.assertEquals(TYPE, bulkRestrictedKeywordUploadModelArgumentCaptor.getValue().getActionType());
    Assertions.assertEquals(REQUEST_ID, bulkRestrictedKeywordUploadModelArgumentCaptor.getValue().getRequestId());
    Assertions.assertEquals(STORE_ID, bulkRestrictedKeywordUploadModelArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(USERNAME, bulkRestrictedKeywordUploadModelArgumentCaptor.getValue().getUpdatedBy());
  }
}
