package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.dto.request.PredictionCategoryMappingRequest;
import com.gdn.x.productcategorybase.dto.response.PredictionIdAndCategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductPredictionCategoryMappingResponse;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.PredictionCategoryMapping;
import com.gdn.x.productcategorybase.repository.PredictionCategoryMappingRepository;
import com.gdn.x.productcategorybase.service.CategoryService;

public class PredictionCategoryMappingServiceTest {

  private static final String STORE_ID = "10001";
  private static final String PREDICTION_ID = "predictionId";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String PREDICTION_ID_1 = "predictionId1";
  private static final String CATEGORY_CODE_1 = "categoryCode1";
  private static final String CATEGORY_NAME_1 = "categoryName1";

  private List<PredictionCategoryMapping> predictionCategoryMappings;

  @InjectMocks
  private PredictionCategoryMappingServiceImpl predictionCategoryMappingService;

  @Mock
  private PredictionCategoryMappingRepository predictionCategoryMappingRepository;

  @Mock
  private CategoryService categoryService;

  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);
    predictionCategoryMappings = new ArrayList<>();
    PredictionCategoryMapping predictionCategoryMapping = new PredictionCategoryMapping();
    predictionCategoryMapping.setPredictionId(PREDICTION_ID);
    PredictionCategoryMapping predictionCategoryMapping1 = new PredictionCategoryMapping();
    predictionCategoryMapping1.setPredictionId(PREDICTION_ID_1);
    predictionCategoryMappings.add(predictionCategoryMapping);
    predictionCategoryMappings.add(predictionCategoryMapping1);
  }

  @Test
  public void upsertPredictionCategoryMappingTest() throws Exception {
    List<PredictionCategoryMapping> predictionCategoryMappingList = new ArrayList<>();
    PredictionCategoryMapping predictionCategoryMapping = new PredictionCategoryMapping();
    predictionCategoryMapping.setPredictionId(PREDICTION_ID);
    predictionCategoryMapping.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMapping.setMarkForDelete(false);
    predictionCategoryMappingList.add(predictionCategoryMapping);

    Mockito.when(predictionCategoryMappingRepository.findByStoreIdAndPredictionIdAndCategoryCodeIn(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList())).thenReturn(predictionCategoryMappingList);

    List<PredictionCategoryMappingRequest> requestList = new ArrayList<>();
    PredictionCategoryMappingRequest predictionCategoryMappingRequest1 = new PredictionCategoryMappingRequest();
    predictionCategoryMappingRequest1.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingRequest1.setPredictionId(PREDICTION_ID);
    predictionCategoryMappingRequest1.setMarkForDelete(true);
    requestList.add(predictionCategoryMappingRequest1);
    PredictionCategoryMappingRequest predictionCategoryMappingRequest2 = new PredictionCategoryMappingRequest();
    predictionCategoryMappingRequest2.setCategoryCode(CATEGORY_CODE_1);
    predictionCategoryMappingRequest2.setPredictionId(PREDICTION_ID);
    predictionCategoryMappingRequest2.setMarkForDelete(false);
    requestList.add(predictionCategoryMappingRequest2);

    this.predictionCategoryMappingService.upsertPredictionCategoryMapping(STORE_ID, requestList);
    Mockito.verify(predictionCategoryMappingRepository)
        .findByStoreIdAndPredictionIdAndCategoryCodeIn(Mockito.anyString(), Mockito.anyString(), Mockito.anyList());
    Mockito.verify(predictionCategoryMappingRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void upsertPredictionCategoryMappingFalseConditionTest() throws Exception {
    List<PredictionCategoryMapping> predictionCategoryMappingList = new ArrayList<>();
    PredictionCategoryMapping predictionCategoryMapping = new PredictionCategoryMapping();
    predictionCategoryMapping.setPredictionId(PREDICTION_ID);
    predictionCategoryMapping.setCategoryCode(CATEGORY_CODE_1);
    predictionCategoryMapping.setMarkForDelete(false);
    predictionCategoryMappingList.add(predictionCategoryMapping);

    Mockito.when(predictionCategoryMappingRepository.findByStoreIdAndPredictionIdAndCategoryCodeIn(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList())).thenReturn(predictionCategoryMappingList);

    List<PredictionCategoryMappingRequest> requestList = new ArrayList<>();
    PredictionCategoryMappingRequest predictionCategoryMappingRequest1 = new PredictionCategoryMappingRequest();
    predictionCategoryMappingRequest1.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingRequest1.setPredictionId(PREDICTION_ID);
    predictionCategoryMappingRequest1.setMarkForDelete(true);
    requestList.add(predictionCategoryMappingRequest1);
    PredictionCategoryMappingRequest predictionCategoryMappingRequest2 = new PredictionCategoryMappingRequest();
    predictionCategoryMappingRequest2.setCategoryCode(CATEGORY_CODE_1);
    predictionCategoryMappingRequest2.setPredictionId(PREDICTION_ID);
    predictionCategoryMappingRequest2.setMarkForDelete(false);
    requestList.add(predictionCategoryMappingRequest2);

    this.predictionCategoryMappingService.upsertPredictionCategoryMapping(STORE_ID, requestList);
    Mockito.verify(predictionCategoryMappingRepository)
        .findByStoreIdAndPredictionIdAndCategoryCodeIn(Mockito.anyString(), Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void upsertPredictionCategoryMappingUpdateStoreIdEmptyTest() throws Exception {
    List<PredictionCategoryMappingRequest> requestList = new ArrayList<>();
    PredictionCategoryMappingRequest predictionCategoryMappingRequest = new PredictionCategoryMappingRequest();
    predictionCategoryMappingRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingRequest.setPredictionId(PREDICTION_ID);
    predictionCategoryMappingRequest.setMarkForDelete(false);
    requestList.add(predictionCategoryMappingRequest);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.predictionCategoryMappingService.upsertPredictionCategoryMapping(StringUtils.EMPTY, requestList));
  }

  @Test
  public void upsertPredictionCategoryMappingUpdateMultiPredictionIdTest() throws Exception {
    List<PredictionCategoryMappingRequest> requestList = new ArrayList<>();
    PredictionCategoryMappingRequest predictionCategoryMappingRequest = new PredictionCategoryMappingRequest();
    predictionCategoryMappingRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingRequest.setPredictionId(PREDICTION_ID);
    predictionCategoryMappingRequest.setMarkForDelete(false);
    requestList.add(predictionCategoryMappingRequest);
    PredictionCategoryMappingRequest predictionCategoryMappingRequest1 = new PredictionCategoryMappingRequest();
    predictionCategoryMappingRequest1.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingRequest1.setPredictionId(PREDICTION_ID_1);
    predictionCategoryMappingRequest1.setMarkForDelete(false);
    requestList.add(predictionCategoryMappingRequest1);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.predictionCategoryMappingService.upsertPredictionCategoryMapping(STORE_ID, requestList));
  }

  @Test
  public void upsertPredictionCategoryMappingUpdatePredictionIdEmptyTest() throws Exception {
    List<PredictionCategoryMappingRequest> requestList = new ArrayList<>();
    PredictionCategoryMappingRequest predictionCategoryMappingRequest = new PredictionCategoryMappingRequest();
    predictionCategoryMappingRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingRequest.setPredictionId(StringUtils.EMPTY);
    predictionCategoryMappingRequest.setMarkForDelete(false);
    requestList.add(predictionCategoryMappingRequest);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.predictionCategoryMappingService.upsertPredictionCategoryMapping(STORE_ID, requestList));
  }

  @Test
  public void upsertPredictionCategoryMappingUpdateCateogryCodeEmptyTest() throws Exception {
    List<PredictionCategoryMappingRequest> requestList = new ArrayList<>();
    PredictionCategoryMappingRequest predictionCategoryMappingRequest = new PredictionCategoryMappingRequest();
    predictionCategoryMappingRequest.setCategoryCode(StringUtils.EMPTY);
    predictionCategoryMappingRequest.setPredictionId(PREDICTION_ID);
    predictionCategoryMappingRequest.setMarkForDelete(false);
    requestList.add(predictionCategoryMappingRequest);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.predictionCategoryMappingService.upsertPredictionCategoryMapping(STORE_ID, requestList));
  }

  @Test
  public void getPredictionIdAndCategoryCodeResponseTest() throws Exception {
    List<String> predictionIdList = new ArrayList<>();
    predictionIdList.add(PREDICTION_ID);
    predictionIdList.add(PREDICTION_ID_1);

    List<PredictionCategoryMapping> predictionCategoryMappingList = new ArrayList<>();
    PredictionCategoryMapping predictionCategoryMapping = new PredictionCategoryMapping();
    predictionCategoryMapping.setPredictionId(PREDICTION_ID);
    predictionCategoryMapping.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingList.add(predictionCategoryMapping);
    PredictionCategoryMapping predictionCategoryMapping1 = new PredictionCategoryMapping();
    predictionCategoryMapping1.setPredictionId(PREDICTION_ID);
    predictionCategoryMapping1.setCategoryCode(CATEGORY_CODE_1);
    predictionCategoryMappingList.add(predictionCategoryMapping1);
    PredictionCategoryMapping predictionCategoryMapping2 = new PredictionCategoryMapping();
    predictionCategoryMapping2.setPredictionId(PREDICTION_ID_1);
    predictionCategoryMapping2.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingList.add(predictionCategoryMapping2);

    Mockito.when(predictionCategoryMappingRepository.findByStoreIdAndPredictionIdInAndMarkForDeleteFalse(STORE_ID,
        predictionIdList)).thenReturn(predictionCategoryMappingList);

    List<Category> categoryList = new ArrayList<>();
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    category.setName(CATEGORY_NAME);
    categoryList.add(category);
    Category category1 = new Category();
    category1.setCategoryCode(CATEGORY_CODE_1);
    category1.setName(CATEGORY_NAME_1);
    categoryList.add(category1);
    Mockito.when(categoryService.findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(categoryList);

    List<PredictionIdAndCategoryCodeResponse> predictionIdAndCategoryCodeResponseList =
        this.predictionCategoryMappingService.getPredictionIdAndCategoryCodeResponse(STORE_ID, predictionIdList);

    Assertions.assertEquals(PREDICTION_ID_1, predictionIdAndCategoryCodeResponseList.get(0).getPredictionId());
    Assertions.assertEquals(CATEGORY_CODE,
        predictionIdAndCategoryCodeResponseList.get(0).getCategoryCodeAndNameResponseList().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME,
        predictionIdAndCategoryCodeResponseList.get(0).getCategoryCodeAndNameResponseList().get(0).getCategoryName());
    Assertions.assertEquals(PREDICTION_ID, predictionIdAndCategoryCodeResponseList.get(1).getPredictionId());
    Assertions.assertEquals(CATEGORY_CODE,
        predictionIdAndCategoryCodeResponseList.get(1).getCategoryCodeAndNameResponseList().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME,
        predictionIdAndCategoryCodeResponseList.get(1).getCategoryCodeAndNameResponseList().get(0).getCategoryName());
    Assertions.assertEquals(CATEGORY_CODE_1,
        predictionIdAndCategoryCodeResponseList.get(1).getCategoryCodeAndNameResponseList().get(1).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME_1,
        predictionIdAndCategoryCodeResponseList.get(1).getCategoryCodeAndNameResponseList().get(1).getCategoryName());
  }

  @Test
  public void getPredictionIdAndCategoryCodeResponseNoPredictionIdFoundErrorTest() throws Exception {
    List<String> predictionIdList = new ArrayList<>();
    predictionIdList.add(PREDICTION_ID);
    predictionIdList.add(PREDICTION_ID_1);

    List<PredictionCategoryMapping> predictionCategoryMappingList = new ArrayList<>();
    PredictionCategoryMapping predictionCategoryMapping = new PredictionCategoryMapping();
    predictionCategoryMapping.setPredictionId(PREDICTION_ID);
    predictionCategoryMapping.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingList.add(predictionCategoryMapping);
    PredictionCategoryMapping predictionCategoryMapping1 = new PredictionCategoryMapping();
    predictionCategoryMapping1.setPredictionId(PREDICTION_ID);
    predictionCategoryMapping1.setCategoryCode(CATEGORY_CODE_1);
    predictionCategoryMappingList.add(predictionCategoryMapping1);
    PredictionCategoryMapping predictionCategoryMapping2 = new PredictionCategoryMapping();
    predictionCategoryMapping2.setPredictionId(PREDICTION_ID_1);
    predictionCategoryMapping2.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingList.add(predictionCategoryMapping2);

    Mockito.when(predictionCategoryMappingRepository.findByStoreIdAndPredictionIdInAndMarkForDeleteFalse(STORE_ID,
        predictionIdList)).thenReturn(new ArrayList<>());

    List<Category> categoryList = new ArrayList<>();
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    category.setName(CATEGORY_NAME);
    categoryList.add(category);
    Category category1 = new Category();
    category1.setCategoryCode(CATEGORY_CODE_1);
    category1.setName(CATEGORY_NAME_1);
    categoryList.add(category1);
    Mockito.when(categoryService.findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(categoryList);

    List<PredictionIdAndCategoryCodeResponse> predictionIdAndCategoryCodeResponseList =
        this.predictionCategoryMappingService.getPredictionIdAndCategoryCodeResponse(STORE_ID, predictionIdList);

  }

  @Test
  public void getPredictionIdAndCategoryCodeResponseStoreIdBlankTest() throws Exception {
    List<String> predictionIdList = new ArrayList<>();
    predictionIdList.add(PREDICTION_ID);
    predictionIdList.add(PREDICTION_ID_1);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        this.predictionCategoryMappingService.getPredictionIdAndCategoryCodeResponse(StringUtils.EMPTY,
            predictionIdList));
  }

  @Test
  public void getPredictionIdAndCategoryCodeResponsepredictionIdListEmptyTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        this.predictionCategoryMappingService.getPredictionIdAndCategoryCodeResponse(STORE_ID,
            new ArrayList<>()));
  }

  @Test
  public void getPredictionIdAndCategoryCodeResponsePredictionIdEmptyTest() throws Exception {
    List<String> predictionIdList = new ArrayList<>();
    predictionIdList.add(PREDICTION_ID);
    predictionIdList.add("");
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        this.predictionCategoryMappingService.getPredictionIdAndCategoryCodeResponse(STORE_ID,
            predictionIdList));
  }

  @Test
  public void getPredictionListByCategoryCodeNullTest() {
    List<ProductPredictionCategoryMappingResponse> predictionListByCategoryCode =
        predictionCategoryMappingService.getPredictionListByCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(predictionCategoryMappingRepository)
        .findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
    Assertions.assertTrue(CollectionUtils.isEmpty(predictionListByCategoryCode));
  }

  @Test
  public void getPredictionListByCategoryCodeTest() {
    Mockito.when(
        predictionCategoryMappingRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(predictionCategoryMappings);
    List<ProductPredictionCategoryMappingResponse> predictionListByCategoryCode =
        predictionCategoryMappingService.getPredictionListByCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(predictionCategoryMappingRepository)
        .findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
    Assertions.assertFalse(CollectionUtils.isEmpty(predictionListByCategoryCode));
    Assertions.assertEquals(PREDICTION_ID, predictionListByCategoryCode.get(0).getPredictionId());
    Assertions.assertEquals(PREDICTION_ID_1, predictionListByCategoryCode.get(1).getPredictionId());
  }
}
