package com.gdn.mta.bulk.service;

import static org.mockito.MockitoAnnotations.initMocks;

import java.math.BigInteger;
import java.util.ArrayList;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import com.gdn.mta.bulk.dto.RecatProductSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProductSummaryResponse;
import com.gdn.mta.bulk.entity.ProductRecatStatus;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductRecatStatusCustomRepository;
import com.gdn.mta.bulk.repository.ProductRecatStatusRepository;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.RecatConstants;

public class ProductRecatStatusServiceBeanTest {

  private static final String STORE_ID = "10001";
  private static final String RECAT_REQUEST_CODE = "request-code";
  private List<Object[]> productCounts;
  private static final String ID = "ID";
  private static final String PRODUCT_CODE = "productCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final int BATCH_SIZE = 100;
  private static final String PRODUCT_NAME = "productName";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String NEW_CATEGORY_CODE = "newCategoryCode";
  private static final String NEW_CATEGORY_NAME = "newCategoryName";
  private static final String ERROR_MESSAGE =
      "Can not process invalid input data :Can not process invalid input data :Category is not leaf category: 15-1000001";

  private ProductRecatStatus productRecatStatus;
  private List<Object[]> statusCountList;


  @InjectMocks
  private ProductRecatStatusServiceBean productRecatStatusServiceBean;

  @Mock
  private ProductRecatStatusRepository productRecatStatusRepository;

  @Mock
  private ProductRecatStatusCustomRepository productRecatStatusCustomRepository;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Captor
  private ArgumentCaptor<ProductRecatStatus> productRecatStatusArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);

    productRecatStatus = new ProductRecatStatus();
    productRecatStatus.setRecatRequestCode(RECAT_REQUEST_CODE);
    productRecatStatus.setProductCode(PRODUCT_CODE);
    productRecatStatus.setNewCategoryCode(CATEGORY_CODE);

    Object[] statusCount = new Object[5];
    statusCount[0] = BigInteger.valueOf(5);
    statusCount[1] = BigInteger.valueOf(10);
    statusCount[2] = BigInteger.valueOf(100);
    statusCount[3] = BigInteger.valueOf(4);
    statusCount[4] = BigInteger.valueOf(6);

    statusCountList = new ArrayList<>();
    statusCountList.add(statusCount);

    productRecatStatus.setProductName(PRODUCT_NAME);
    productRecatStatus.setCategoryCode(CATEGORY_CODE);
    productRecatStatus.setCategoryName(CATEGORY_NAME);
    productRecatStatus.setNewCategoryCode(NEW_CATEGORY_CODE);
    productRecatStatus.setNewCategoryName(NEW_CATEGORY_NAME);
    productRecatStatus.setUpdatedDate(new SimpleDateFormat("yyyy-MM-dd").parse("2021-01-01"));

    Object[] publishedProducts = {RecatConstants.PUBLISHED, new BigInteger("10")};
    Object[] pendingProducts = {RecatConstants.PENDING,  new BigInteger("2")};
    Object[] completedProducts = {RecatConstants.FINISHED,  new BigInteger("10")};
    Object[] failedProducts = {RecatConstants.FAILED,  new BigInteger("10")};
    Object[] newProducts = {RecatConstants.NEW,  new BigInteger("10")};
    productCounts = Arrays.asList(publishedProducts, pendingProducts, completedProducts, failedProducts);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productRecatStatusRepository);
    Mockito.verifyNoMoreInteractions(productBusinessPartnerRepository);
  }

  @Test
  public void saveProductRecatStatusListTest() {
    productRecatStatusServiceBean.saveProductRecatStatusList(new ArrayList<>());
    Mockito.verify(productRecatStatusRepository).saveAll(new ArrayList<>());
  }

  @Test
  public void getProductCountByRecatRequestCodeTest() {
    Mockito.when(productRecatStatusRepository.countProductsByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(productCounts);
    Map<String, Integer> productCountMap =
        productRecatStatusServiceBean.getProductCountByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(productRecatStatusRepository).countProductsByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Assertions.assertEquals(10, productCountMap.get(RecatConstants.PUBLISHED).longValue());
    Assertions.assertEquals(2, productCountMap.get(RecatConstants.PENDING).longValue());
    Assertions.assertEquals(10, productCountMap.get(RecatConstants.FINISHED).longValue());
    Assertions.assertEquals(10, productCountMap.get(RecatConstants.FAILED).longValue());
  }

  @Test
  public void findProductRecatStatusByStoreIdAndAndStatusTest() {
    productRecatStatusServiceBean
        .findProductRecatStatusByStoreIdAndAndStatus(STORE_ID, RecatConstants.PENDING, BATCH_SIZE);
    Mockito.verify(productRecatStatusRepository)
        .getProductRecatStatusByStoreIdAndStatus(STORE_ID, RecatConstants.PENDING, PageRequest.of(0, BATCH_SIZE));
  }

  @Test
  public void getRecatProductSummaryTest() {
    productRecatStatus.setStatus(RecatConstants.PUBLISHED);
    Mockito.when(productRecatStatusCustomRepository
        .findProductRecatStatusByStatusFilterAndKeyword(STORE_ID, RECAT_REQUEST_CODE, RecatConstants.PENDING,
            StringUtils.EMPTY, PageRequest.of(0, 1))).thenReturn(new PageImpl<>(Arrays.asList(productRecatStatus)));
    Page<RecatProductSummaryResponse> recatProductSummaryResponsePage = productRecatStatusServiceBean
        .getRecatProductSummary(STORE_ID, RECAT_REQUEST_CODE,
            new RecatProductSummaryRequest(RecatConstants.PENDING, StringUtils.EMPTY), 0, 1);
    Mockito.verify(productRecatStatusCustomRepository)
        .findProductRecatStatusByStatusFilterAndKeyword(STORE_ID, RECAT_REQUEST_CODE, RecatConstants.PENDING,
            StringUtils.EMPTY, PageRequest.of(0, 1));
    Assertions.assertEquals(RECAT_REQUEST_CODE, recatProductSummaryResponsePage.getContent().get(0).getRecatRequestCode());
    Assertions.assertEquals(PRODUCT_CODE, recatProductSummaryResponsePage.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, recatProductSummaryResponsePage.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, recatProductSummaryResponsePage.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, recatProductSummaryResponsePage.getContent().get(0).getCategoryName());
    Assertions.assertEquals(NEW_CATEGORY_CODE, recatProductSummaryResponsePage.getContent().get(0).getNewCategoryCode());
    Assertions.assertEquals(NEW_CATEGORY_NAME, recatProductSummaryResponsePage.getContent().get(0).getNewCategoryName());
    Assertions.assertEquals(RecatConstants.PENDING, recatProductSummaryResponsePage.getContent().get(0).getStatus());
    Assertions.assertEquals(RecatConstants.IN_PROGRESS_NOTE, recatProductSummaryResponsePage.getContent().get(0).getNotes());
  }

  @Test
  public void getRecatProductSummaryPendingTest() {
    productRecatStatus.setStatus(RecatConstants.PENDING);
    Mockito.when(productRecatStatusCustomRepository
        .findProductRecatStatusByStatusFilterAndKeyword(STORE_ID, RECAT_REQUEST_CODE, RecatConstants.PENDING,
            StringUtils.EMPTY, PageRequest.of(0, 1))).thenReturn(new PageImpl<>(Arrays.asList(productRecatStatus)));
    Page<RecatProductSummaryResponse> recatProductSummaryResponsePage = productRecatStatusServiceBean
        .getRecatProductSummary(STORE_ID, RECAT_REQUEST_CODE,
            new RecatProductSummaryRequest(RecatConstants.PENDING, StringUtils.EMPTY), 0, 1);
    Mockito.verify(productRecatStatusCustomRepository)
        .findProductRecatStatusByStatusFilterAndKeyword(STORE_ID, RECAT_REQUEST_CODE, RecatConstants.PENDING,
            StringUtils.EMPTY, PageRequest.of(0, 1));
    Assertions.assertEquals(RECAT_REQUEST_CODE, recatProductSummaryResponsePage.getContent().get(0).getRecatRequestCode());
    Assertions.assertEquals(PRODUCT_CODE, recatProductSummaryResponsePage.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, recatProductSummaryResponsePage.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, recatProductSummaryResponsePage.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, recatProductSummaryResponsePage.getContent().get(0).getCategoryName());
    Assertions.assertEquals(NEW_CATEGORY_CODE, recatProductSummaryResponsePage.getContent().get(0).getNewCategoryCode());
    Assertions.assertEquals(NEW_CATEGORY_NAME, recatProductSummaryResponsePage.getContent().get(0).getNewCategoryName());
    Assertions.assertEquals(RecatConstants.PENDING, recatProductSummaryResponsePage.getContent().get(0).getStatus());
    Assertions.assertEquals(RecatConstants.IN_PROGRESS_NOTE, recatProductSummaryResponsePage.getContent().get(0).getNotes());
  }

  @Test
  public void getRecatProductSummaryCompletedTest() {
    productRecatStatus.setStatus(RecatConstants.FINISHED);
    Mockito.when(productRecatStatusCustomRepository
        .findProductRecatStatusByStatusFilterAndKeyword(STORE_ID, RECAT_REQUEST_CODE, RecatConstants.FINISHED,
            StringUtils.EMPTY, PageRequest.of(0, 1))).thenReturn(new PageImpl<>(Arrays.asList(productRecatStatus)));
    Page<RecatProductSummaryResponse> recatProductSummaryResponsePage = productRecatStatusServiceBean
        .getRecatProductSummary(STORE_ID, RECAT_REQUEST_CODE,
            new RecatProductSummaryRequest(RecatConstants.FINISHED, StringUtils.EMPTY), 0, 1);
    Mockito.verify(productRecatStatusCustomRepository)
        .findProductRecatStatusByStatusFilterAndKeyword(STORE_ID, RECAT_REQUEST_CODE, RecatConstants.FINISHED,
            StringUtils.EMPTY, PageRequest.of(0, 1));
    Assertions.assertEquals(RECAT_REQUEST_CODE, recatProductSummaryResponsePage.getContent().get(0).getRecatRequestCode());
    Assertions.assertEquals(PRODUCT_CODE, recatProductSummaryResponsePage.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, recatProductSummaryResponsePage.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, recatProductSummaryResponsePage.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, recatProductSummaryResponsePage.getContent().get(0).getCategoryName());
    Assertions.assertEquals(NEW_CATEGORY_CODE, recatProductSummaryResponsePage.getContent().get(0).getNewCategoryCode());
    Assertions.assertEquals(NEW_CATEGORY_NAME, recatProductSummaryResponsePage.getContent().get(0).getNewCategoryName());
    Assertions.assertEquals(RecatConstants.FINISHED, recatProductSummaryResponsePage.getContent().get(0).getStatus());
    Assertions.assertEquals(RecatConstants.SUCCEED_NOTE, recatProductSummaryResponsePage.getContent().get(0).getNotes());
  }

  @Test
  public void getRecatProductSummaryFailedTest() {
    productRecatStatus.setStatus(RecatConstants.FAILED);
    productRecatStatus.setErrorMessage(RecatConstants.FILE_IS_EMPTY);
    Mockito.when(productRecatStatusCustomRepository
        .findProductRecatStatusByStatusFilterAndKeyword(STORE_ID, RECAT_REQUEST_CODE, RecatConstants.FAILED,
            StringUtils.EMPTY, PageRequest.of(0, 1))).thenReturn(new PageImpl<>(Arrays.asList(productRecatStatus)));
    Page<RecatProductSummaryResponse> recatProductSummaryResponsePage = productRecatStatusServiceBean
        .getRecatProductSummary(STORE_ID, RECAT_REQUEST_CODE,
            new RecatProductSummaryRequest(RecatConstants.FAILED, StringUtils.EMPTY), 0, 1);
    Mockito.verify(productRecatStatusCustomRepository)
        .findProductRecatStatusByStatusFilterAndKeyword(STORE_ID, RECAT_REQUEST_CODE, RecatConstants.FAILED,
            StringUtils.EMPTY, PageRequest.of(0, 1));
    Assertions.assertEquals(RECAT_REQUEST_CODE, recatProductSummaryResponsePage.getContent().get(0).getRecatRequestCode());
    Assertions.assertEquals(PRODUCT_CODE, recatProductSummaryResponsePage.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, recatProductSummaryResponsePage.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, recatProductSummaryResponsePage.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, recatProductSummaryResponsePage.getContent().get(0).getCategoryName());
    Assertions.assertEquals(NEW_CATEGORY_CODE, recatProductSummaryResponsePage.getContent().get(0).getNewCategoryCode());
    Assertions.assertEquals(NEW_CATEGORY_NAME, recatProductSummaryResponsePage.getContent().get(0).getNewCategoryName());
    Assertions.assertEquals(RecatConstants.FAILED, recatProductSummaryResponsePage.getContent().get(0).getStatus());
    Assertions.assertEquals(RecatConstants.FILE_IS_EMPTY, recatProductSummaryResponsePage.getContent().get(0).getNotes());
  }

  @Test
  public void getRecatProductSummaryEmptyTest() {
    Mockito.when(productRecatStatusCustomRepository
        .findProductRecatStatusByStatusFilterAndKeyword(STORE_ID, RECAT_REQUEST_CODE, RecatConstants.PENDING,
            StringUtils.EMPTY, PageRequest.of(0, 1))).thenReturn(new PageImpl<>(new ArrayList<>()));
    Page<RecatProductSummaryResponse> recatProductSummaryResponsePage = productRecatStatusServiceBean
        .getRecatProductSummary(STORE_ID, RECAT_REQUEST_CODE,
            new RecatProductSummaryRequest(RecatConstants.PENDING, StringUtils.EMPTY), 0, 1);
    Mockito.verify(productRecatStatusCustomRepository)
        .findProductRecatStatusByStatusFilterAndKeyword(STORE_ID, RECAT_REQUEST_CODE, RecatConstants.PENDING,
            StringUtils.EMPTY, PageRequest.of(0, 1));
    Assertions.assertTrue(recatProductSummaryResponsePage.getContent().isEmpty());
  }

  @Test
  public void findByIdTest() {
    productRecatStatusServiceBean.findByIdAndStatus(ID, RecatConstants.PUBLISHED);
    Mockito.verify(productRecatStatusRepository).findByIdAndStatus(ID, RecatConstants.PUBLISHED);
  }

  @Test
  public void updateProductCategoryTest() {
    productRecatStatusServiceBean.updateProductCategory(productRecatStatus);
    Mockito.verify(productBusinessPartnerRepository).updateProductCategory(PRODUCT_CODE, NEW_CATEGORY_CODE, productRecatStatus.getCreatedBy());
  }

  @Test
  public void validateResponseAndSaveTest() {
    productRecatStatusServiceBean.validateResponseAndSave(productRecatStatus, StringUtils.EMPTY);
    Mockito.verify(productRecatStatusRepository).save(productRecatStatusArgumentCaptor.capture());
    ProductRecatStatus value = productRecatStatusArgumentCaptor.getValue();
    Assertions.assertEquals(RecatConstants.FINISHED, value.getStatus());
    Assertions.assertTrue(StringUtils.isBlank(value.getErrorMessage()));
    Assertions.assertFalse(value.isSystemError());
    Assertions.assertFalse(value.isValidationError());
  }

  @Test
  public void validateResponseAndSaveSystemErrorTest() {
    productRecatStatusServiceBean.validateResponseAndSave(productRecatStatus, Constant.SYSTEM_ERROR);
    Mockito.verify(productRecatStatusRepository).save(productRecatStatusArgumentCaptor.capture());
    ProductRecatStatus value = productRecatStatusArgumentCaptor.getValue();
    Assertions.assertEquals(RecatConstants.FAILED, value.getStatus());
    Assertions.assertEquals(Constant.SYSTEM_ERROR, value.getErrorMessage());
    Assertions.assertTrue(value.isSystemError());
    Assertions.assertFalse(value.isValidationError());
  }

  @Test
  public void validateResponseAndSaveValidationErrorErrorTest() {
    productRecatStatusServiceBean.validateResponseAndSave(productRecatStatus, RecatConstants.PRODUCT_NAME);
    Mockito.verify(productRecatStatusRepository).save(productRecatStatusArgumentCaptor.capture());
    ProductRecatStatus value = productRecatStatusArgumentCaptor.getValue();
    Assertions.assertEquals(RecatConstants.FAILED, value.getStatus());
    Assertions.assertEquals(RecatConstants.PRODUCT_NAME, value.getErrorMessage());
    Assertions.assertFalse(value.isSystemError());
    Assertions.assertTrue(value.isValidationError());
  }

  @Test
  public void validateResponseAndSaveValidationErrorTest() {
    productRecatStatusServiceBean.validateResponseAndSave(productRecatStatus, ERROR_MESSAGE);
    Mockito.verify(productRecatStatusRepository).save(productRecatStatusArgumentCaptor.capture());
    ProductRecatStatus value = productRecatStatusArgumentCaptor.getValue();
    Assertions.assertEquals(RecatConstants.FAILED, value.getStatus());
    Assertions.assertEquals(ERROR_MESSAGE.replaceAll(RecatConstants.VALIDATION_ERROR_MESSAGE, StringUtils.EMPTY),
        value.getErrorMessage());
    Assertions.assertFalse(value.isSystemError());
    Assertions.assertTrue(value.isValidationError());
  }

  @Test
  public void findProductRecatStatusCountByStoreIdAndStatusAndRecatRequestCodeTest() {
    productRecatStatusServiceBean
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            RECAT_REQUEST_CODE);
    Mockito.verify(productRecatStatusRepository)
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            RECAT_REQUEST_CODE);
  }

  @Test
  public void findByStoreIdAndStatusAndRecatRequestCodeTest() {
    productRecatStatusServiceBean
        .findByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PUBLISHED, RECAT_REQUEST_CODE);
    Mockito.verify(productRecatStatusRepository)
        .findByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PUBLISHED, RECAT_REQUEST_CODE);
  }

  @Test
  public void getStatusCountByStoreIdAndRecatRequestCountTest() {
    Mockito.when(productRecatStatusRepository.findStatusCountByStoreIdAndRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(statusCountList);
    Map<String, Integer> statusCountMap =
        productRecatStatusServiceBean.getStatusCountByStoreIdAndRecatRequestCount(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(productRecatStatusRepository)
        .findStatusCountByStoreIdAndRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Assertions.assertEquals(5, statusCountMap.get(RecatConstants.PENDING), 0);
    Assertions.assertEquals(10, statusCountMap.get(RecatConstants.FINISHED), 0);
    Assertions.assertEquals(100, statusCountMap.get(RecatConstants.FAILED), 0);
    Assertions.assertEquals(4, statusCountMap.get(RecatConstants.VALIDATION_ERROR), 0);
    Assertions.assertEquals(6, statusCountMap.get(RecatConstants.SYSTEM_ERROR), 0);
  }
}